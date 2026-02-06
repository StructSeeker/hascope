{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Codegen
Description : LLVM code generation and IR builder for Kaleidoscope compiler
Copyright   : (c) 2025
License     : GPL-2

This module provides the low-level infrastructure for generating LLVM intermediate representation (IR)
from Kaleidoscope AST nodes. It abstracts away LLVM IR complexities and provides a cleaner monadic interface
for building functions, basic blocks, and instructions.

Key components:

1. __Module-level operations__: 

2. __Type system__: Defines the IEEE 754 double-precision floating-point type used throughout
   the Kaleidoscope language.

3. __Name management__: 

4. __Codegen monad__: A stateful computation monad that tracks:
   - Current block being generated
   - All blocks in the current function
   - Symbol table (variable bindings)
   - Instruction counter for unique naming
   - Name supply

5. __Instruction builders__: High-level functions to emit LLVM instructions (arithmetic, memory,
   control flow) that automatically track placement in blocks and generate unique names.

The module provides a clean API for building LLVM IR without exposing the full complexity of
the LLVM AST. It's designed to be used by the Emit module, which translates Kaleidoscope
AST to Codegen operations.

Implementation notes:
- All numbers in Kaleidoscope are represented as IEEE 754 doubles
- Blocks are tracked in a map and assembled in sorted order at the end
- Instructions within blocks are accumulated on a stack and reversed when finalizing
- The symbol table uses a simple list (variable shadowing is allowed by prepending)
-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

-- Module Level Operations
-- Functions to build and manage LLVM modules, which serve as
-- containers for all functions, external declarations, and metadata.

-- | The LLVM code generation monad for building entire modules
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

-- | Execute an LLVM code generation computation with an initial module
runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

-- | Create an empty LLVM module with a given name
emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

-- | Add a definition (function or external declaration) to the current module
addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- | Define a function with a body
define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

-- | Declare an external function (no implementation provided)
external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

-- Types

double :: Type
double = FloatingPointType 64 IEEE

-- Name Management
-- Provides a supply of unique names to ensure all generated entities (variables,
--   blocks) have distinct identifiers.

-- | Map of known names to their occurrence count
type Names = Map.Map String Int

-- | Generate a unique name based on a desired name
uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)


--  Codegen State

-- | Symbol table mapping variable names to their LLVM values
type SymbolTable = [(String, Operand)]

-- | State maintained during code generation of a single function
data CodegenState
  = CodegenState {
    currentBlock :: Name                     --  Name of the active block
  , blocks       :: Map.Map Name BlockState  -- All blocks in current function
  , symtab       :: SymbolTable              -- Variable bindings in scope
  , blockCount   :: Int                      -- Counter for block numbering
  , count        :: Word                     -- Counter for instruction naming
  , names        :: Names                    -- Name supply for uniqueness
  } deriving Show

-- | State of a single basic block
data BlockState
  = BlockState {
    idx   :: Int                            -- Block index (for ordering)
  , stack :: [Named Instruction]            -- Instructions (in reverse order)
  , term  :: Maybe (Named Terminator)       -- Block terminator (br/ret)
  } deriving Show

-- Codegen Operations


-- | The code generation monad for building a single function
newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

-- | Sort blocks by their index to get the correct execution order
sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

-- | Convert the accumulated code generation state into LLVM BasicBlocks
createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

-- | Convert a block state into an LLVM BasicBlock
makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

-- | The name of the first (entry) block in each function
entryBlockName :: String
entryBlockName = "entry"

-- | Create an empty basic block with given index
emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

-- | Initial code generation state for a function
emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

-- | Execute code generation and return the final state
execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

-- | Generate a fresh unique identifier for unnamed instructions
fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

-- | Emit an LLVM instruction to the current block
instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ local ref

-- | Add a terminator to the current block
terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

------------------------------------------------------------------------------------
-- Basic Block Management
------------------------------------------------------------------------------------

-- | Get the name of the current block
-- 
-- Returns the name of the block currently being generated to.
entry :: Codegen Name
entry = gets currentBlock

-- | Create a new basic block with a given label
addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

-- | Switch code generation to a different block
setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

-- | Get the name of the current block (convenience variant of 'entry')
getBlock :: Codegen Name
getBlock = gets currentBlock

-- | Update the state of the current block
modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

-- | Get the current block's state
current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

--Symbol Table Operations

-- | Bind a variable name to an LLVM value
assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

-- | Look up a variable in the symbol table
-- 
-- Retrieves the LLVM value for a variable name. Returns an error
-- if the variable is not in scope.
--
-- Arguments:
--   * @var@ - The variable name to look up
--
-- Returns: The LLVM operand for the variable
getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

--Operand References and Constants

-- | Create an operand that references a local variable by name
-- Used to reference values that are in memory or have been computed.
-- The type is always double (the only numeric type in Kaleidoscope).
local ::  Name -> Operand
local = LocalReference double

-- | Create an operand that references a global value (function) by name
-- Used in code generation to create references to functions
-- that may be called.
global ::  Name -> C.Constant
global = C.GlobalReference double

-- | Create an operand that references an external function
-- 
-- Wraps a global reference in ConstantOperand for use in function calls.
externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

-- Arithmetic and Floating-Point Operations


-- | Emit a floating-point addition instruction
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []
 
fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

-- | Create an operand from a LLVM constant
cons :: C.Constant -> Operand
cons = ConstantOperand

-- | Convert an unsigned integer to floating-point
uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

-- | Convert operands to function call arguments
toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

--Effects and Memory Operations

-- | Emit a function call instruction
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

-- | Allocate space on the stack for a variable
alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

-- | Store a value to memory
store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

-- | Load a value from memory
load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []



-- Control Flow Operations

-- | Emit an unconditional branch to a block
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

-- | Emit a conditional branch to one of two blocks
cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

-- | Emit a phi node to merge values from multiple blocks
phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

-- | Emit a return instruction
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
