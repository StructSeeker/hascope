{-# LANGUAGE OverloadedStrings #-}

{-| This module is the semantic analyzer and code generator that translates Kaleidoscope
Abstract Syntax Trees (AST) into LLVM intermediate representation (IR).

The module performs Top-level compilation via 'codegenTop' and expression code generation via 'cgen':

Example:
  @Function \"factorial\" [\"n\"] ...@  becomes a full LLVM function definition
  with entry block, parameter setup, and proper code generation for the body.
-}

module Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT
import qualified Syntax as S

-- | Constant 1.0 
one = cons $ C.Float (F.Double 1.0)

-- | Constant 0.0 
zero = cons $ C.Float (F.Double 0.0)

-- | Alias for 'zero' (false value for conditionals)
false = zero

-- | Alias for 'one' (true value for conditionals)
true = one



-- | Convert a list of parameter names to LLVM function parameter types
-- In Kaleidoscope, all parameters are floating-point numbers (doubles).
-- This helper creates the (Type, Name) pairs needed for LLVM function definitions.
-- Arguments:
--   * @names@ - List of parameter name strings
--
-- Returns: List of (double type, Name) pairs suitable for 'define' or 'external'
toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))


-- | Compile a top-level Kaleidoscope item to LLVM
--   Function definitions: creates an LLVM function with parameters and body
--   External declarations: creates an LLVM external function declaration
--   Standalone expressions: wraps the expression in a "main" function
-- Arguments:
--   * @expr@ - The Kaleidoscope AST expression to compile
codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

------------------------------------------------------------------------------------
-- Binary Operations
------------------------------------------------------------------------------------

-- | Less-than comparison operation
-- 
-- Compares two floating-point values and returns 1.0 if a < b, else 0.0.
-- ULT is unsigned less-than (appropriate for floating-point comparisons).
--
-- Arguments:
--   * @a@ - Left operand
--   * @b@ - Right operand
--
-- Returns: 1.0 or 0.0 representing the comparison result
lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

-- | Map of binary operator names to their code generation functions
-- 
-- Defines all supported binary operators in Kaleidoscope:
--   * \"+\" : Addition
--   * \"-\" : Subtraction
--   * \"*\" : Multiplication
--   * \"/\" : Division
--   * \"<\" : Less-than comparison
binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]


-- Code Generation for Expressions

-- | Generate LLVM code for a Kaleidoscope expression
cgen :: S.Expr -> Codegen AST.Operand
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"

-- | Generate code for variable reference
-- Loads the variable value from its allocated stack location.
cgen (S.Var x) = getvar x >>= load

-- | Generate code for floating-point literal
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)

-- | Generate code for function call
-- Evaluates all arguments and generates a call instruction.
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

-- | Generate code for if-then-else expression
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]

-- | Generate code for for loop expression
cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  ------------------
  i <- alloca double
  istart <- cgen start           -- Gen loop variable initial value
  stepval <- cgen step           -- Gen loop variable step

  store i istart                 -- Store the loop variable initial value
  assign ivar i                  -- Assign loop variable to the variable name
  br forloop                     -- Branch to the loop body block

  -- for.loop
  ------------------
  setBlock forloop
  cgen body                      -- Generate the loop body
  ival <- load i                 -- Load the current loop iteration
  inext <- fadd ival stepval     -- Increment loop variable
  store i inext

  cond <- cgen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forloop forexit       -- Generate the loop condition

  -- for.exit
  ------------------
  setBlock forexit
  return zero


-- Module Compilation
-- | Takes a LLVM module and a list of top-level expressions (functions and declarations),
-- generates LLVM IR, sends it to the JIT compiler for optimization and execution,
-- and returns the compiled module.
--
-- Arguments:
--   * @mod@ - The current LLVM module to add definitions to
--   * @fns@ - List of top-level Kaleidoscope expressions
--
-- Returns: Updated LLVM module with new definitions
codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res  of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = mapM codegenTop fns
    oldast  = runLLVM mod modn
