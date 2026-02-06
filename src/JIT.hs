{-|
This module handles the JIT compilation and immediate execution of LLVM intermediate
representation (IR). JIT compilation allows the Kaleidoscope compiler to execute
programs without producing standalone executables - the code is compiled to machine
code and executed in-process.

Example flow:
  LLVM AST -> Module Creation -> Optimization -> Code Generation -> Execution Engine ->
  Function Lookup -> FFI Call -> Result
-}

module JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE

-- | FFI binding to call Haskell-wrapped C function pointers
foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

-- | Execute a compiled Kaleidoscope function
--   * @fn@ - Function pointer to call (function take no arguments and
-- return a Double) 
-- Returns: The result of executing the function
run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

-- | Initialize MCJIT execution engine with specified options
-- Creates a MCJIT (Modular Code Generator and JIT) execution engine with
-- the given LLVM context and configuration parameters. MCJIT is a modern
-- JIT compiler that supports generating and linking multiple modules.
jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

-- | Optimization pass configuration
-- Level 3 provides good optimization without excessive compilation time.
passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

-- | JIT compile and execute an LLVM module
-- Takes an LLVM AST module and:
--   Creates an LLVM context and execution engine
--   Converts the AST to a LLVM module
--   Applies optimization passes
--   Prints the generated LLVM assembly for inspection
--   Looks up and executes the \"main\" function
--   Returns the compiled module (for potential reuse)
--
-- Arguments:
--   * @mod@ - The LLVM AST module to compile and execute
--
-- Returns: @Right compiledModule@ if successful, @Left errorMessage@ if error occurs
runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          {-runPassManager pm m-}
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()

          -- Return the optimized module
          return optmod
