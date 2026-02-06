{-|
This module serves as the main entry point for the Kaleidoscope compiler project.
Kaleidoscope is a minimal toy language that compiles to LLVM intermediate representation.
This application supports both:
  1. Interactive REPL mode - for testing and ad-hoc compilation
  2. File compilation mode - for compiling Kaleidoscope source files

Example usage:
  Interactive: @hascope@
  File mode:  @hascope filename.ks@
-}

module Main where

import Parser
import Codegen
import Emit

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST

-- | Initialize an empty LLVM module with the name \"jit\"
-- The module serves as a container for the generated functions and external declarations
initModule :: AST.Module
initModule = emptyModule "jit"

-- | Process a Kaleidoscope source string by parsing it to an AST and generating LLVM code
-- 
-- Takes a LLVM module and source code string, parses the source, and generates
-- LLVM IR for the parsed expressions. Returns the updated module or Nothing if parsing fails.
--
-- Arguments:
--   * @modo@ - The current LLVM Module state
--   * @source@ - The Kaleidoscope source code string
--
-- Returns: @Just updatedModule@ if successful, @Nothing@ if parsing fails
process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

-- | Process and compile a Kaleidoscope source file
-- 
-- Reads the entire file contents and processes it through the compiler pipeline.
-- 
-- Arguments:
--   * @fname@ - Path to the Kaleidoscope source file
--
-- Returns: The compiled LLVM module or Nothing if compilation fails
processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

-- | Interactive REPL (Read-Eval-Print Loop)
--
-- Provides a command-line interface for interactively entering and compiling Kaleidoscope code.
-- The REPL maintains the LLVM module state across multiple inputs, allowing function definitions
-- to persist throughout the session. Each command is terminated with a semicolon (;).
--
-- Example:
--   @> def add(x y) x + y;@
--   @> add(3, 4);@
repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Exiting REPL."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

-- | Main entry point
-- Determines whether to launch the interactive REPL or compile a file based on command-line arguments.
-- Usage:
--   * No arguments: launches interactive REPL
--   * One argument: compiles the specified file
main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
