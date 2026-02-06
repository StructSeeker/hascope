{-|
This module defines the Abstract Syntax Tree (AST) types for the Kaleidoscope language.
The Kaleidoscope language is a minimalist toy language with support for:
-}

module Syntax where

-- | A type alias for function/variable names
type Name = String

-- | AST for Kaleidoscope expressions
data Expr
  -- | Integer literal: @123@
  = Int Integer
  
  -- | Floating-point literal: @3.14@
  | Float Double
  
  -- | Variable reference: @x@, @myVar@
  | Var String
  
  -- | Function call: @add(x, y)@, @sqrt(16)@
  | Call Name [Expr]
  
  -- | Function definition: @def multiply(a b) a * b@
  | Function Name [Name] Expr
  
  -- | External function declaration: @extern sin(x)@
  -- Used to declare functions from external libraries (like libm)
  | Extern Name [Name]
  
  -- | Binary operation: @x + y@, @a < b@, @p * q@
  | BinaryOp Name Expr Expr
  
  -- | Unary operation: (future extension for operations like negation)
  | UnaryOp Name Expr
  
  -- | Conditional expression: @if x < 5 then x else 10@
  | If Expr Expr Expr
  
  -- | Loop expression: @for i = 1, i < 100, 1.0 in i * 2@
  -- Arguments: loop variable name, start value, condition, step value, body
  -- Executes body repeatedly while condition is true, incrementing loop variable by step each iteration
  | For Name Expr Expr Expr Expr
  
  -- | Variable binding expression: @let x = 5 in x + 1@
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)
