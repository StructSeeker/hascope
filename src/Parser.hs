{-|
This module implements a top-down recursive descent parser for the Kaleidoscope language.
The parser converts a stream of tokens (produced by the Lexer) into an Abstract Syntax Tree (AST).
-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

-- | Parse an integer literal, converting it to a floating-point
int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

-- | Binary operator parser with configurable associativity
-- Creates an infix operator parser for Parsec's built-in expression parser.
-- Arguments:
--   * @s@ - The operator symbol/name (e.g., \"+\", \"*\")
--   * @assoc@ - The associativity of the operator (AssocLeft or AssocRight)
binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

-- | Operator precedence table for the expression parser
binops = [[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft]]

-- | Parse an arbitrary expression with operator precedence
-- 
-- Uses Parsec's expression parser to handle binary operators with the precedence
-- and associativity defined in 'binops'. This is the main entry point for parsing
-- expressions that may contain operators.
expr :: Parser Expr
expr =  Ex.buildExpressionParser binops factor

-- | Parse a variable reference (identifier)
variable :: Parser Expr
variable = Var <$> identifier

-- | Parse a function definition
-- Syntax: @def <name> (<params>) <body>@
function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body



-- | Parse an external function declaration
-- Syntax: @extern <name> (<params>)@
extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

-- | Parse a function call
-- Syntax: @<name> (<args>)@
-- Example: @sqrt(16)@ or @add(2, 3)@
call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

-- | Parse a conditional (if-then-else) expression
-- Syntax: @if <cond> then <true> else <false>@
-- Example: @if x < 5 then 0 else 1@
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

-- | Parse a for loop expression
-- Syntax: @for <var> = <start>, <cond>, <step> in <body>@
-- Example: @for i = 1, i < 100, 1.0 in i * 2@
for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

-- | Parse a expression (or factor in parser terminology)
factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> ifthen
      <|> for
      <|> (parens expr)

-- | Parse top-level definitions or expressions
-- 
-- Tries to parse, in order:
--   1. External function declarations (@extern ...@)
--   2. Function definitions (@def ...@)
--   3. Standalone expressions
defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

-- | Parse a complete program (consumes leading whitespace and expects EOF)
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

-- | Parse a sequence of top-level definitions separated by semicolons
toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

-- | Parse a single expression from a string
parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

-- | Parse a complete Kaleidoscope program (top-level definitions) from a string
parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
