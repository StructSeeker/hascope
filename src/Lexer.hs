{-|
This module provides the lexical analyzer (lexer) for the Kaleidoscope language.
The lexer converts a stream of characters into a stream of meaningful tokens,
handling:
-}

module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Tok

-- | The Parsec TokenParser configured for Kaleidoscope language
-- 
-- This tokenizer is configured with:
--   * Comment style: lines starting with #
--   * Reserved operators: +, -, *, /, <, =, ;, ,
--   * Reserved names (keywords): def, extern, if, then, else, in, for
-- 
-- The TokenParser provides high-level combinators for parsing tokens while
-- automatically handling whitespace and comments.
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    -- | Operators that cannot be used as identifiers
    ops = ["+","*","-","/",";",",","<"]
    
    -- | Keywords that cannot be used as identifiers
    names = ["def","extern","if","then","else","in","for"]
    
    -- | Language definition for Kaleidoscope
    style = emptyDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

-- | Parser for whole number integer literals
integer    = Tok.integer lexer

-- | Parser for decimal floating-point literals
float      = Tok.float lexer

-- | Parser for expressions enclosed in parentheses
parens     = Tok.parens lexer

-- | Parser for comma-separated list of items (the commas are consumed)
commaSep   = Tok.commaSep lexer

-- | Parser for semicolon-separated list of items
semiSep    = Tok.semiSep lexer

-- | Parser for identifiers (function/variable names)
-- 
-- Recognizes valid identifiers that don't conflict with keywords.
-- An identifier starts with a letter and can contain letters, digits, and underscores.
identifier = Tok.identifier lexer

-- | Parser for whitespace and comments
-- 
-- Skips any amount of whitespace and comments (lines starting with #).
-- This is typically called implicitly by other token parsers.
whitespace = Tok.whiteSpace lexer

-- | Parser for reserved keywords
-- 
-- Matches a keyword by name. Fails if the keyword is followed by an identifier character.
reserved   = Tok.reserved lexer

-- | Parser for reserved operators
-- 
-- Matches an operator by name. Fails if the operator syntax continues beyond the matched text.
reservedOp = Tok.reservedOp lexer
