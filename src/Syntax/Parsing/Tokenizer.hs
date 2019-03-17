module Syntax.Parsing.Tokenizer where 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

gilReservedNames = 
  [ 
    -- PL Constructs 
    "skip", ":=", "goto", "return", "throw", "proc",  
    -- Boolean Literals
    "false", "true", 
    -- Type Literals 
    "$real", "$int", "$str", "$symb", "$bool", "$type", "$list", "$set", 
    -- Logic Constructs
    "requires", "ensures", "assert", "predicate", 
    -- Separators
    "(", ")", "{", "}", "]", "[" 
  ]

gilReservedOpNames = 
  [ 
    -- arithmetic 
    "+", "-", "*", "/", 
    -- lists 
    "cons", "hd", "tl",
    -- (in)equalities 
    "=", "!=", "<", ">", ">=", "<=", 
    -- logic 
    "and", "or", "not" 
    -- 
  ]

gilLangDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.caseSensitive   = True
           , Token.reservedNames   = gilReservedNames
           , Token.reservedOpNames = gilReservedOpNames
           }

gilLexer   :: Token.TokenParser ()
gilLexer   = Token.makeTokenParser gilLangDef

identifier :: Parser String -- parses an identifier
identifier = Token.identifier gilLexer 

integer    :: Parser Integer
integer    = Token.integer gilLexer 

real       :: Parser Float 
real       = Token.float gilLexer >>= \d -> return $ realToFrac d 

reserved   :: String -> Parser () 
reserved   = Token.reserved gilLexer 

reservedOp :: String -> Parser () 
reservedOp = Token.reservedOp gilLexer 

parens     :: Parser a -> Parser a 
parens     = Token.parens gilLexer

