module Syntax.Parsing.Literal where 

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language

import Syntax.Parsing.Tokenizer
import Syntax.Literal 

pBool :: Parser Literal 
pBool = 
  (string "true"  >> return (Bool True))
  <|> 
  (string "false" >> return (Bool False))

pInt :: Parser Literal 
pInt = integer >>= \i -> return $ Int (fromIntegral i) 

pReal :: Parser Literal 
pReal = real >>= \r -> return $ Real r 

pString :: Parser Literal  
pString = char '"' >> (many $ noneOf "\"") >>= \s -> char '"' >> return (String s)  

pSymbol :: Parser Literal
pSymbol = char '@' >> (many $ noneOf "\"") >>= \s -> return (Symbol s)

literal = pBool <|> pInt <|> pReal <|> pString <|> pSymbol 