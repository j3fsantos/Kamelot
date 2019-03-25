module Syntax.Parsing.TType where 

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language

import Syntax.Parsing.Tokenizer
import Syntax.TType 

pType :: Parser TType 
pType = 
        (r "$real" RealT)
    <|> (r "$int"  IntT)
    <|> (r "$str"  StrT)
    <|> (r "$symb" SymbT) 
    <|> (r "$bool" BoolT)
    <|> (r "$type" TypeT) 
    <|> (r "$list" ListT) 
    <|> (r "$set"  SetT) 
    where r s t = reserved s >> return t
