module Syntax.Parsing.LVar where 

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language

import Syntax.Parsing.Tokenizer
import Syntax.LVar

lVariable :: Parser LVar
lVariable = char '#' >> identifier >>= \id -> return $ '#':id 