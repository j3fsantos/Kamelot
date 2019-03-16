module Syntax.Parsing.Var where 

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language

import Syntax.Parsing.Tokenizer
import Syntax.Var

variable :: Parser Var
variable = identifier >>= \id -> return $ id 