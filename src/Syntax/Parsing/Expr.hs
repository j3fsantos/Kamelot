module Syntax.Parsing.Expr where 

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax.Expr
import Syntax.Op

import Syntax.Parsing.Tokenizer
import Syntax.Parsing.Var 
import Syntax.Parsing.LVar 
import Syntax.Parsing.Literal

expr = buildExpressionParser eOperators eTerm
         <?> "expression"

eOperators = 
  [
    [  
      Infix (reservedOp "*" >> return (BinOp MultI)) AssocLeft, 
      Infix (reservedOp "/" >> return (BinOp DivI))  AssocLeft
    ],
    [
      Infix (reservedOp "*" >> return (BinOp MultI)) AssocLeft, 
      Infix (reservedOp "/" >> return (BinOp DivI))  AssocLeft
    ]
  ]

eTerm =  parens expr
     <|> (variable   >>= \x -> return $ Var x)
     <|> (lVariable  >>= \x -> return $ LVar x)
     <|> (literal    >>= \l -> return $ Lit l)
