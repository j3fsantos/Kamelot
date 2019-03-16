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

expr = buildExpressionParser eOperators eTerm
         <?> "expression"

eOperators = 
  [
    [  
      Infix (reservedOp "*" >> return (BinOp MultI (invInfo MultI))) AssocLeft, 
      Infix (reservedOp "/" >> return (BinOp DivI  (invInfo DivI)))  AssocLeft
    ],
    [
      Infix (reservedOp "*" >> return (BinOp MultI (invInfo MultI))) AssocLeft, 
      Infix (reservedOp "/" >> return (BinOp DivI  (invInfo DivI)))  AssocLeft
    ]
  ]

eTerm =  parens expr
     <|> variable
     <|> lVariable
     <|> literal
