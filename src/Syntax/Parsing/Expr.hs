{-# LANGUAGE FlexibleContexts #-}

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

pExpr = try (buildExpressionParser eOperators eTerm <?> "expression")

pExprList = (sepBy pExpr ((string ";") >> whiteSpace)) >>= \es -> eof >> return es 

eOperators = 
  [
    [  
      Infix (reservedOp "*" >> return (BinOp MultI)) AssocLeft, 
      Infix (reservedOp "/" >> return (BinOp DivI))  AssocLeft
    ],
    [
      Infix (reservedOp "+" >> return (BinOp PlusI)) AssocLeft, 
      Infix (reservedOp "-" >> return (BinOp MinusI))  AssocLeft
    ], 
    [ 
      Infix (reservedOp "=" >> return (BinOp EEq)) AssocLeft
    ]
  ]

eTerm =  (parens pExpr) 
     <|> (variable   >>= \x -> return $ Var x)
     <|> (lVariable  >>= \x -> return $ LVar x)
     <|> (literal    >>= \l -> return $ Lit l)
     <|> (reserved "[" >> pExprs >>= \es -> reserved "]" >> return (NOp EList es))
     <|> (reserved "{" >> pExprs >>= \es -> reserved "}" >> return (NOp ESet es))
      -- 
  where pExprs = (sepBy pExpr ((reserved ",") >> whiteSpace))