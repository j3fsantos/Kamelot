module Syntax.Parsing.Formula where 

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax.Formula 
import Syntax.Expr


data FormulaParseInfo = FormulaParseInfo { 
  e_binops  :: [ (String, Expr -> Expr -> Formula) ], 
  f_binops  :: [ (String, Formula -> Formula -> Formula) ], 
  e_unops   :: [ (String, Expr -> Formula) ], 
  f_unops   :: [ (String, Formula -> Formula) ], 
  zero_ops  :: [ (String, Formula) ] 
}

pStr :: String -> Parser String 
pStr = Text.ParserCombinators.Parsec.Char.string

pBinOpF :: (String, (Formula -> Formula -> Formula)) -> Parser Formula 
pBinOpF (s, c) = pFormula >>= \f1 -> pStr s >> pFormula >>= \f2 -> return (c f1 f2) 

pBinOpE :: (String, (Expr -> Expr -> Formula)) -> Parser Formula 
pBinOpE (s, c) = pExpr >>= \e1 -> pStr s >> pExpr >>= \e2 -> return (c e1 e2) 

pUnOpF  :: (String, Formula -> Formula) -> Parser Formula
pUnOpF (s, c)  = pStr s >> pFormula >>= \f -> return (c f)   

pUnOpE  :: (String, Expr -> Formula) -> Parser Formula
pUnOpE (s, c)  = pStr s >> pExpr >>= \e -> return (c e) 

pZeroOp :: (String, Formula) -> Parser Formula 
pZeroOp (s, f) = pStr s >> return f

pFormula :: Parser Formula 
pFormula = error "bananas!!!"

pExpr    :: Parser Expr 
pExpr    = error "bananas!!!"



{-
pFormula :: FormulaParseInfo -> Parser Formula 
pFormula p_info = 
  let p_expr_binops = map pBinOpE (e_binops p_info) in 
  let p_frml_binops = map pBinOpF (f_binops p_info) in 
  let p_expr_unops  = map pUnOpE  (e_unops  p_info) in 
  let p_frml_unops  = map pUnOpF  (f_unops  p_info) in 
  let p_zerops      = map pZeroOp (zero_ops p_info) in 
-}