{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts #-}

module Syntax.Expr where 

import Syntax.Var 
import Syntax.LVar
import Syntax.Literal
import Syntax.Op 

data Expr = 
    LVar  LVar 
  | Var   Var  
  | Lit   Literal 
  | UnOp  UnOp Expr 
  | BinOp BinOp Expr Expr 
  | NOp   NOp [ Expr ] 
  deriving (Show)

map_expr :: (Expr -> Maybe (Expr, Bool)) -> Expr -> Maybe Expr 
map_expr f e = 
  let f_m   = map_expr f in
  case f e of 
    Nothing -> Nothing 
    Just (e', b) -> 
      if not b then Just e' else (
        case e' of 
          Var _  -> Just e'  
          LVar _ -> Just e'
          Lit _  -> Just e'  
          UnOp op e      -> (f_m e) >>= \e' -> return (UnOp op e') 
          BinOp op e1 e2 -> (f_m e1) >>= \e1' -> (f_m e2) >>= \e2' -> return (BinOp op e1' e2')
          NOp op es      -> 
            let es' = foldr (\e ac -> ac >>= \es' -> (f_m e) >>= \e' -> return (e' : es')) (Just []) es in 
            es' >>= \es' -> return (NOp op es'))


uInvInfo :: UnOp -> UInvInfo Expr  
uInvInfo NegI = UInv (\res -> UnOp NegI res)

bInvInfo :: BinOp -> BInvInfo Expr  
bInvInfo PlusI  = LeftRightInv (\res r -> BinOp MinusI res r) (\res l -> BinOp MinusI res l)
bInvInfo MinusI = LeftRightInv (\res r -> BinOp PlusI res r) (\res l -> BinOp MinusI l res)
bInvInfo MultI  = NBInv -- 0 issue 
bInvInfo DivI   = LeftRightInv (\res r -> BinOp MultI res r) (\res l -> BinOp DivI l res)

nInvInfo :: NOp -> NInvInfo Expr  
nInvInfo EList = error "bananas!!!"
nInvInfo ESet  = NNInv


