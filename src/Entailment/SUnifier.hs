{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Entailment.SUnifier where 

import Control.Applicative 
import Control.Monad (liftM, ap)
import Control.Monad.State
--
import Entailment.Unifier
import Entailment.Result
--
import Syntax.Literal 

type Var = String 

data Expr = 
    PVar Var
  | OK 
  | Fail 
  | IS_OK Expr 
  | Lit Literal  
  deriving (Show)

data Term a = 
    Skip 
  | Seq (Term a) (Term a) 
  | If (Expr) (Term a) (Term a) 
  | While (Expr) (Term a) 
  | Action Var a [ Expr ]
  | Asgn Var Expr  
  deriving (Show) 

data SynthUnifier a st b c = SU (st -> (Term a, Term a, Var, st, c))

class StaticState st where 
  freshVar :: st -> (st, Var)

instance (StaticState st) => Functor (SynthUnifier a st b) where
  fmap = liftM

instance (StaticState st) => Applicative (SynthUnifier a st b) where
  pure  = return
  (<*>) = ap

instance (StaticState st) => Monad (SynthUnifier a st b) where 
  return b = 
    SU  
      (\s -> 
        let (s', x) = freshVar s in
        let t = Asgn x OK in
        (t, Skip, x, s', b))
  
  (SU u) >>= k = 
    SU
      (\s -> 
        let (t, t_undo, x, s', c) = u s in 
        let (SU u') = k c in  
        let (t', t_undo', y, s'', c') = u' s' in 
        let (s''', z) = freshVar s'' in
        let t'' = 
                  Seq t
                      (If (IS_OK $ PVar x) 
                          (Seq t' (Asgn z (PVar y)))
                          (Asgn z (PVar x))) in
            (t'', Skip, z, s''', c'))
        

instance (StaticState st) => Unifier (SynthUnifier a st b) b where 
  bottom b =
      SU  
        (\s -> 
          let (s', x) = freshVar s in
          let t = Asgn x Fail in
          (t, Skip, x, s', b))

  (SU u1) `choose` (SU u2) = 
    SU 
      (\s -> 
        let (t1, undo1, x1, s1, b1) = u1 s in 
        let (t2, undo2, x2, s2, b2) = u2 s2 in 
        let (s', x) = freshVar s2 in 
        let t = 
                Seq t1
                  (If (IS_OK $ PVar x1) 
                      (Asgn x (PVar x1))
                      (Seq t2 (Asgn x (PVar x2)))) in  
        (t, Skip, x, s', b2))
