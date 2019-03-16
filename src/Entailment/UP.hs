module Entailment.UP where 

import Syntax.Pred
import Syntax.Asrt
--
import Entailment.Unifier

data UP a b = 
    Leaf a b 
  | Branch a [ UP a b ]

data UPPred a b = UPPred { 
  pred :: Pred a,
  up   :: UP (SAsrt a) b   
}

up_unify :: (Unifier u b) => ((UP a b -> u b) -> a -> u c) -> (() -> b) -> (UP a b) -> u b 
up_unify uf bf (Leaf a b) = (uf' a) >> (return b) 
  where uf' = uf (up_unify uf bf)

up_unify uf bf (Branch a ups) = 
  let us = map (up_unify uf bf) ups in 
  (uf' a) >> (foldl choose (bottom (bf ())) us)  
  where uf' = uf (up_unify uf bf)