module Syntax.Op where 

import Syntax.TType 

data UnOp   = NegI deriving (Show)
data BinOp  = PlusI | MinusI | MultI | DivI deriving (Show)
data NOp    = EList | ESet deriving (Show)

uop_dom_rng :: UnOp -> (PType, PType)
uop_dom_rng uop =
  case uop of 
    NegI -> (TType IntT, TType IntT) 

bop_dom_rng :: BinOp -> ((PType, PType), PType)
bop_dom_rng bop = 
  case bop of 
    PlusI  -> ((TType IntT, TType IntT), TType IntT)
    MinusI -> ((TType IntT, TType IntT), TType IntT)
    MultI  -> ((TType IntT, TType IntT), TType IntT)
    DivI   -> ((TType IntT, TType IntT), TType IntT) 

nop_dom_rng :: NOp -> (PType, PType)
nop_dom_rng nop = 
  case nop of 
    EList -> (TopT, TType ListT) 
    ESet  -> (TopT, TType ListT) 


data UInvInfo e = 
    UInv  (e -> e)
  | NUInv  

instance Show (UInvInfo e) where 
  show (UInv f) = "u_inv"
  show  NUInv   = "n_u_inv" 

data BInvInfo e = 
    BFullyInv    (e -> e) (e -> e)
  | LeftInv      (e -> e -> e)
  | RightInv     (e -> e -> e)
  | LeftRightInv (e -> e -> e) (e -> e -> e)
  | NBInv     

instance Show (BInvInfo e) where 
  show (BFullyInv f1 f2)    = "b_fully_inv"
  show (LeftInv f)          = "left_inv" 
  show (RightInv f)         = "right_inv" 
  show (LeftRightInv f1 f2) = "left_right_inv"
  show NBInv                = "n_b_inv"

data NInvInfo e = 
    NFullyInv (e -> e -> e)
  | NNInv     

instance Show (NInvInfo e) where 
  show (NFullyInv f) = "n_fully_inv"
  show NNInv         = "n_n_inv"
