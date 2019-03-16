module Syntax.Pred where

import Syntax.Var 
import Syntax.Asrt 

type PredId = Int 

type PredDefAnnot = (PredId, [ Var ])

data Pred a = Pred { 
  name :: String, 
  ins  :: [ Var ], 
  outs :: [ Var ], 
  defs :: [ (Maybe PredDefAnnot, Asrt a) ]    
}