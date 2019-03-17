module Syntax.Pred where

import Syntax.Var 
import Syntax.Asrt 
import Syntax.PName 

type PredId = Int 

type PredDefAnnot = (PredId, [ Var ])

data Pred a = Pred { 
  name :: PName, 
  ins  :: [ Var ], 
  outs :: [ Var ], 
  defs :: [ (Maybe PredDefAnnot, Asrt a) ]    
}