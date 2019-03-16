
module Syntax.TType where 

import qualified Prelude 
import Prelude hiding ((<=))

data TType = 
    RealT | IntT  | StrT  | SymbT 
  | BoolT | TypeT | ListT | SetT
  deriving (Eq, Show)

data PType = TopT | BotT | TType TType  deriving (Eq, Show)

ptype :: TType -> PType 
ptype = TType 

tbot  :: PType 
tbot  = BotT

(<=) :: PType -> PType -> Bool 
t1 <= t2 = 
  case (t1, t2) of 
    (_, TopT) -> True
    (BotT, _) -> True 
    (_, _)    -> if (t1 /= t2) then False else True 

(+) :: PType -> PType -> PType 
t1 + t2 = 
  if (t1 <= t2) then t2 else 
    if (t2 <= t1) then t1 else TopT 
