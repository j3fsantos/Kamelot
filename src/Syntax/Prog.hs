module Syntax.Prog where 

import qualified Data.Map as Map 

import Syntax.Var 
import Syntax.Pred 
import Syntax.ProcId 
import Syntax.Proc 
import Syntax.PName

type Predecessors = Map.Map (ProcId, Int, Int) Int 

data Prog annot a = Prog { 
  preds :: Map.Map PName (Pred a),
  procs :: Map.Map ProcId (Proc annot a),
  --  
  predecessors :: Predecessors 
}

getProc :: Prog annot a -> ProcId -> Maybe (Proc annot a) 
getProc p fid = Map.lookup fid (procs p)
  