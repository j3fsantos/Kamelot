module State.CallStack where 

import Syntax.Var
import Syntax.ProcId

  {- 
    Type of call stacks: a call stack is a list of tuples, each of which contains
    1) identifier of the current procedure (string)
    2) arguments of the current procedure (list of values)
    3) calling store
    4) variable that should hold the return value
    5) index of the calling procedure from whence the procedure was called
    6) normal continuation index in the calling procedure
    7) optional error continuation index in the calling procedure
  -} 

type CallInfo sto v = (ProcId, [ v ], Maybe (sto v), Var, Int, Int, Maybe Int)

newtype CallStack sto v = CS [ (CallInfo sto v) ] 

csPush :: CallStack sto v -> CallInfo sto v -> CallStack sto v 
csPush (CS cs) ci = CS (ci:cs)