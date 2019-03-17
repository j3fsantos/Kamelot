module Entailment.USAsrt where 

import Entailment.UExpr 
import Syntax.Expr 
import Syntax.Formula 
import Syntax.PName

data USAsrt a = 
    UGAsrt a [ Expr ] [ UExpr ] 
  | UPure Formula  
  | UPred PName [ Expr ] [ UExpr ]
  | UDefEq Expr UExpr 