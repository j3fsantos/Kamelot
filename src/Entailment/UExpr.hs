module Entailment.UExpr where 

import Syntax.Var 
import Syntax.LVar
import Syntax.Op
import Syntax.Literal
import Syntax.Expr

data UExpr = 
    ULVar  LVar 
  | UVar   Var  
  | ULit   Literal 
  | UUnOp  UnOp (UInvInfo Expr) UExpr 
  | UBinOp BinOp (BInvInfo Expr) UExpr UExpr 
  | UNOp   NOp (NInvInfo Expr) [ UExpr ] 
  deriving (Show)

uexpr_to_expr :: UExpr -> Expr 
uexpr_to_expr (ULVar x)           = LVar x 
uexpr_to_expr (UVar x)            = Var x 
uexpr_to_expr (ULit l)            = Lit l 
uexpr_to_expr (UUnOp op _ e)      = UnOp op (uexpr_to_expr e)
uexpr_to_expr (UBinOp op _ e1 e2) = BinOp op (uexpr_to_expr e1) (uexpr_to_expr e2)
uexpr_to_expr (UNOp op _ es)      = NOp op (map uexpr_to_expr es)