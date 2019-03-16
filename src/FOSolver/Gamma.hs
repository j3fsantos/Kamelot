module FOSolver.Gamma where 
import Syntax.Var
import Syntax.TType 

class TypeEnv tenv where
  get_type :: tenv -> Var -> Maybe TType  
  set_type :: tenv -> Var -> TType -> tenv 
  