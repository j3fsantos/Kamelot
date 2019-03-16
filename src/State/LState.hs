{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}


module State.LState where 

import Syntax.Var
import Syntax.Literal 
import Syntax.Expr 
import Syntax.Asrt
-- 
import Entailment.Result

class Val v where 
  to_expr  :: v -> Expr 
  from_lit :: Literal -> v

class Subst sub where   
  set      :: sub v -> Var -> v -> sub v 
  get      :: sub v -> Var -> Maybe v 
  bindings :: sub v -> [ (Var, v) ]
  init     :: [ (Var, v) ] -> sub v 

class LState s a | s -> a where 
  v_lookup :: s v -> Var -> v 
  eval     :: s v -> Expr -> v 
  s_eq     :: s v -> v -> v -> Bool 
  s_get    :: s v -> a -> [ v ] -> Result (s v, [ v ])
  s_set    :: s v -> a -> [ v ] -> [ v ] -> Result (s v)
  get_pred :: s v -> PName -> [ v ] -> Result (s v, [ v ])

apply :: (Val v, Subst sub) => sub v -> Bool -> Expr -> Maybe Expr 
apply sub b e = 
  let f_sub e = 
                case e of 
                  LVar x ->
                    case get sub x of 
                      Just v  -> Just (to_expr v, False) 
                      Nothing -> if b then Just (LVar x, False) else Nothing 
                  _ -> Just (e, True) in 
  map_expr f_sub e
