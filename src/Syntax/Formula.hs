module Syntax.Formula where
  import Syntax.Expr 
  import Syntax.Var
  import Syntax.TType

  type FBindings  = [ (Var, TType) ] 
  type FTriggers  = [ Expr ]

  data Formula a = 
    And Formula Formula 
    | Or Formula Formula 
    | Not Formula 
    --
    | Eq      (Expr a) (Expr a)
    | DefEq   (Expr a) (Expr a)
    -- 
    | ILess   (Expr a) (Expr a) 
    | ILessEq (Expr a) (Expr a) 
    -- 
    | StrLess (Expr a) (Expr a) 
    -- 
    | SetMem  (Expr a) (Expr a) 
    | SetSub  (Expr a) (Expr a) 
    -- 
    | ForAll FBindings FTriggers Formula 
    -- 
    | FTrue 
    | FFalse 
