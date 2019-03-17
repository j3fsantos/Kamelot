module Syntax.Formula where
  import Syntax.Expr 
  import Syntax.Var
  import Syntax.TType

  type FBindings = [ (Var, TType) ] 
  type FTriggers = [ Expr ]

  data Formula = 
      And Formula Formula 
    | Or  Formula Formula 
    | Not Formula 
    --
    | Eq      Expr Expr 
    -- 
    | ILess   Expr Expr  
    | ILessEq Expr Expr  
    -- 
    | StrLess Expr Expr  
    -- 
    | SetMem  Expr Expr 
    | SetSub  Expr Expr  
    -- 
    | ForAll FBindings FTriggers Formula  
    -- 
    | FTrue 
    | FFalse 
