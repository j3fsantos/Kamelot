module Syntax.Formula where
  import Syntax.Expr 
  import Syntax.LVar
  import Syntax.TType

  type FBindings = [ (LVar, TType) ] 
  type FTriggers = [ Expr ]

  data Formula = 
      And Formula Formula 
    | Or  Formula Formula 
    | Not Formula 
    --
    | Eq        Expr Expr 
    -- 
    | ILess     Expr Expr  
    | ILessEq   Expr Expr  
    -- 
    | StrLess   Expr Expr  
    | StrLessEq Expr Expr 
    -- 
    | SetMem    Expr Expr 
    | SetSub    Expr Expr  
    -- 
    | ForAll FBindings FTriggers Formula  
    -- 
    | FTrue 
    | FFalse 
    deriving (Show)