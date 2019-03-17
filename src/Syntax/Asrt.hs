{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Syntax.Asrt where
  import Syntax.Expr 
  import Syntax.Formula
  import Syntax.TType
  import Syntax.Var
  import Syntax.PName

  data SAsrt a = 
      GAsrt a [ Expr ] [ Expr ] 
    | Pure Formula  
    | Pred PName [ Expr ] [ Expr ]

  data Asrt a = SAsrt (SAsrt a) | Star (Asrt a) (Asrt a) | Emp | Types [ (Var, TType) ]