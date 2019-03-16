module Syntax.Literal where 

import Syntax.TType 

data Literal = 
	Bool Bool 
  |	Int Int 
  | Real Float 
  | String String 
  | Symbol String 
  | Type TType 
  | List [ Literal ]
  | Set [ Literal ]
  deriving (Eq, Show)

lit_type :: Literal -> TType 
lit_type (Bool _)   = BoolT 
lit_type (Int _)    = IntT  
lit_type (Real _)   = RealT 
lit_type (String _) = StrT 
lit_type (Symbol _) = SymbT
lit_type (Type _)   = TypeT 
lit_type (List _)   = ListT 
lit_type (Set _)    = SetT 

