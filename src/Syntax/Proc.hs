module Syntax.Proc where 

import Data.Array.IArray
import Syntax.Var 
import Syntax.Cmd 

data Proc annot a = Proc { 
  name   :: String, 
  body   :: Array Int (annot, Cmd a), 
  params :: [ Var ]  
}