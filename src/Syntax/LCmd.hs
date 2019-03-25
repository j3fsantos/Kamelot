module Syntax.LCmd where 

import Syntax.Expr
import Syntax.Formula
import Syntax.MacroName

data LCmd =
    If Expr [ LCmd ] [ LCmd ]  -- If-then-else
  | Branch Formula             -- Branching on a formula 
	| Macro MacroName [ Expr ]   -- Macro          
	| Assert Formula             -- Assert
  | Assume Formula             -- Assume 