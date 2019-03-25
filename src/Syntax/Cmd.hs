module Syntax.Cmd where 

import Syntax.Var
import Syntax.Expr
import Syntax.LCmd

data ActionType = Getter | Setter | Remover deriving (Show,Eq)

data Cmd a =
  -- basic commands 
    Skip                                  -- Skip                
  | Assignment Var Expr                   -- Variable assignment                               
  | Action Var a ActionType [ Expr ]      -- Action
  | Logic LCmd                            -- GIL logic commands
  -- control flow 
  | Goto  Int                             -- Unconditional goto 
  | GuardedGoto Expr Int Int              -- Conditional goto
  | Call Var Expr [ Expr ] (Maybe Int)    -- Procedure call 
  | ECall Var Expr [ Expr ] (Maybe Int)   -- External procedure call 
  | Apply Var Expr Expr (Maybe Int)       -- "Application-style" procedure call 
  | Arguments Var                         -- Arguments 
  | PhiAssignment [ (Var, [ Expr ]) ]     -- PHI assignment
  | ReturnNormal                          -- Normal return 
  | ReturnError                           -- Error return