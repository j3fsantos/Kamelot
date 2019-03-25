{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, AllowAmbiguousTypes #-}

module State.Interpreter where 

import Control.Applicative 
import Control.Monad (liftM, ap)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import qualified Data.Map as Map 

import Entailment.Result
import State.LState
import State.CallStack 
import Syntax.Cmd
import Syntax.LCmd
import Syntax.Expr 
import Syntax.Op
import Syntax.Var
import Syntax.ProcId 
import Syntax.Flag
import Syntax.Literal
import Syntax.Prog 
import Syntax.Proc

type GilComp st = StateT st Result

data Configuration annot a st sto v = Conf (Prog annot a) (st v) (CallStack sto v) Int Int 
 
data GilFinalResult st v = Finish Flag (st v) v

data GilResult v err = 
    Cont 
  | Ret (v, Flag) 
  | Err (err v)  

runLCmd :: (Val v, LState st sto a) => LCmd -> GilComp (st v) ()
runLCmd _ = error "bananas!"

runCmd :: (Val v, LState st sto a, Action a) => Cmd a -> GilComp (Configuration annot a st sto v) (GilResult v err) --(GilResult st v)
runCmd cmd = 
  case cmd of 
    --
    Skip -> return Cont
    --
    Assignment x e -> 
      modify 
        (\(Conf p s cs _ j) -> 
          let v  = eval s e in 
          let s' = v_set s x v in 
          Conf p s' cs j (j+1)) >> return Cont 
    -- 
    Action x a atype es -> 
      Control.Monad.State.get >>= \(Conf p s cs _ j) -> 
        let vs = eval_es s es in 
        case atype of 
          Getter -> 
            lift (s_get s a vs) >>= \(s', outs) -> 
              let s'' = v_set s' x (v_list s' outs) in  
              put (Conf p s'' cs j (j+1)) >> return Cont  
          Setter  -> 
            let (ins, outs) = getInsOuts a vs in    
            lift (s_set s a ins outs) >>= \s' -> 
              put (Conf p s' cs j (j+1))  >> return Cont 
          Remover ->
            lift (s_remove s a vs) >>= \s' ->
              put (Conf p s' cs j (j+1))  >> return Cont 
    --
    Logic lcmd -> 
      Control.Monad.State.get >>= \(Conf p s cs i j) -> 
        lift (runGComp (runLCmd lcmd) s) >>= \(_, s') -> 
          put (Conf p s' cs j (j+1)) >> return Cont  
    -- 
    Goto i -> 
      Control.Monad.State.get >>= \(Conf p s cs j k) -> 
          put (Conf p s cs k i) >> return Cont     
    -- 
    GuardedGoto e i1 i2 -> 
      Control.Monad.State.get >>= \conf ->
        let s  = confState conf in 
        let vt = eval s e in
        let vf = eval s (UnOp Not e) in  
        let ss = (assumeGuard conf vt i1) ++ (assumeGuard conf vf i2) in 
        --
        StateT $ \_ -> Ok (map (\conf -> (Cont, conf)) ss)
    -- 
    ReturnNormal -> 
      Control.Monad.State.get >>= \(Conf p s (CS cs) j k) -> 
        let v = v_lookup s ret in 
        case cs of
          (_, _, Just sto, x, prev, i, _):cs' ->  
            let s'   = v_set s x v in 
            let conf = Conf p s' (CS cs') prev i in  
            StateT $ \_ -> Ok [ (Cont, conf) ]
          [ (_, _, Nothing, x, _, _, _) ] -> return (Ret (v, Normal))
    -- 
    ReturnError -> 
      Control.Monad.State.get >>= \(Conf p s (CS cs) j k) -> 
        let v = v_lookup s ret in 
        case cs of
          (_, _, Just sto, x, prev, _, Just i):cs' ->  
            let s'   = v_set s x v in 
            let conf = Conf p s' (CS cs') prev i in  
            StateT $ \_ -> Ok [ (Cont, conf) ]
          [ (_, _, Nothing, x, _, _, _) ] -> return (Ret (v, Syntax.Flag.Error))
    -- 
    Call x ef es oi -> Control.Monad.State.get >>= run_call x ef es oi
    --
    Apply x ef es oi -> 
      Control.Monad.State.get >>= \(Conf p s cs _ j) ->


    --
    where eval_es s       = map (eval s)    
          v_list s vs     = eval s (NOp EList (map to_expr vs))
          runGComp gc     = runStateT gc
          confState conf  = let (Conf p s _ _ _) = conf in s 
          assumeGuard (Conf p s cs j k) v i      = map (\s' -> Conf p s' cs k i) (assume s v)   
          completeAux vs i def = if (i == 0) then vs else completeAux (def:vs) (i-1) def
          completeLst vs i def = reverse (completeAux (reverse vs) i def)
          --
          run_call x ef vs oi =
            \(Conf p s cs _ j) -> 
              let f'  = eval s ef in 
              case to_lit f' of 
                Just (String f) ->  
                  case getProc p f of 
                    Just proc -> 
                      let xs    = params proc in 
                      let vs'   = completeLst vs ((length xs)-(length vs)) val_def in 
                      let sto   = get_store s in 
                      let s'    = set_store s (zip xs vs') in  
                      let cs'   = csPush cs (f, vs', Just sto, x, j, j+1, oi) in 
                      let conf' = Conf p s' cs' 0 0 in  
                      StateT $ \_ -> Ok [ (Cont, conf') ]
                    _ -> lift (Entailment.Result.Error [ "Call error: fid " ++ f ++ " does not exist" ])
                -- 
                _ -> lift (Entailment.Result.Error [ "Call error: could not resolve fid" ]) 



{-
  | Apply Var Expr Expr (Maybe Int)       -- "Application-style" procedure call 
  | Arguments Var                         -- Arguments 
  | PhiAssignment [ (Var, [ Expr ]) ]     -- PHI assignment
  -}