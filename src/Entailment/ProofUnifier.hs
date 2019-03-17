{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}
module Entailment.ProofUnifier where 

import Control.Applicative 
import Control.Monad (liftM, ap)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans
import qualified Data.Map as Map 

import Syntax.Literal
import Syntax.Op
import Syntax.Expr 
import Syntax.Formula
import Syntax.Asrt
import Syntax.Pred
import Syntax.PName
--
import State.LState
-- 
import Entailment.UP 
import Entailment.Unifier
import Entailment.Result
import Entailment.USAsrt
import Entailment.UExpr

type ProofUnifier st b = StateT st Result

instance Unifier (ProofUnifier st b) b where 
  bottom b                         = StateT $ \s -> Entailment.Result.bottom (b, s) 
  (StateT u1) `choose` (StateT u2) = StateT $ Entailment.Result.either u1 u2

type ExprUnifier st sub v = ReaderT (st, v) (StateT sub (ErrorT String Identity))

--------------------------
-- Expression Unification 
--------------------------

from_uexpr = uexpr_to_expr

expr_unify :: (Val v, LState st a, Subst sub) => UExpr  -> ExprUnifier (st v) (sub v) v [ (Expr, v) ] 
expr_unify (UVar x) = 
  ask >>= \(s, v) -> 
    lift (Control.Monad.State.get) >>= \sub -> 
      if (s_eq s (v_lookup s x) v) 
        then return []
        else throwError "undefined variable"
          
expr_unify (ULit l) =
  ask >>= \(s, v) -> 
    lift (Control.Monad.State.get) >>= \sub -> 
      if (s_eq s (from_lit l) v) 
        then return []  
        else throwError "unification failed" 

expr_unify (ULVar x) = 
  ask >>= \(s, v) -> 
    lift (Control.Monad.State.get) >>= \sub -> 
      case State.LState.get sub x of 
        Nothing -> put (set sub x v) >> return [] 
        Just v' -> 
          if (s_eq s v' v) 
            then return [] 
            else throwError "unification failed" 

expr_unify (UUnOp op ii e) = 
  case ii of 
    UInv ctx -> 
      ask >>= \(s, v) ->  
        let v' = eval s $ ctx (to_expr v) in 
        lift (runReaderT (expr_unify e) (s, v'))
    NUInv -> 
      ask >>= \(_, v) -> 
        return [ ((UnOp op (from_uexpr e)), v) ] 
         
expr_unify (UBinOp op ii e1 e2) = 
  let ue1 = expr_unify e1 in 
  let ue2 = expr_unify e2 in 

  case ii of 
    BFullyInv ctx1 ctx2 -> 
      ask >>= \(s, v) ->  
        let v1 = eval s $ ctx1 (to_expr v) in 
        let v2 = eval s $ ctx2 (to_expr v) in 
        (local (\(s, _) -> (s, v1)) ue1) >>
          (local (\(s, _) -> (s, v2)) ue2)

    LeftInv ctx ->
      ask >>= \(s, v) -> 
        lift (Control.Monad.State.get) >>= \sub -> 
          let v' = (apply sub True (from_uexpr e1)) >>= \e1' -> return (eval s $ ctx (to_expr v) e1') in
          case v' of 
            Nothing -> lift (throwError "fatal")
            Just v' -> local (\(s, _) -> (s, v')) ue2

    RightInv ctx ->
      ask >>= \(s, v) -> 
        lift (Control.Monad.State.get) >>= \sub ->  
          let v' = (apply sub True (from_uexpr e2)) >>= \e2' -> return (eval s $ ctx (to_expr v) e2') in
          case v' of 
            Nothing -> lift (throwError "fatal")
            Just v' -> local (\(s, _) -> (s, v')) ue1   

    NBInv -> ask >>= \(s, v) -> return [ (BinOp op (from_uexpr e1) (from_uexpr e2), v) ]

    _ -> error "DEATH. expr_unify. binop"

expr_unify (UNOp op ii es) = 
  case ii of
    NFullyInv ctx -> 
      ask >>= \(s, v) -> 
          let e_v  = to_expr v in 
          let vs   = [ eval s $ ctx e_v (Lit (Int x)) | x <- [0..(length es - 1)]] in 
          let ues  = map (\(e, v) -> local (\(s, _) -> (s, v)) (expr_unify e)) (zip es vs) in
          sequence ues >>= \ds -> return (concat ds) 
          --
    NNInv -> ask >>= \(_, v) -> return [ (NOp op (map from_uexpr es), v) ]  

expr_unify_lst :: (Val v, LState st a, Subst sub) => [ UExpr ] -> ExprUnifier (st v) (sub v) [ v ] [ (Expr, v) ] 
expr_unify_lst es = 
  ReaderT $ \(s, vs) -> 
    let ues  = map (\e -> let (ReaderT ue) = expr_unify e in ue) es in 
    let ues' = map (\(ue, v) -> ue (s, v)) (zip ues vs) in
    foldl (\ue1 ue2 -> ue1 >>= \d1 -> ue2 >>= \d2 -> return (d1 ++ d2)) (return []) ues'
  
    --ask >>= \(s, vs) -> 
    --  let evs  = zip es vs in 
    --  let ues  = map (\(e, v) -> local (\(s, _) -> (s, v)) (expr_unify e)) evs in
    --  withReader (\(s, _) -> (s, vs)) (sequence ues >>= \ds -> return (concat ds))
     

     
run_expr_unify :: ExprUnifier st sub v a -> st -> v -> sub -> Result (a, sub) 
run_expr_unify eu st v sub = either_to_result (runIdentity (runErrorT (runStateT (runReaderT eu (st, v)) sub)))

---------------------------
-- Assertion Unification --
---------------------------

type LeafAnnotation = Int 

type FProofUnifier st sub v = ProofUnifier (st v, sub v) LeafAnnotation

type PredUPTable a = Map.Map PName (UPPred a LeafAnnotation)

type UPUnificationFunction st sub v a = UP (USAsrt a) LeafAnnotation -> FProofUnifier st sub v LeafAnnotation

run_proof_unifier :: FProofUnifier st sub v a -> st v -> sub v -> Result (a, (st v, sub v)) 
run_proof_unifier pu st sub = (runStateT pu (st, sub)) 

eval_es :: (Val v, LState st a, Subst sub) => st v -> sub v -> [ Expr ] -> Result [ v ]
eval_es s sub ins = 
  let f e ac = ac >>= \ac' -> apply sub True e >>= \e' -> return (e':ac') in 
  let ins'   = foldr f (Just []) ins in
  maybe_to_result (fmap (fmap (eval s)) ins')

unify_asrt :: (Val v, LState st a, Subst sub) => 
  PredUPTable a -> UPUnificationFunction st sub v a -> USAsrt a -> FProofUnifier st sub v ()
-- General Assertion 
unify_asrt p_tbl u_up (UGAsrt a ins outs) = 
  Control.Monad.State.get >>= \(s, sub) ->
    let eu_outs = expr_unify_lst outs in
    let v_ins   = eval_es s sub ins in
    lift v_ins >>= \v_ins' ->
      lift (s_get s a v_ins') >>= \(s', v_outs) ->
        lift (run_expr_unify eu_outs s' v_outs sub) >>= \(ds, sub') ->
          modify (\_ -> (s', sub')) -- I need to so sth about the discharges
  
-- Predicate Assertion   
unify_asrt p_tbl u_up (UPred pn ins outs) =
  Control.Monad.State.get >>= \(s, sub) -> 
    let eu_outs = expr_unify_lst outs in 
    lift (eval_es s sub ins) >>= \v_ins -> 
      case get_pred s pn v_ins of 
        Error _ -> 
          -- Fold 
          let pred = Map.lookup pn p_tbl in 
          lift (maybe_to_result pred) >>= \pred -> 
            let sub_p = State.LState.init (zip ((Syntax.Pred.ins . Entailment.UP.pred) pred) v_ins) in 
            lift (run_proof_unifier ((u_up . up) pred) s sub_p) >>= \(_, (s', sub_p')) -> 
              let outs   = (Syntax.Pred.outs . Entailment.UP.pred) pred in
              let e_outs = foldr (\x ac -> ac >>= \ac -> 
                            State.LState.get sub_p' x >>= \v -> 
                              return ((State.LState.to_expr v):ac)) (Just []) outs in 
              lift (maybe_to_result e_outs) >>= \e_outs ->
                lift (eval_es s' sub e_outs) >>= \v_outs -> 
                  lift (run_expr_unify eu_outs s' v_outs sub) >>= \(ds, sub') ->
                    modify (\_ -> (s', sub')) -- I need to so sth about the discharges

        x -> 
          -- No folding is needed
          lift x >>= \(s', v_outs) -> 
            lift (run_expr_unify eu_outs s' v_outs sub) >>= \(ds, sub') ->
              modify (\_ -> (s', sub')) -- I need to so sth about the discharges

-- Conjunction 
unify_asrt p_tbl u_up (UPure (And f1 f2)) = 
  unify_asrt p_tbl u_up (UPure f1) >> unify_asrt p_tbl u_up (UPure f2)

-- Defining Equality 
unify_asrt p_tbl u_up (UDefEq e1 e2) = 
  Control.Monad.State.get >>= \(s, sub) ->
    lift (maybe_to_result (apply sub False e1)) >>= \e1' -> 
      let v1 = eval s e1' in 
      let eu2 = expr_unify e2 in 
      lift (run_expr_unify eu2 s v1 sub) >>= \(ds, sub') -> 
        modify (\_ -> (s, sub'))

-- Pure formula 
unify_asrt p_tbl u_up (UPure f) = 
   Control.Monad.State.get >>= \(s, sub) -> error "bananas"



