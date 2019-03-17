module FOSolver.Typing where 

--import qualified Prelude 
import Prelude hiding ((<=), (+))

import Syntax.Var
import FOSolver.Gamma

import Syntax.TType 
import Syntax.Op  
import Syntax.Expr
import Syntax.Literal

uop :: UnOp -> PType -> Maybe PType
uop op t =  
  let (t', rng) = uop_dom_rng op in 
  if (t <= t') then Just rng else Nothing 


bop :: BinOp -> PType -> PType -> Maybe PType 
bop op t1 t2 = 
  let ((t1', t2'), t_r) = bop_dom_rng op in 
  if ((t1 <= t1') && (t2 <= t2')) then Just t_r else Nothing 

nop :: NOp -> [ PType ] -> Maybe PType 
nop op ts = 
  let t = foldl (+) tbot ts in 
  let (t', rng) = nop_dom_rng op in 
  if (t <= t') then Just rng else Nothing 

type_expr :: (TypeEnv te) => te -> Expr -> Maybe PType  
type_expr te e = 
  case e of 
    LVar x            -> get_type' x 
    Var x             -> get_type' x
    Lit  l            -> lit_type' l  
    UnOp op e         -> fe e  >>= \t -> uop op t
    BinOp op e1 e2    -> fe e1 >>= \t1 -> fe e2 >>= \t2 -> bop op t1 t2   
    NOp op es         -> fes es >>= \ts -> nop op ts 
  where fe         = type_expr te
        fes        = sequence . map fe
        get_type'  = (fmap ptype) . (get_type te) 
        lit_type'  = Just . ptype . lit_type 
 

rev_type_expr :: PType -> Expr -> Maybe [ (Expr, PType) ]  
rev_type_expr t e =
  case e of 
    LVar x  -> Just [ (e, t) ] 
    Var x   -> Just [ (e, t) ]
    
    Lit  l  -> if (ptype (lit_type l) <= t) then Just [] else Nothing 

    UnOp op e -> 
      let (t', t_r) = uop_dom_rng op in 
        fe t' e >>= \cnstrs -> 
          if ((t_r <= t) || (t <= t_r)) 
            then if (t_r <= t) then return cnstrs else return ((e, t):cnstrs) 
            else Nothing 
    
    BinOp op e1 e2 -> 
      let ((t1, t2), t_r) = bop_dom_rng op in       
      fe t1 e1 >>= \cnstrs1 -> fe t2 e2 >>= \cnstrs2 -> 
        let cnstrs = cnstrs1 ++ cnstrs2 in 
        if ((t_r <= t) || (t <= t_r)) 
            then if (t_r <= t) then return cnstrs else return ((e, t):cnstrs) 
            else Nothing 
    
    NOp op es -> 
      let (t', t_r) = nop_dom_rng op in
      fmap concat (sequence (map (fe t') es)) >>= \cnstrs -> 
        if ((t_r <= t) || (t <= t_r))    
          then if (t_r <= t) then return cnstrs else return ((e, t):cnstrs) 
          else Nothing 

  where fe            = rev_type_expr 
        fes           = sequence . map fe