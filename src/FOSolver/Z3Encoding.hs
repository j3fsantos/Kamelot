{-# LANGUAGE FlexibleContexts #-}

module FOSolver.Z3Encoding where 

import Z3.Monad
import Control.Applicative 
import Control.Monad (liftM, ap)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Data.Map as Map 

import Syntax.Op
import Syntax.Literal 
import Syntax.Expr
import Syntax.Formula
import Syntax.TType
import Syntax.Var

data DataCons = DataCons { 
  constr  :: FuncDecl, 
  rec     :: FuncDecl, 
  sels    :: [ FuncDecl ]
}

data TValCons = TValCons { 
  tsort   :: Sort, 
  tbool   :: DataCons, 
  tint    :: DataCons,
  treal   :: DataCons,
  tstr    :: DataCons,
  tsymb   :: DataCons,
  ttype   :: DataCons,
  tlist   :: DataCons,
  tset    :: DataCons
}

data BValCons = BValCons { 
  bvsort  :: Sort, 
  bool    :: DataCons, 
  int     :: DataCons, 
  real    :: DataCons, 
  str     :: DataCons, 
  symb    :: DataCons, 
  vtype   :: DataCons, 
  list    :: DataCons
}

data LValCons = LValCons { 
  lsort   :: Sort, 
  lnil    :: DataCons, 
  lcons   :: DataCons
}

data ValCons = ValCons { 
  vsort   :: Sort, 
  velem   :: DataCons, 
  vset    :: DataCons
}

data AllCons = AllCons { 
  t   :: TValCons, 
  bv  :: BValCons, 
  lv  :: LValCons, 
  v   :: ValCons 
}

encodeDataConstructor :: String -> String -> [ (String, Z3 (Maybe Sort), Int) ] -> Z3 Constructor 
encodeDataConstructor consName recName selectors = 
    mkStringSymbol consName >>= \c -> 
      mkStringSymbol recName >>= \r -> 
        (f selectors) >>= \sels -> mkConstructor c r sels
    where f sels =
                  sequence (
                    map (\(selName, sort, rec) -> 
                      mkStringSymbol selName >>= \s -> 
                        sort >>= \sort' -> return (s, sort', rec)) 
                    selectors)

getConsRecsSels :: Sort -> Z3 ([FuncDecl], [FuncDecl], [[FuncDecl]]) 
getConsRecsSels sort = 
  getDatatypeSortConstructors sort >>= \constrs' -> 
    getDatatypeSortRecognizers sort >>= \recgns' -> 
      getDatatypeSortConstructorAccessors sort >>= \sels' ->
        return (constrs', recgns', sels')

mkSortWrapper :: String -> [ Constructor ] -> Z3 (Sort, [ DataCons ])
mkSortWrapper tName constrs = 
  mkStringSymbol tName >>= \tName' ->    
    mkDatatype tName' constrs >>= \tSort -> 
      getConsRecsSels tSort >>= \(constrs', recgns', sels') -> 
        let func_decls = (zip (zip constrs' recgns') sels') in 
          return (tSort, map (\((c, r), sls) -> DataCons c r sls) func_decls)     

mkTwoSortWrapper :: (String, [ Constructor ]) -> (String, [ Constructor ]) -> Z3 ((Sort, [ DataCons ]), (Sort, [ DataCons ]))
mkTwoSortWrapper (t1Name, constrs1) (t2Name, constrs2) = 
  mkStringSymbol t1Name >>= \t1Name' -> 
    mkStringSymbol t2Name >>= \t2Name' -> 
      mkDatatypes [t1Name', t2Name'] [ constrs1, constrs2 ] >>= \[sort1, sort2] -> 
        getConsRecsSels sort1 >>= \(constrs1, recgns1, sels1) ->
          getConsRecsSels sort2 >>= \(constrs2, recgns2, sels2) -> 
            let fdecls1 = make_func_decls constrs1 recgns1 sels1 in 
            let fdecls2 = make_func_decls constrs2 recgns2 sels2 in
            return ((sort1, fdecls1), (sort2, fdecls2))

  where make_func_decls constrs recgns sels = map (\((c, r), sls) -> DataCons c r sls) (zip (zip constrs recgns) sels) 


mkTypeDatatype :: Z3 TValCons 
mkTypeDatatype = do 
  -- Type Value Constructors 
  btvC          <- cons "BoolT" "isBoolT"
  itvC          <- cons "IntT" "isIntT"  
  rtvC          <- cons "RealT" "isRealT"  
  stvC          <- cons "StrT" "isStrT"  
  sytvC         <- cons "SymbT" "isSymbT"  
  ttvC          <- cons "TypeT" "isTypeT" 
  ltvC          <- cons "ListT" "isListT"  
  stvC          <- cons "SetT" "isSetT"  
  --
  (btS, fdecls) <- mkSortWrapper "Type" [ btvC, itvC, rtvC, stvC, sytvC, ttvC, ltvC, stvC ] 
  case fdecls of 
    [ b, i, r, s, sy, t, l, set ] -> return (TValCons btS b i r s sy t l set)
    _ -> error "DEATH. mkTypeDatatype"
  where cons c r = encodeDataConstructor c r [ ]

mkBaseValDatatype :: Sort -> Z3 (BValCons, LValCons)
mkBaseValDatatype typeSort = do
  --
  -- Base Data Constructors  
  boolConst   <- cons "Bool" "isBool" [ ("getBool", (j mkBoolSort),   0) ] 
  intConst    <- cons "Int"  "isInt"  [ ("getInt",  (j mkIntSort),    0) ] 
  realConst   <- cons "Real" "isReal" [ ("getReal", (j mkRealSort),   0) ] 
  strConst    <- cons "Str"  "isStr"  [ ("getStr",  (j mkIntSort),    0) ]       
  symbConst   <- cons "Symb" "isSymb" [ ("getSymb", (j mkIntSort),    0) ]  
  listConst   <- cons "List" "isList" [ ("getList", (return Nothing), 1) ]
  typeConst   <- cons "Type" "isType" [ ("getType", (return (Just typeSort)), 0) ] 
  --
  -- List Value Constructors 
  nilConst    <- cons "Nil" "isNil" [ ]
  consConst   <- cons "Cons" "isCons" [ ("head", (return Nothing), 1), ("tail", (return Nothing), 1) ]
  --
  ((bvS, bvFdecls), (lvS, lvFdecls)) <- mkTwoSortWrapper ("BVal", [ boolConst, intConst, realConst, strConst, symbConst, listConst, typeConst ]) ("ListVal", [ nilConst, consConst ])
  case (bvFdecls, lvFdecls) of 
    ([ b, i, r, s, sy, l, t ], [ hd, tl ]) -> return ((BValCons bvS b i r s sy t l), (LValCons lvS hd tl))
    _ -> error "DEATH. mkBaseValDatatype"
  where cons = encodeDataConstructor
        j m = m >>= \v -> return (Just v) 

mkValDatatype :: Sort -> Z3 ValCons
mkValDatatype bvalDataType = do
  --
  elemConst    <- cons "Elem" "isCons" [ ("getElem", return (Just bvalDataType), 0) ]  
  setConst     <- cons "Set" "isSet" [ ("getSet", j (mkSetSort bvalDataType), 0) ]
  (vS, fdecls) <- mkSortWrapper "Val" [ elemConst, setConst ] 
  case fdecls of 
    [ e, s ] -> return (ValCons vS e s)
    _ -> error "DEATH. mkTypeDatatype" 
  where cons = encodeDataConstructor
        j m = m >>= \v -> return (Just v) 
-- 

class EncodingState s where 
  encode_string :: s -> String -> (s, Int)

type EncodingM st = ReaderT AllCons (StateT st Z3)

z3_ttype_fdecl :: TType -> AllCons -> DataCons
z3_ttype_fdecl BoolT = tbool . t 
z3_ttype_fdecl IntT  = tint  . t 
z3_ttype_fdecl RealT = treal . t 
z3_ttype_fdecl StrT  = tstr  . t 
z3_ttype_fdecl SymbT = tsymb . t 
z3_ttype_fdecl TypeT = ttype . t 
z3_ttype_fdecl ListT = ttype . t 
z3_ttype_fdecl SetT  = tset  . t 

typeToConstrs :: AllCons -> TType -> [ DataCons ] 
typeToConstrs datatypes t = 
  case t of 
    BoolT -> [ vcons, bool  (bv datatypes) ]
    IntT  -> [ vcons, int   (bv datatypes) ]
    RealT -> [ vcons, real  (bv datatypes) ]
    StrT  -> [ vcons, str   (bv datatypes) ]
    SymbT -> [ vcons, symb  (bv datatypes) ]
    TypeT -> [ vcons, vtype (bv datatypes) ]
    ListT -> [ vcons, list  (bv datatypes) ]
    SetT  -> [ vset (v datatypes)          ]
  where vcons = velem (v datatypes)

-- up -> from the smallest to the biggest
z3_wrap_val :: (EncodingState st) => TType -> (DataCons -> FuncDecl) -> Bool -> AST -> EncodingM st AST 
z3_wrap_val t op up x = 
  ask >>= \datatypes -> 
    let constrs   = typeToConstrs datatypes t in 
    let constrs'  = if up then reverse constrs else constrs in
    let constrs'' = map op constrs' in  
      foldl 
        (\e_ac cons -> 
           e_ac >>= \e_ac' ->  lift (lift (mkApp cons [ e_ac' ])))
        (return x) constrs'' 

valCons :: (EncodingState st) => TType -> AST -> EncodingM st AST
valCons t = z3_wrap_val t constr True

valSel :: (EncodingState st) => TType -> AST -> EncodingM st AST
valSel t = z3_wrap_val t f_sel False 
  where f_sel x = (sels x)!!0 

valRec :: (EncodingState st) => TType -> AST -> EncodingM st AST
valRec t = z3_wrap_val t rec False  

encodeList :: AllCons -> [ AST ] -> Z3 AST
encodeList dataTypes elems = 
  let cons_cons = constr (lcons (lv dataTypes)) in
  let nil_cons  = constr (lnil (lv dataTypes)) in   
  foldr 
    (\elem list -> list >>= \list' -> mkApp cons_cons  [ elem, list' ]) 
    (mkApp nil_cons []) 
    elems 

encodeSet :: AllCons -> [ AST ] -> Z3 AST
encodeSet dataTypes elems = 
  foldl 
    (\set elem -> set >>= \set' -> mkSetAdd set' elem)
    (mkEmptySet (bvsort (bv dataTypes)))
    elems


--

encodeVar :: (EncodingState st) => String -> EncodingM st AST 
encodeVar x = 
  lift' (mkStringSymbol x) >>= \x' -> 
    ask >>= \datatypes -> 
      lift' (mkVar x' (vsort' datatypes))
  where lift' = lift . lift 
        vsort' datatypes = vsort (v datatypes)


encodeLit :: (EncodingState st) => Literal -> EncodingM st AST 
--
encodeLit l =
  case l of 
    Int i       -> 
      mkInteger' (toInteger i) >>= \i' -> 
        valCons IntT i' 
    
    Real r      ->   
      lift' (mkRealNum r) >>= \r' -> 
        valCons RealT r' 
   
    String str' ->  
      lift get >>= \s -> 
        let (s', i) = encode_string s str' in 
          lift (put s') >> 
            lift' (mkInteger (toInteger i)) >>= \i' -> 
              valCons StrT i' 

    Bool b      ->  
      lift' (mkBool b) >>= \b' -> 
        valCons BoolT b' 

    Symbol sy   ->   
      lift get >>= \s -> 
        let (s', i) = encode_string s sy in 
          put s' >> 
            mkInteger' (toInteger i) >>= \i' -> 
              valCons SymbT i' 
   
    Type t     -> 
      ask >>= \datatypes -> 
        lift get >>= \s -> 
          let t_cons = constr (z3_ttype_fdecl t datatypes) in 
            mkApp' t_cons [] >>= \t' -> 
              valCons TypeT t'        
    
    List lits   ->  
      let m_lits  = map encodeLit lits in 
      (sequence m_lits) >>= \lits' -> 
        ask >>= \datatypes -> 
          lift' (encodeList datatypes lits') >>= \list -> 
            valCons ListT list 

    Set lits   ->
      let m_lits  = map encodeLit lits in 
      (sequence m_lits) >>= \lits' ->
        ask >>= \datatypes -> 
          lift' (encodeSet datatypes lits') >>= \set -> 
            mkApp' (constr (vset (v datatypes))) [ set ]

    where lift'        = lift . lift 
          --
          mkApp' f vs  = lift' (mkApp f vs)
          --
          mkInteger'   = lift' . mkInteger


encodeUnop :: (EncodingState st) => UnOp -> AST -> EncodingM st AST 
encodeUnop op v = 
  case op of 
    NegI -> int_unop mkUnaryMinus
    where int_unop op = 
            valSel IntT v >>= \i -> 
              lift (lift (op i)) >>= \i' -> 
                valCons IntT i' 


encodeBinop :: (EncodingState st) => BinOp -> AST -> AST -> EncodingM st AST 
encodeBinop op v1 v2 =  
  case op of 
    PlusI  -> int_binop mkAdd
    
    MinusI -> int_binop mkSub

    MultI  -> int_binop mkMul

    DivI   -> int_binop (\[i1, i2] -> mkDiv i1 i2)

    where int_binop op = 
            valSel IntT v1 >>= \i1 -> 
              valSel IntT v2 >>= \i2 -> 
                lift (lift (op [i1, i2])) >>= \i -> 
                  valCons IntT i 


encodeNop :: (EncodingState st) => NOp -> [ AST ] -> EncodingM st AST 
encodeNop op vs = 
  case op of 
    EList ->
      ask >>= \datatypes ->  
        lift' (encodeList datatypes vs) >>= \vs' -> 
          valCons ListT vs' 
    
    ESet  -> 
      ask >>= \datatypes -> 
        lift' (encodeSet datatypes vs) >>= \set -> 
          lift' (mkApp (constr (vset (v datatypes))) [ set ])

    where lift' = lift . lift 


encodeExpr :: (EncodingState st) => Expr -> EncodingM st AST 
--
encodeExpr e =
  case e of
    Var _  -> error "DEATH. encodeExpr"

    LVar x -> encodeVar x 

    Lit l  -> encodeLit l 
    
    UnOp op e -> fe e >>= \v -> encodeUnop op v 
    
    BinOp op e1 e2 -> fe e1 >>= \v1 -> fe e2 >>= \v2 -> encodeBinop op v1 v2

    NOp op es -> sequence (map fe es) >>= \vs -> encodeNop op vs 

    where fe = encodeExpr


encodeGamma :: (EncodingState st) => [ (Var, TType) ] -> EncodingM st AST 
encodeGamma vts = 
  let ems = map (\(x, t) -> encodeVar x >>= \x' -> valRec t x') vts in 
    sequence ems >>= \fs -> lift (lift (mkAnd fs))
  

encodeFormula :: (EncodingState st) => Formula -> EncodingM st AST 
encodeFormula f =
  case f of 
    And f1 f2         -> ef2 f1 f2 >>= \(f1', f2') -> lift' (mkAnd [ f1', f2' ])
    Or  f1 f2         -> ef2 f1 f2 >>= \(f1', f2') -> lift' (mkOr  [ f1', f2' ])
    Not f             -> ef f  >>= \f'  -> lift' (mkNot f')
    -- 
    Eq e1 e2          -> ee2 e1 e2 >>= \(e1', e2') -> lift' (mkEq e1' e2')
    ILess e1 e2       -> ee2 e1 e2 >>= \(e1', e2') -> 
                          mk_isel e1' >>= \i1 -> mk_isel e2' >>= \i2 -> 
                            lift' (mkLt i1 i2)
    ILessEq e1 e2     -> ee2 e1 e2 >>= \(e1', e2') -> 
                          mk_isel e1' >>= \i1 -> mk_isel e2' >>= \i2 -> 
                            lift' (mkLe i1 i2)
    --
    StrLess e1 e2     -> error "DEATH. encodeFormula"
    -- 
    SetMem e1 e2      -> ee2 e1 e2 >>= \(e1', e2') -> 
                          mk_vsel velem e1' >>= \v1 -> mk_vsel vset e2' >>= \set -> 
                            lift' (mkSetMember v1 set)
    SetSub e1 e2      -> ee2 e1 e2 >>= \(e1', e2') -> 
                          mk_vsel vset e1' >>= \set1 -> mk_vsel vset e2' >>= \set2 -> 
                            lift' (mkSetSubset set1 set2)      
      --  
    FTrue             -> lift' mkTrue 
    FFalse            -> lift' mkFalse 
    --
   
    ForAll bnds pts f -> ef f >>= \fbody -> 
                          encodeGamma bnds >>= \fgamma -> 
                            implies fgamma fbody >>= \fbody' -> 
                              binders (map fst bnds) >>= \bnds' -> 
                                lift' (mkForallConst [] bnds' fbody')
    
  where ef            = encodeFormula 
        ee            = encodeExpr 
        lift'         = lift . lift 
        ef2 f1 f2     = ef f1 >>= \f1' -> ef f2 >>= \f2' -> return (f1', f2') 
        ee2 e1 e2     = ee e1 >>= \e1' -> ee e2 >>= \e2' -> return (e1', e2')
        mk_isel       = valSel IntT
        mk_vsel f x   = ask >>= \datatypes -> lift' (mkApp ((sels (f (v datatypes)))!!0) [ x ])
        implies f1 f2 = (lift' (mkNot f1)) >>= \f1' -> lift' (mkOr [ f1', f2])
        binders xs    = sequence (map encodeVar xs) >>= \xs' -> lift' (sequence (map toApp xs'))



