module Syntax.Parsing.Formula where 

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax.Formula 
import Syntax.Expr
import Syntax.TType

import Syntax.Parsing.Tokenizer
import Syntax.Parsing.Var 
import Syntax.Parsing.LVar 
import Syntax.Parsing.Literal
import Syntax.Parsing.Expr 
import Syntax.Parsing.TType

pForm :: Parser Formula 
pForm = try (buildExpressionParser fOperators fTerm <?> "formula")

pFormList :: Parser [ Formula ]
pFormList = (sepBy pForm ((string ";") >> whiteSpace)) >>= \fs -> eof >> return fs 

fOperators = 
  [
    [ Prefix (reservedOp "not" >> return Not)           ],
    [ Infix  (reservedOp "and" >> return And) AssocLeft ],
    [ Infix  (reservedOp "or"  >> return Or) AssocLeft  ] 
  ]

fTerm =  (parens pForm) 
     <|> (reserved "true"   >> return FTrue)
     <|> (reserved "false"  >> return FFalse)
     <|> pBinExprForm 
     <|> (reserved "forall" >> pBinders >>= \bndrs -> 
           pTriggers >>= \es -> pForm >>= \f -> 
             return (ForAll bndrs es f))

pRel :: Parser (Expr -> Expr -> Formula) 
pRel = (f "=="  Eq)
   <|> (f "<"   ILess) 
   <|> (f "<="  ILessEq)
   <|> (f ">"   (f_comp ILessEq Not))
   <|> (f ">="  (f_comp ILess Not))
   <|> (f "<s"  StrLess)
   <|> (f "<=s" StrLessEq)
   <|> (f "mem" SetMem)
   <|> (f "sub" SetSub)
   --
   where f_comp f g = \e1 e2 -> g (f e1 e2)
         f x op = (reservedOp x) >> return op 

pBinExprForm :: Parser Formula 
pBinExprForm = 
  pExpr >>= \e1 -> whiteSpace >> 
    pRel >>= \rel -> whiteSpace >> 
      pExpr >>= \e2 -> return (rel e1 e2) 

pBinders :: Parser FBindings 
pBinders = 
  reserved "{" >> f >>= \bnds -> reserved "}" >> return bnds 
  where 
    f = (lVariable >>= \x -> reserved ":" >> pType >>= \t -> return (x, t))
        `sepBy`
        (reserved ",")

pTriggers :: Parser FTriggers 
pTriggers = 
  reserved "[" >> f >>= \es -> reserved "]" >> return es 
  where f = pExpr `sepBy` (reserved ", ")