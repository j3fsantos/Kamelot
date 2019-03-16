module Entailment.Result where 
  import Control.Applicative 
  import Control.Monad (liftM, ap)

  data Result a = 
      Ok [ a ]
    | Error [ String ]

  both :: Result a -> Result a -> Result a 
  both r1 r2 = 
    case (r1, r2) of 
      (Ok xs1, Ok xs2)       -> Ok (xs1 ++ xs2)
      (Ok _, Error ys)       -> Error ys 
      (Error ys, Ok _)       -> Error ys
      (Error ys1, Error ys2) -> Error (ys1 ++ ys2) 

  either :: (s -> Result a) -> (s -> Result a) -> s -> Result a 
  either f1 f2 =
    \s -> 
      case f1 s of 
        Ok xs -> Ok xs 
        Error ys -> 
          case f2 s of 
            Ok xs -> Ok xs 
            Error ys' -> Error (ys ++ ys')

  r_all :: [ Result a ] -> Result a 
  r_all rets = foldl both (Ok []) rets 

  bottom :: a -> Result a 
  bottom a = Error [] 

  either_to_result :: Either String a -> Result a 
  either_to_result (Left s) = Error [ s ]
  either_to_result (Right a) = Ok [ a ] 

  maybe_to_result  :: Maybe a -> Result a 
  maybe_to_result Nothing  = Error [ ]
  maybe_to_result (Just a) = Ok [ a ]

  instance Functor Result where
    fmap = liftM

  instance Applicative Result where
    pure  = return
    (<*>) = ap

  instance Monad Result where 
    return = Ok . (:[])

    m >>= k = 
      case m of 
        Ok xs -> r_all (map k xs) 
        Error ys -> Error ys 


