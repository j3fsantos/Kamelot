{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Entailment.Unifier where 
  import Syntax.Asrt

  class Monad m => Unifier m b | m -> b where
    bottom :: b -> m b
    choose :: m a -> m a -> m a 