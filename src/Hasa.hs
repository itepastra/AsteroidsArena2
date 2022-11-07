{-# LANGUAGE MultiParamTypeClasses #-}


module Hasa where
import GHC.Base (join)

class HasA a b where
  getA :: b -> a
  setA :: a -> b -> b

updateA :: HasA a b => (a -> a) -> b -> b
updateA f a = setA (f (getA a)) a


(#) :: HasA a b => (a -> a) -> b -> b
(#) = updateA

infixr #