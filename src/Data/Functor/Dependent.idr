||| Functors with type depending on some extra data, the "index"
|||
||| They are expected to obey the laws
|||   imap g . imap f = imap (\i => g i . f i)
||| and
|||   imap (\i => id {t i}) = id
module Data.Functor.Dependent

import Data.SortedMap.Dependent


public export
interface DepFunctor (0 i : Type) (0 t : (i -> Type) -> Type) | t where
  dmap : {0 u : i -> Type} -> {0 v : i -> Type} -> ((x : i) -> u x -> v x) -> t u -> t v

export
{0 i : Type} -> DepFunctor i (DPair i) where
  dmap f (x ** y) = (x ** f x y)

DepFunctor k (SortedDMap k) where
  dmap = map
