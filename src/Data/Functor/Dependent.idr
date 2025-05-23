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

%inline
impl : {0 u,v : i -> Type} -> ((x : i) -> u x -> v x) -> {x : i} -> u x -> v x
impl f {x} = f x

DepFunctor k (SortedDMap k) where
  dmap f = Dependent.map $ impl f
