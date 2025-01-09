module Data.Functor.Dependent


public export
interface DepFunctor (0 i : Type) (0 t : (i -> Type) -> Type) | t where
  dmap : {0 u : i -> Type} -> {0 v : i -> Type} -> ((x : i) -> u x -> v x) -> t u -> t v

export
{0 i : Type} -> DepFunctor i (DPair i) where
  dmap f (x ** y) = (x ** f x y)

