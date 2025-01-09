module Data.Traversable.Dependent

import public Data.Foldable.Dependent


public export
interface (DepFoldable i t) => DepTraversable (0 i : Type) (0 t : (i -> Type) -> Type) | t where
  dtraverse : Applicative f => {0 u : i -> Type} -> {0 v : i -> Type} -> ((x : i) -> u x -> f (v x)) -> t u -> f (t v)


export
{0 i : Type} -> DepTraversable i (DPair i) where
  dtraverse k (x ** y) = (MkDPair x) <$> k x y
