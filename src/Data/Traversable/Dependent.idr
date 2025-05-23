||| A dependently-typed traversable class
module Data.Traversable.Dependent

import Control.Applicative.Const
import public Data.Foldable.Dependent
import Data.SortedMap.Dependent


public export
interface (DepFoldable i t) => DepTraversable (0 i : Type) (0 t : (i -> Type) -> Type) | t where
  dtraverse : Applicative f => {0 u : i -> Type} -> {0 v : i -> Type} -> ((x : i) -> u x -> f (v x)) -> t u -> f (t v)


export
{0 i : Type} -> DepTraversable i (DPair i) where
  dtraverse k (x ** y) = (MkDPair x) <$> k x y

%inline
impl : {0 u,v : i -> Type} -> ((x : i) -> u x -> v x) -> {x : i} -> u x -> v x
impl f {x} = f x

export
DepTraversable k (SortedDMap k) where
  dtraverse f = traverse (impl f)

||| Any traversable is automatically foldable
depTraversableFoldMap : (DepTraversable i t, Monoid m) => ((x : i) -> v x -> m) -> t v -> m
depTraversableFoldMap f = let
  g : (x : i) -> v x -> Const m (v x)
  g x = MkConst . f x
  in runConst . dtraverse g
