module Data.Indexed.AddDep

-- Make a collection into an dependently indexed collection by storing
-- dependent pairs.

import Data.Witherable
import Data.Witherable.Dependent


public export
record DepIndexed (t : Type -> Type) (i : Type) (a : i -> Type) where
  constructor AddDepIndex
  unDepIndex : t (DPair i a)


export
(Functor t) => DepFunctor i (DepIndexed t i) where
  dmap f (AddDepIndex p) = AddDepIndex (dmap f <$> p)

export
(Foldable t) => DepFoldable i (DepIndexed t i) where
  dfoldl f z (AddDepIndex p) = foldl (\u, (MkDPair x y) => f u x y) z p
  dfoldr f z (AddDepIndex p) = foldr (\(MkDPair x y), u => f x y u) z p

export
(Traversable t) => DepTraversable i (DepIndexed t i) where
  dtraverse f (AddDepIndex p) = AddDepIndex <$> traverse (dtraverse f) p

export
(Filterable t) => DepFilterable i (DepIndexed t i) where
  dmapMaybe f (AddDepIndex p) = AddDepIndex $ mapMaybe (dtraverse f) p

export
[depIndexedWitherable] (Witherable t) => DepWitherable i (DepIndexed t i) where

