module Data.Indexed.FixedDep

-- Regard a functor as dependently indexed, with specified fixed index.

import Data.Witherable
import Data.Witherable.Dependent


public export
record FixDepIndex {0 i : Type} (x : i) (f : Type -> Type) (a : i -> Type) where
  constructor WithDepIndex
  unIndex : f (a x)


export
{x : i} -> Functor f => DepFunctor i (FixDepIndex x f) where
  dmap f (WithDepIndex m) = WithDepIndex $ map (f x) m

export
{x : i} -> Foldable f => DepFoldable i (FixDepIndex x f) where
  dfoldl f z (WithDepIndex m) = foldl (\i => f i x) z m
  dfoldr f z (WithDepIndex m) = foldr (f x) z m

export
{x : i} -> Traversable f => DepTraversable i (FixDepIndex x f) where
  dtraverse f (WithDepIndex m) = WithDepIndex <$> traverse (f x) m

export
{x : i} -> Filterable f => DepFilterable i (FixDepIndex x f) where
  dmapMaybe f (WithDepIndex m) = WithDepIndex $ mapMaybe (f x) m

export
{x : i} -> Witherable f => DepWitherable i (FixDepIndex x f) where
  dwither f (WithDepIndex m) = WithDepIndex <$> wither (f x) m
