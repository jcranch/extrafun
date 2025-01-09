module Data.Indexed.Fixed

-- Regarding a functor as indexed, with specified fixed index.

import Data.Witherable.Indexed


public export
record FixIndex {0 i : Type} (x : i) (f : Type -> Type) (a : Type) where
  constructor WithIndex
  unIndex : f a


export
Functor f => Functor (FixIndex x f) where
  map f (WithIndex m) = WithIndex $ map f m

export
{x : i} -> Functor f => IndFunctor i (FixIndex x f) where
  imap f (WithIndex m) = WithIndex $ map (f x) m

export
Foldable f => Foldable (FixIndex x f) where
  foldl f z (WithIndex m) = foldl f z m
  foldr f z (WithIndex m) = foldr f z m

export
{x : i} -> Foldable f => IndFoldable i (FixIndex x f) where
  ifoldl f z (WithIndex m) = foldl (flip f x) z m
  ifoldr f z (WithIndex m) = foldr (f x) z m

export
Traversable f => Traversable (FixIndex x f) where
  traverse f (WithIndex m) = WithIndex <$> traverse f m

export
{x : i} -> Traversable f => IndTraversable i (FixIndex x f) where
  itraverse f (WithIndex m) = WithIndex <$> traverse (f x) m

export
Filterable f => Filterable (FixIndex x f) where
  mapMaybe f (WithIndex m) = WithIndex $ mapMaybe f m

export
{x : i} -> Filterable f => IndFilterable i (FixIndex x f) where
  imapMaybe f (WithIndex m) = WithIndex $ mapMaybe (f x) m

export
Witherable f => Witherable (FixIndex x f) where
  wither f (WithIndex m) = WithIndex <$> wither f m

export
{x : i} -> Witherable f => IndWitherable i (FixIndex x f) where
  iwither f (WithIndex m) = WithIndex <$> wither (f x) m
