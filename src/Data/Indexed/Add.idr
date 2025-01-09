module Data.Indexed.Add

-- Make a collection into an indexed collection by storing pairs.

import Data.Witherable.Indexed



public export
record Indexed (t : Type -> Type) (i : Type) (a : Type) where
  constructor AddIndex
  unIndex : t (Pair i a)


export
(Functor t) => Functor (Indexed t i) where
  map f (AddIndex p) = AddIndex (map f <$> p)

export
(Functor t) => IndFunctor i (Indexed t i) where
  imap f (AddIndex p) = AddIndex (imap f <$> p)

export
(Foldable t) => Foldable (Indexed t i) where
  foldr f z (AddIndex p) = foldr (\(_,a), x => f a x) z p
  foldl f z (AddIndex p) = foldl (\x, (_,a) => f x a) z p
--  concatMap f (AddIndex p) = concatMap (f . snd) p

export
(Functor t, Foldable t) => IndFoldable i (Indexed t i) where
  ifoldr f z (AddIndex p) = foldr (\(i,a), x => f i a x) z p
  ifoldl f z (AddIndex p) = foldl (\x, (i,a) => f x i a) z p

export
(Traversable t) => Traversable (Indexed t i) where
  traverse f (AddIndex p) = AddIndex <$> traverse (traverse f) p

export
(Traversable t) => IndTraversable i (Indexed t i) where
  itraverse f (AddIndex p) = AddIndex <$> traverse (itraverse f) p

export
(Filterable t) => Filterable (Indexed t i) where
  mapMaybe f (AddIndex p) = AddIndex $ mapMaybe (traverse f) p

export
(Filterable t) => IndFilterable i (Indexed t i) where
  imapMaybe f (AddIndex p) = AddIndex $ mapMaybe (itraverse f) p

export
(Witherable t) => Witherable (Indexed t i) where

export
[indWitherableIndexed] (Witherable t) => IndWitherable i (Indexed t i) where
