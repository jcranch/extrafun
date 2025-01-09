module Data.Foldable.Indexed

import Data.Vect


-- I would propose that the ordinary Foldable interface also make
-- concatMap a method, since whether left folds or right folds are
-- faster depends on the data structure.


-- changed so as not to depend on IndFunctor as well
public export
interface (Foldable f) => IndFoldable i f | f where

  ifoldl : (a -> i -> x -> a) -> a -> f x -> a
  ifoldr : (i -> x -> a -> a) -> a -> f x -> a

  iconcatMap : Monoid m => (i -> x -> m) -> f x -> m
  iconcatMap f = ifoldl (\a, i, x => a <+> f i x) neutral

export
iconcatMapRight : (IndFoldable i f, Monoid m) => (i -> x -> m) -> f x -> m
iconcatMapRight f = ifoldr (\i, x, a => f i x <+> a) neutral

export
IndFoldable () Maybe where
  ifoldl f = foldl (\x => f x ())
  ifoldr f = foldr (f ())
  iconcatMap f = concatMap (f ())

export
IndFoldable a (Pair a) where
  ifoldl f z (MkPair x y) = f z x y
  ifoldr f z (MkPair x y) = f x y z

export
IndFoldable Nat List where
  ifoldl f = inner 0 where
    inner : Nat -> a -> List x -> a
    inner _ z [] = z
    inner i z (x::xs) = inner (i+1) (f z i x) xs
  ifoldr f z = inner 0 where
    inner : Nat -> List x -> a
    inner _ [] = z
    inner i (x::xs) = f i x (inner (i+1) xs)

export
IndFoldable (Fin k) (Vect k) where
  ifoldl f a [] = a
  ifoldl f a (x::xs) = ifoldl (\y, i => f y (FS i)) (f a FZ x) xs
  ifoldr f a [] = a
  ifoldr f a (x::xs) = f FZ x (ifoldr (f . FS) a xs)
