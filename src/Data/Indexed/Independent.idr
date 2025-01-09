module Data.Indexed.Independent

-- Code for treating a dependent functor as an independent functor.

import Data.Witherable.Dependent
import Data.Witherable.Indexed


public export
record Independent {k : Type} (t : (k -> Type) -> Type) (v : Type) where
  constructor Indep
  dep : t (const v)

export
DepFunctor i t => Functor (Independent t) where
  map f (Indep m) = Indep $ dmap (const f) m

export
DepFunctor i t => IndFunctor i (Independent t) where
  imap f (Indep m) = Indep $ dmap f m

export
DepFoldable i t => Foldable (Independent t) where
  foldl f z (Indep m) = dfoldl (\x, _, y => f x y) z m
  foldr f z (Indep m) = dfoldr (\_, x, y => f x y) z m

export
(DepFoldable i t, DepFunctor i t) => IndFoldable i (Independent t) where
  ifoldl f z (Indep m) = dfoldl f z m
  ifoldr f z (Indep m) = dfoldr f z m
  iconcatMap f (Indep m) = dconcatMap f m

export
(DepTraversable i t, DepFunctor i t) => Traversable (Independent t) where
  traverse f (Indep m) = Indep <$> dtraverse (const f) m

export
(DepTraversable i t, DepFunctor i t, DepFoldable i t) => IndTraversable i (Independent t) where
  itraverse f (Indep m) = Indep <$> dtraverse f m

export
(DepFilterable i t, DepFoldable i t) => Filterable (Independent t) where
  mapMaybe f (Indep m) = Indep $ dmapMaybe (const f) m

export
(DepFilterable i t, DepFoldable i t) => IndFilterable i (Independent t) where
  imapMaybe f (Indep m) = Indep $ dmapMaybe f m

export
(DepWitherable i t) => Witherable (Independent t) where
  wither f (Indep m) = Indep <$> dwither (const f) m

export
[independentWitherable] (DepWitherable i t) => IndWitherable i (Independent t) where
  iwither f (Indep m) = Indep <$> dwither f m
