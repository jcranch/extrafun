module Data.Foldable.Dependent


public export
interface DepFoldable (0 i : Type) (0 t : (i -> Type) -> Type) | t where
  dfoldl : {0 v : i -> Type} -> (m -> (x : i) -> v x -> m) -> m -> t v -> m
  dfoldr : {0 v : i -> Type} -> ((x : i) -> v x -> m -> m) -> m -> t v -> m

  dconcatMap : Monoid m => {0 v : i -> Type} -> ((x : i) -> v x -> m) -> t v -> m
  dconcatMap f = dfoldl (\a, x, y => a <+> f x y) neutral

||| an alternative implementation, using a right fold instead
dconcatMap' : {0 i : Type} -> {0 t : (i -> Type) -> Type} -> (DepFoldable i t, Monoid m) => {0 v : i -> Type} -> ((x : i) -> v x -> m) -> t v -> m
dconcatMap' f = dfoldr (\x, y, a => f x y <+> a) neutral

export
dToList : DepFoldable i t => t v -> List (DPair i v)
dToList = dfoldr (\x, y, l => (x ** y)::l) []

export
indepToList : DepFoldable i t => t (const a) -> List (i, a)
indepToList = dfoldr (\x, y, l => (x, y)::l) []


export
{0 i : Type} -> DepFoldable i (DPair i) where
  dfoldl f z (x ** y) = f z x y
  dfoldr f z (x ** y) = f x y z
  dconcatMap f (x ** y) = f x y
