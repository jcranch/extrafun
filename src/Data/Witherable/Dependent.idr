module Data.Witherable.Dependent

import public Data.Filterable.Dependent
import public Data.Traversable.Dependent


public export
interface (DepTraversable i t, DepFilterable i t) => DepWitherable (0 i : Type) (0 t : (i -> Type) -> Type) | t where
  dwither : (Applicative f) => {0 u : i -> Type} -> {0 v : i -> Type} -> ((x : i) -> u x -> f (Maybe (v x))) -> t u -> f (t v)
  dwither f = map dcatMaybes . dtraverse f
