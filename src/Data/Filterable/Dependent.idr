module Data.Filterable.Dependent

import public Data.Functor.Dependent


public export
interface (DepFunctor i t) => DepFilterable (0 i : Type) (0 t : (i -> Type) -> Type) | t where
  dmapMaybe : {0 u : i -> Type} -> {0 v : i -> Type} -> ((x : i) -> u x -> Maybe (v x)) -> t u -> t v
  dmapMaybe f = dcatMaybes . dmap f

  dcatMaybes : {0 v : i -> Type} -> t (Maybe . v) -> t v
  dcatMaybes = dmapMaybe (\_, y => y)

  flush : {0 u : i -> Type} -> {0 v : i -> Type} -> t u -> t v
  flush = dmapMaybe (\_, _ => Nothing)
