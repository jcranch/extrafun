||| An attempt at a class of functors defined on a subclass of Type.
module Data.Functor.Restricted

import Data.SortedMap
import Data.SortedSet


public export
interface RFunctor (0 r : Type -> Type) (0 f : Type -> Type) | f where
  rmap : (r x, r y) => (x -> y) -> f x -> f y

RFunctor Ord SortedSet where
  rmap f = fromList . map f . Prelude.toList
