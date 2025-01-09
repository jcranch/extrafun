module Data.Functor.Restricted


public export
interface RFunctor (0 r : Type -> Type) (0 f : Type -> Type) where
  rmap : (r x, r y) => (x -> y) -> f x -> f y
