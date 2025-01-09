module Data.Filterable.Indexed

import public Data.Filterable
import public Data.Functor.Indexed


public export
interface (Filterable t, IndFunctor i t) => IndFilterable i t | t where
  imapMaybe : (i -> a -> Maybe b) -> t a -> t b

export
IndFilterable () Maybe where
  imapMaybe f = mapMaybe (f ())


-- This is not a conformant implementation of IndFilterable, because
-- filtering changes the indices, but it's useful functionality.
export
enumFilter : (Nat -> a -> Maybe b) -> List a -> List b
enumFilter f = inner 0 where
  inner : Nat -> List a -> List b
  inner _ [] = []
  inner n (x::xs) = case f n x of
    Just y => y::inner (n+1) xs
    Nothing => inner (n+1) xs
