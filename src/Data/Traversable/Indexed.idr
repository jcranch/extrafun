module Data.Traversable.Indexed

import Data.Vect

import public Data.Foldable.Indexed


public export
interface (Traversable t, IndFoldable i t) => IndTraversable i t | t where
  itraverse : Applicative f => (i -> a -> f b) -> t a -> f (t b)


export
IndTraversable () Maybe where
  itraverse f = traverse (f ())

export
IndTraversable a (Pair a) where
  itraverse f (MkPair x y) = MkPair x <$> f x y

export
IndTraversable Nat List where
  itraverse g = inner 0 where
    inner : Nat -> List a -> f (List b)
    inner _ [] = pure []
    inner i (a::as) = (::) <$> g i a <*> inner (i+1) as

export
IndTraversable (Fin k) (Vect k) where
  itraverse f [] = pure []
  itraverse f (x::xs) = (::) <$> f FZ x <*> itraverse (f . FS) xs
