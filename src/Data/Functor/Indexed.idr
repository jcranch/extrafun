module Data.Functor.Indexed

-- "Ind" can be taken to stand for "Indexed" or "Independent"

import Data.Vect

export infixr 4 <|$>


public export
interface (Functor f) => IndFunctor i f | f where
  imap : (i -> x -> y) -> f x -> f y

export
(<|$>) : (IndFunctor i f) => (i -> x -> y) -> f x -> f y
(<|$>) = imap

export
IndFunctor k (Pair k) where
  imap f (i, x) = (i, f i x)

export
IndFunctor () Maybe where
  imap f = map (f ())

export
IndFunctor Nat List where
  imap f = inner 0 where
    inner : Nat -> List x -> List y
    inner _ [] = []
    inner i (a::as) = f i a::inner (i+1) as

export
IndFunctor (Fin k) (Vect k) where
  imap _ [] = []
  imap f (x :: xs) = f FZ x :: imap (f . FS) xs