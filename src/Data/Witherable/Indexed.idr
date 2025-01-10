||| An indexed Witherable
module Data.Witherable.Indexed

import public Data.Filterable.Indexed
import public Data.Traversable.Indexed
import public Data.Witherable


public export
interface (Witherable t, IndTraversable i t, IndFilterable i t) => IndWitherable i t | t where
  iwither : (Applicative f) => (i -> a -> f (Maybe b)) -> t a -> f (t b)
  iwither f = map catMaybes . itraverse f


export
IndWitherable () Maybe where


-- This is not a conformant implementation, because filtering changes
-- the indices, but it's useful functionality.
export
enumWither : (Applicative f) => (Nat -> a -> f (Maybe b)) -> List a -> f (List b)
enumWither g = inner 0 where
  inner : Nat -> List a -> f (List b)
  inner _ [] = pure []
  inner n (x::xs) = maybeCompose <$> g n x <*> inner (n+1) xs where
    maybeCompose : Maybe b -> List b -> List b
    maybeCompose Nothing ys = ys
    maybeCompose (Just y) ys = y :: ys
