module Data.Witherable

import public Data.Filterable


public export
interface (Filterable t, Traversable t) => Witherable t where
  wither : (Applicative f) => (a -> f (Maybe b)) -> t a -> f (t b)
  wither k = map catMaybes . traverse k

export
Witherable Maybe where

export
Witherable List where
