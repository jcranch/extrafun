module Data.Filterable


public export
interface (Functor t) => Filterable t where

  mapMaybe : (a -> Maybe b) -> t a -> t b
  mapMaybe f = catMaybes . map f

  catMaybes : t (Maybe a) -> t a
  catMaybes = mapMaybe id

  filter : (a -> Bool) -> t a -> t a
  filter f = catMaybes . map g where
    g : a -> Maybe a
    g x = if f x then Just x else Nothing

  flush : t a -> t b
  flush = mapMaybe (const Nothing)


export
Filterable Maybe where
  mapMaybe = (=<<)

export
Filterable List where
  mapMaybe = Prelude.List.mapMaybe
