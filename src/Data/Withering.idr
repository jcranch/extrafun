module Data.Withering

import Data.Indexed.Fixed
import Data.Witherable.Indexed


public export
data Withering : (Type -> Type) -> Type -> Type -> Type -> Type where
  MkWithering : Applicative f
             => ({0 t : Type -> Type} -> (IndWitherable i t) => t a -> f (t b))
             -> Withering f i a b

export
runWither : (IndWitherable i t) => Withering f i a b -> t a -> f (t b)
runWither (MkWithering w) m = w m

export
witherOne : Withering f i a b -> i -> a -> f (Maybe b)
witherOne (MkWithering w) x y = let
  z : FixIndex x Maybe a
  z = WithIndex (Just y)
  in unIndex <$> w z



export
preserving : Applicative f => Withering f i a a
preserving = MkWithering (pure . id)

export
flushing : Applicative f => Withering f i a b
flushing = MkWithering (pure . flush)

export
mapping : Applicative f => (a -> b) -> Withering f i a b
mapping f = MkWithering (pure . map f)

export
imapping : Applicative f => (i -> a -> b) -> Withering f i a b
imapping f = MkWithering (pure . imap f)

export
mapMaybeing : Applicative f => (a -> Maybe b) -> Withering f i a b
mapMaybeing f = MkWithering (pure . mapMaybe f)

export
imapMaybeing : Applicative f => (i -> a -> Maybe b) -> Withering f i a b
imapMaybeing f = MkWithering (pure . imapMaybe f)

export
traversing : Applicative f => (a -> f b) -> Withering f i a b
traversing f = MkWithering (traverse f)

export
itraversing : Applicative f => (i -> a -> f b) -> Withering f i a b
itraversing f = MkWithering (itraverse f)

export
withering : Applicative f => (a -> f (Maybe b)) -> Withering f i a b
withering f = MkWithering (wither f)

export
iwithering : Applicative f => (i -> a -> f (Maybe b)) -> Withering f i a b
iwithering f = MkWithering (iwither f)
