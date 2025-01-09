module Control.Kleisli

import Control.Category
import Data.Functor.Indexed
import Data.Filterable.Indexed


record Kleisli (m : Type -> Type) (a : Type) (b : Type) where
  constructor MkKleisli
  runKleisli : a -> m b

Functor m => Functor (Kleisli m a) where
  map f (MkKleisli p) = MkKleisli (map f . p)

Functor m => IndFunctor a (Kleisli m a) where
  imap f (MkKleisli p) = MkKleisli (\x => map (f x) (p x))

Applicative m => Applicative (Kleisli m a) where
  pure = MkKleisli . const . pure
  MkKleisli p <*> MkKleisli q = MkKleisli (\x => p x <*> q x)

Monad m => Monad (Kleisli m a) where
  MkKleisli p >>= r = MkKleisli (\x => p x >>= (\a => runKleisli (r a) x))

-- Haven't got this working yet
{-
Monad m => Category (Kleisli m) where
  identity a = MkKleisli pure
  cmp (MkKleisli g) (MkKleisli f) = MkKleisli (g <=< f)
-}

-- This belongs in with Alternative
liftMaybe : Alternative m => Maybe a -> m a
liftMaybe = maybe empty pure

-- Why must the type be called a'? It's a little mysterious
(Monad m, Alternative m) => Filterable (Kleisli m a') where
  mapMaybe f (MkKleisli p) = MkKleisli (liftMaybe . f <=< p)
  flush (MkKleisli p) = MkKleisli (const empty)

(Monad m, Alternative m) => IndFilterable a (Kleisli m a) where
  imapMaybe f (MkKleisli p) = MkKleisli (\x => liftMaybe . f x =<< p x)
