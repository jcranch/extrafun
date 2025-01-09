module Control.Monad.Restricted

import Control.Applicative.Restricted
import Data.Functor.Restricted


export infixl 1 =<<
export infixl 1 >=>
export infixl 1 <=<


public export
interface RApplicative r m => RMonad (0 r : Type -> Type) (0 m : Type -> Type) where
  rjoin : (r x, r y) => m x -> (x -> m y) -> m y

export
(>>=) : {0 m : Type -> Type} -> (RMonad r m) => (r x, r y) => m x -> (x -> m y) -> m y
(>>=) = rjoin

export
(=<<) : (RMonad r m) => (r x, r y) => (x -> m y) -> m x -> m y
f =<< a = a >>= f

export
(<=<) : (RMonad r m) => (r x, r y, r z) => (y -> m z) -> (x -> m y) -> x -> m z
(g <=< f) x = g =<< f x

export
(>=>) : (RMonad r m) => (r x, r y, r z) => (x -> m y) -> (y -> m z) -> x -> m z
f >=> g = g <=< f
