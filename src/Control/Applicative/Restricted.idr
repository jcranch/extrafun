module Control.Applicative.Restricted

import Data.Functor.Restricted


export infixl 4 <*
export infixl 4 *>


-- We provide a restricted version of liftA2 rather than of <*>, since
-- in cases of interest (x -> y) does not satisfy the restriction.
public export
interface RFunctor r f => RApplicative (0 r : Type -> Type) (0 f : Type -> Type) | f where
  rpure : r x => x -> f x
  rliftA2 : (r x, r y, r z) => (x -> y -> z) -> f x -> f y -> f z

export
(<*) : (RApplicative r f, r x, r y) => f x -> f y -> f x
(<*) = rliftA2 const

export
(*>) : (RApplicative r f, r x, r y) => f x -> f y -> f y
(*>) = rliftA2 (const id)

public export
data RApp : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  MkRApp : f x -> RApp r f x

(RApplicative r f, Monoid m, r m) => Semigroup (RApp r f m) where
  (<+>) (MkRApp x) (MkRApp y) = MkRApp (rliftA2 (<+>) x y)

(RApplicative r f, Monoid m, r m) => Monoid (RApp r f m) where
  neutral = MkRApp $ rpure neutral
