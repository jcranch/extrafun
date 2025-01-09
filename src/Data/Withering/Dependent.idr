module Data.Withering.Dependent

import Data.Indexed.FixedDep
import Data.Indexed.Independent
import Data.Witherable.Dependent
import Data.Witherable.Indexed
import Data.Withering


public export
data DepWithering : (Type -> Type) -> (i : Type) -> (i -> Type) -> (i -> Type) -> Type where
  MkDepWithering : Applicative f
                => ({0 t : (i -> Type) -> Type} -> (DepWitherable i t) => t u -> f (t v))
                -> DepWithering f i u v

export
runDepWither : (DepWitherable i t) => DepWithering f i u v -> t u -> f (t v)
runDepWither (MkDepWithering w) m = w m

export
depWitherOne : DepWithering f i u v -> (x : i) -> u x -> f (Maybe (v x))
depWitherOne (MkDepWithering w) x y = let
  z : FixDepIndex x Maybe u
  z = WithDepIndex (Just y)
  in unIndex <$> w z



export
preserving : Applicative f => DepWithering f i u u
preserving = MkDepWithering (pure . id)

export
flushing : Applicative f => DepWithering f i u v
flushing = MkDepWithering (pure . flush)

export
mapping : Applicative f => ((x : i) -> u x -> v x) -> DepWithering f i u v
mapping f = MkDepWithering (pure . dmap f)

export
mapMaybeing : Applicative f => ((x : i) -> u x -> Maybe (v x)) -> DepWithering f i u v
mapMaybeing f = MkDepWithering (pure . dmapMaybe f)

export
traversing : Applicative f => ((x : i) -> u x -> f (v x)) -> DepWithering f i u v
traversing f = MkDepWithering (dtraverse f)

export
withering : Applicative f => ((x : i) -> u x -> f (Maybe (v x))) -> DepWithering f i u v
withering f = MkDepWithering (dwither f)


export
depWithering : Withering f i a b -> DepWithering f i (const a) (const b)
depWithering (MkWithering w) = MkDepWithering (map dep . w @{independentWitherable} . Indep)
