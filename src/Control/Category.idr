module Control.Category

import Data.Morphisms


{-
-- it doesn't like this for reasons unknown
public export
interface Category (m : o -> o -> Type) where
  identity : (a : o) -> m a a
  cmp : m b c -> m a b -> m a c


export
Category Morphism where
  identity a = Mor id
  cmp (Mor g) (Mor f) = Mor (g . f)


public export
record Op (m : o -> o -> Type) (a : o) (b : o) where
  constructor MkOp
  getOp : m b a

-- Haven't got this working yet
Category m => Category (Op m) where
  identity = MkOp . identity
  cmp (MkOp f) (MkOp g) = MkOp (cmp g f)


public export
data ProdC : (m1 : o1 -> o1 -> Type) -> (m2 : o2 -> o2 -> Type) -> (o1,o2) -> (o1,o2) -> Type where
  MkProdC : {0 a1 : o1} -> {0 b1 : o1} -> {0 a2 : o2} -> {0 b2 : o2} -> m1 a1 b1 -> m2 a2 b2 -> ProdC m1 m2 (a1,a2) (b1,b2)

proj1C : ProdC m1 m2 (a1,a2) (b1,b2) -> m1 a1 b1
proj1C (MkProdC f _) = f

proj2C : ProdC m1 m2 (a1,a2) (b1,b2) -> m2 a2 b2
proj2C (MkProdC _ f) = f

-- Haven't got this working yet
{-
(Category m1, Category m2) => Category (ProdC m1 m2) where
  identity (x,y) = MkProdC (identity x) (identity y)
  cmp (MkProdC g1 g2) (MkProdC f1 f2) = MkProdC (cmp g1 f1) (cmp g2 f2)
-}

-}
