{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

-- Instances for 'Const', 'Product', and 'Sum'

module AST.Knot.Class.Orphans () where

import           AST.Knot.Class.KApplicative
import           AST.Knot.Class.KApply
import           AST.Knot.Class.KFoldable
import           AST.Knot.Class.KFunctor
import           AST.Knot.Class.KMonad
import           AST.Knot.Class.KNodes
import           AST.Knot.Class.KPointed
import           AST.Knot.Class.KTraversable
import           AST.Knot.Class.Recursive
import           Data.Functor.Const
import           Data.Functor.Product
import           Data.Functor.Sum

-- * KNodes

instance KNodes (Const a) where
    type KNodesConstraint (Const a) x = ()
    data KWitness (Const a) i
    kLiftConstraint x = case x of
    {-# INLINE kLiftConstraint #-}

instance (KNodes a, KNodes b) => KNodes (Product a b) where
    type KNodesConstraint (Product a b) x = (KNodesConstraint a x, KNodesConstraint b x)
    data KWitness (Product a b) n where
        E_Product_a :: KWitness a n -> KWitness (Product a b) n
        E_Product_b :: KWitness b n -> KWitness (Product a b) n
    kLiftConstraint (E_Product_a w) = kLiftConstraint w
    kLiftConstraint (E_Product_b w) = kLiftConstraint w
    {-# INLINE kLiftConstraint #-}

instance (KNodes a, KNodes b) => KNodes (Sum a b) where
    type KNodesConstraint (Sum a b) x = (KNodesConstraint a x, KNodesConstraint b x)
    data KWitness (Sum a b) n where
        E_Sum_a :: KWitness a n -> KWitness (Sum a b) n
        E_Sum_b :: KWitness b n -> KWitness (Sum a b) n
    kLiftConstraint (E_Sum_a w) = kLiftConstraint w
    kLiftConstraint (E_Sum_b w) = kLiftConstraint w
    {-# INLINE kLiftConstraint #-}


-- * Functor

instance KFunctor (Const a) where
    mapK _ (Const x) = Const x
    {-# INLINE mapK #-}

instance (KFunctor a, KFunctor b) => KFunctor (Product a b) where
    mapK f (Pair x y) = Pair (mapK (f . E_Product_a) x) (mapK (f . E_Product_b) y)
    {-# INLINE mapK #-}

instance (KFunctor a, KFunctor b) => KFunctor (Sum a b) where
    mapK f (InL x) = InL (mapK (f . E_Sum_a) x)
    mapK f (InR x) = InR (mapK (f . E_Sum_b) x)
    {-# INLINE mapK #-}

-- * Foldable

instance KFoldable (Const a) where
    foldMapK _ _ = mempty
    {-# INLINE foldMapK #-}

instance (KFoldable a, KFoldable b) => KFoldable (Product a b) where
    foldMapK f (Pair x y) = foldMapK (f . E_Product_a) x <> foldMapK (f . E_Product_b) y
    {-# INLINE foldMapK #-}

instance (KFoldable a, KFoldable b) => KFoldable (Sum a b) where
    foldMapK f (InL x) = foldMapK (f . E_Sum_a) x
    foldMapK f (InR x) = foldMapK (f . E_Sum_b) x
    {-# INLINE foldMapK #-}

-- * Traversable

instance KTraversable (Const a) where
    sequenceK (Const x) = pure (Const x)
    {-# INLINE sequenceK #-}

instance (KTraversable a, KTraversable b) => KTraversable (Product a b) where
    sequenceK (Pair x y) = Pair <$> sequenceK x <*> sequenceK y
    {-# INLINE sequenceK #-}

instance (KTraversable a, KTraversable b) => KTraversable (Sum a b) where
    sequenceK (InL x) = InL <$> sequenceK x
    sequenceK (InR x) = InR <$> sequenceK x
    {-# INLINE sequenceK #-}

-- * Pointed (the `pure` method from Applicative)

instance Monoid a => KPointed (Const a) where
    pureK _ = Const mempty
    {-# INLINE pureK #-}

instance (KPointed a, KPointed b) => KPointed (Product a b) where
    pureK f = Pair (pureK (f . E_Product_a)) (pureK (f . E_Product_b))
    {-# INLINE pureK #-}

-- * Apply (the `(<*>)` method from Applicative)

instance Semigroup a => KApply (Const a) where
    zipK (Const x) (Const y) = Const (x <> y)

instance (KApply a, KApply b) => KApply (Product a b) where
    zipK (Pair a b) (Pair a' b') = Pair (zipK a a') (zipK b b')

-- * Applicative

-- * Monad

-- * Recursive

instance RNodes (Const a)
instance c (Const a) => Recursively c (Const a)
instance RTraversable (Const a)

