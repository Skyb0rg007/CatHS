{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Knot.Class.KTraversable
    ( ContainedK (..)
    , KTraversable (..)
    , traverseK
    ) where

import           AST.Knot                 (Knot, Tree)
import           AST.Knot.Class.KFoldable (KFoldable)
import           AST.Knot.Class.KFunctor  (KFunctor (mapK))
import           AST.Knot.Class.KNodes    (KNodes (KWitness))

newtype ContainedK f p (k :: Knot) = MkContainedK { runContainedK :: f (p k) }

class (KFunctor k, KFoldable k) => KTraversable k where
    sequenceK
        :: Applicative f
        => Tree k (ContainedK f p)
        -> f (Tree k p)

traverseK
    :: ( Applicative f, KTraversable k )
    => (forall n . KWitness k n -> Tree p n -> f (Tree q n))
    -> Tree k p
    -> f (Tree k q)
traverseK f = sequenceK . mapK (fmap MkContainedK . f)
{-# INLINE traverseK #-}
