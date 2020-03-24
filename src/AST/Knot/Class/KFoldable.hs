{-# LANGUAGE RankNTypes #-}

module AST.Knot.Class.KFoldable
    ( KFoldable (..)
    , traverseK_
    ) where

import           AST.Knot              (Tree)
import           AST.Knot.Class.KNodes (KNodes (KWitness))
import           Data.Foldable         (sequenceA_)

class KNodes k => KFoldable k where
    foldMapK
        :: Monoid a
        => (forall n . KWitness k n -> Tree p n -> a)
        -> Tree k p
        -> a

traverseK_
    :: ( Applicative f, KFoldable k )
    => (forall c . KWitness k c -> Tree m c -> f ())
    -> Tree k m
    -> f ()
traverseK_ f = sequenceA_ . foldMapK (fmap (:[]) . f)
{-# INLINE traverseK_ #-}
