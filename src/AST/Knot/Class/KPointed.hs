{-# LANGUAGE RankNTypes #-}

module AST.Knot.Class.KPointed
    ( KPointed (..)
    ) where

import           AST.Knot              (Tree)
import           AST.Knot.Class.KNodes (KNodes (KWitness))

class KNodes k => KPointed k where
    pureK
        :: (forall n . KWitness k n -> Tree p n)
        -> Tree k p
