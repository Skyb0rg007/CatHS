{-# LANGUAGE RankNTypes #-}

module AST.Knot.Class.KApply
    ( KApply (zipK)
    , liftK2
    ) where

import           AST.Knot                (Tree)
import           AST.Knot.Class.KFunctor (KFunctor (mapK))
import           AST.Knot.Class.KNodes   (KNodes (KWitness))
import           Data.Functor.Product    (Product (Pair))

class KFunctor k => KApply k where
    zipK
        :: Tree k p
        -> Tree k q
        -> Tree k (Product p q)

liftK2
    :: KApply k
    => (forall n . KWitness k n -> Tree p n -> Tree q n -> Tree r n)
    -> Tree k p
    -> Tree k q
    -> Tree k r
liftK2 f x = mapK (\w (Pair a b) -> f w a b) . zipK x

