{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module AST.Knot.Class.KFunctor
    ( KFunctor (..)
    ) where

import           AST.Knot              (Tree)
import           AST.Knot.Class.KNodes (KNodes (KWitness))
import           Data.Kind             (Constraint, Type)
import           Data.Proxy            (Proxy)

class KNodes k => KFunctor k where
    mapK
        :: (forall n . KWitness k n -> Tree p n -> Tree q n)
        -> Tree k p
        -> Tree k q

