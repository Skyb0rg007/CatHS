{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}

module AST.Knot.Class.KMonad
    ( KMonad (joinK)
    , Compose (MkCompose, getCompose)
    , bindK
    ) where

import           AST.Knot                    (GetKnot, Tree)
import           AST.Knot.Class.KApplicative
import           AST.Knot.Class.KFunctor
import           AST.Knot.Class.KNodes       (KNodes (KWitness))
import           AST.Knot.Class.Recursive
import           Data.Kind                   (Constraint, Type)
import           Data.Proxy                  (Proxy)
import           GHC.Generics                (Generic)

newtype Compose a b k = MkCompose { getCompose :: Tree a (Compose b (GetKnot k)) }
    deriving stock Generic

class KApplicative k => KMonad k where
    joinK
        :: Recursively KFunctor p
        => Tree (Compose k k) p
        -> Tree k p

bindK
    :: ( KMonad k, Recursively KFunctor p )
    => Tree k p
    -> (forall n . KWitness k n -> Tree p n -> Tree (Compose k p) n)
    -> Tree k p
bindK x f = joinK $ MkCompose $ mapK f x
