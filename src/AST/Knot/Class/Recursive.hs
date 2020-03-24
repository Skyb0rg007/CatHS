{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module AST.Knot.Class.Recursive
    ( Recursive (recurse)
    , RNodes (recursiveKNodes)
    , Recursively (recursively)
    , RTraversable (recursiveKTraversable)
    ) where

import           AST.Knot                    (Knot, Tree)
import           AST.Knot.Class.KFoldable    (KFoldable)
import           AST.Knot.Class.KFunctor     (KFunctor)
import           AST.Knot.Class.KNodes       (KNodes (KNodesConstraint, KWitness))
import           AST.Knot.Class.KTraversable (KTraversable)
import           Data.Constraint             (Dict (Dict))
import           Data.Kind                   (Constraint, Type)
import           Data.Proxy                  (Proxy (Proxy))

-- | Class of constraints that apply to all recursive child nodes
class Recursive c where
    recurse
        :: (KNodes k, c k)
        => Proxy (c k)
        -> Dict (KNodesConstraint k c)

-- | Class of 'Knot's which recursively implement 'KNodes'
class KNodes k => RNodes k where
    recursiveKNodes
        :: Proxy k
        -> Dict (KNodesConstraint k RNodes)
    default recursiveKNodes
        :: KNodesConstraint k RNodes
        => Proxy k
        -> Dict (KNodesConstraint k RNodes)
    recursiveKNodes _ = Dict
    {-# INLINE recursiveKNodes #-}

instance Recursive RNodes where
    recurse = recursiveKNodes . f
        where
            f :: Proxy (f k :: Constraint) -> Proxy (k :: Knot -> Type)
            f _ = Proxy

instance Recursive RTraversable where
    recurse = recursiveKTraversable . f
        where
            f :: Proxy (f k :: Constraint) -> Proxy (k :: Knot -> Type)
            f _ = Proxy

-- | A constraint lifted to apply recursively
class RNodes k => Recursively c k where
    recursively
        :: Proxy (c k)
        -> Dict (c k, KNodesConstraint k (Recursively c))
    default recursively
        :: (c k, KNodesConstraint k (Recursively c))
        => Proxy (c k)
        -> Dict (c k, KNodesConstraint k (Recursively c))
    recursively _ = Dict
    {-# INLINE recursively #-}

-- | Class of 'Knot's that recursively implement KTraversable
class (KTraversable k, Recursively KFunctor k, Recursively KFoldable k) => RTraversable k where
    recursiveKTraversable
        :: Proxy k
        -> Dict (KNodesConstraint k RTraversable)
    default recursiveKTraversable
        :: KNodesConstraint k RTraversable
        => Proxy k
        -> Dict (KNodesConstraint k RTraversable)
    recursiveKTraversable _ = Dict
    {-# INLINE recursiveKTraversable #-}

