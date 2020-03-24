{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module AST.Knot.Class.KNodes
    ( KNodes (..)
    , (#>)
    , (#*#)
    ) where

import           AST.Knot   (Knot)
import           Data.Kind  (Constraint, Type)
import           Data.Proxy (Proxy)

-- | Class for lifting a constraint to the child nodes of a 'Knot'
class KNodes (k :: Knot -> Type) where
    type family KNodesConstraint k (c :: ((Knot -> Type) -> Constraint)) :: Constraint
    data family KWitness k :: (Knot -> Type) -> Type
    kLiftConstraint
        :: KNodesConstraint k c
        => KWitness k n
        -> Proxy c
        -> (c n => r)
        -> r

infixr 0 #>
infixr 0 #*#

(#>)
    :: ( KNodes k, KNodesConstraint k c )
    => Proxy c
    -> (c n => r)
    -> KWitness k n
    -> r
(#>) p r w = kLiftConstraint w p r

(#*#)
    :: ( KNodes k, KNodesConstraint k c )
    => Proxy c
    -> (KWitness k n -> (c n => r))
    -> KWitness k n
    -> r
(#*#) p r w = (p #> r) w w
