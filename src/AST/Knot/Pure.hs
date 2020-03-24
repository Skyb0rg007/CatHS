{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module AST.Knot.Pure
    (
    ) where

import           AST.Knot
import           AST.Knot.Class
import           Control.DeepSeq (NFData)
import           Data.Constraint (withDict)
import           Data.Function   ((&))
import           Data.Proxy      (Proxy (Proxy))
import           GHC.Generics    (Generic)

newtype Pure k = Pure (k # Pure)
    deriving stock Generic

deriving instance (Eq (k # Pure)) => Eq (Pure k)
deriving instance (Ord (k # Pure)) => Ord (Pure k)
deriving instance (Show (k # Pure)) => Show (Pure k)
deriving instance (NFData (k # Pure)) => NFData (Pure k)

instance KNodes Pure where
    type KNodesConstraint Pure c = c Pure
    data KWitness Pure n where
        W_Pure_Pure :: KWitness Pure Pure
    kLiftConstraint W_Pure_Pure Proxy r = r

instance KFunctor Pure where
    mapK f (Pure x) = Pure (f W_Pure_Pure x)
    {-# INLINE mapK #-}

instance KApply Pure where
    zipK (Pure a) (Pure b) = Pure (Pair a b)
    {-# INLINE zipK #-}

instance KPointed Pure where
    pureK a = Pure (a W_Pure_Pure)
    {-# INLINE pureK #-}

instance KFoldable Pure where
    foldMapK f (Pure x) = f W_Pure_Pure x
    {-# INLINE foldMapK #-}

instance KTraversable Pure where
    sequenceK (Pure x) = Pure <$> runContainedK x
    {-# INLINE sequenceK #-}

instance KMonad Pure where
    -- No clue how this works
    joinK x =
        withDict (recursively (p x)) $
            Pure $
                mapK (Proxy @(Recursively KFunctor) #> joinK) $
                    (x & getCompose & getPure & getCompose & getPure & getCompose)
        where
            getPure :: Pure k -> k # Pure
            getPure (Pure x) = x

            p :: Tree (Compose Pure Pure) p -> Proxy (KFunctor p)
            p _ = Proxy
    {-# INLINE joinK #-}

instance RNodes Pure
instance c Pure => Recursively c Pure
instance RTraversable Pure

