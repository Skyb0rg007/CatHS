{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- Lift a 'Functor' or type constructor to a 'Knot'

module AST.Knot.Functor
    (
    ) where

import           AST.Knot
import           AST.Knot.Class
import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

newtype F f h = F (f (h # F f))
    deriving stock Generic

deriving instance (Eq (f (k # F f))) => Eq (F f k)
deriving instance (Ord (f (k # F f))) => Ord (F f k)
deriving instance (Show (f (k # F f))) => Show (F f k)
deriving instance (NFData (f (k # F f))) => NFData (F f k)

instance KNodes (F f) where
    type KNodesConstraint (F f) c = c (F f)
    data KWitness (F f) n where
        W_F_F_f :: KWitness (F f) (F f)
    kLiftConstraint W_F_F_f _ r = r

instance Functor f => KFunctor (F f) where
    mapK f (F x) = F (fmap (f W_F_F_f) x)

instance Applicative f => KApply (F f) where
    zipK (F a) (F b) = F $ Pair <$> a <*> b

instance Applicative f => KPointed (F f) where
    pureK f = F (pure (f W_F_F_f))

instance Foldable f => KFoldable (F f) where
    foldMapK f (F x) = foldMap (f W_F_F_f) x

instance Traversable f => KTraversable (F f) where
    sequenceK (F x) = F <$> traverse runContainedK x

instance Monad f => KMonad (F f) where
    joinK = undefined
        -- withDict (recursively ())
        -- where
            -- getF :: F f h -> f (h # F f)
            -- getF (F x) = x
            -- t :: forall p . Recursively KFunctor p
              -- => p # Compose (F f) (F f)
              -- -> p # F f
            -- t = withDict (recursively (Proxy @(KFunctor p))) $
                -- mapK (Proxy @(Recursively KFunctor) #> joinK)

instance RNodes (F f)
instance c (F f) => Recursively c (F f)
instance Traversable f => RTraversable (F f)

