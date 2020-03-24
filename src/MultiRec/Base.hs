{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module MultiRec.Base
    (
    ) where

import           Control.Applicative
import           Data.Type.Equality
import           MultiRec.Constructor

infixr 5 :+:
-- infix  6 :>:
infixr 7 :*:

-- Recursive position
newtype I xi (r :: * -> *) ix = I { unI :: r xi }

-- Constant types
newtype K a  (r :: * -> *) ix = K { unK :: a }

-- Constructor without fields
data U (r :: * -> *) ix = U

-- Sums
data (f :+: g) (r :: * -> *) ix = L (f r ix) | R (g r ix)

-- Products
data (f :*: g) (r :: * -> *) ix = f r ix :*: g r ix

-- Indicates the type that a particular constructor injects to
data (f :>: ix) (r :: * -> *) ix' where
    Tag :: f r ix -> (f :>: ix) r ix

unTag :: (f :>: ix) r ix -> f r ix
unTag (Tag x) = x

-- Represents composition of functors


