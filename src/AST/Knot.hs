{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module AST.Knot
    ( Knot (Knot)
    , GetKnot
    , Tree
    , type (#)
    ) where

import           Data.Kind (Type)

-- | Kind for higher-kinded data
newtype Knot = Knot (Knot -> Type)

-- | Type-level getter for Knot
type family GetKnot k where
    GetKnot ('Knot t) = t

-- | Type synonym to express nested higher-kinded data structures
type Tree k p = (k ('Knot p) :: Type)

-- | Type synonym to express child nodes n nested higher-kinded data structures
type k # p = Tree (GetKnot k) p
