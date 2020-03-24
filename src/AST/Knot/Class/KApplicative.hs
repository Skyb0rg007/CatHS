{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Knot.Class.KApplicative
    ( KApplicative (..)
    ) where

import           AST.Knot.Class.KApply   (KApply)
import           AST.Knot.Class.KPointed (KPointed)

class (KPointed k, KApply k) => KApplicative k
instance (KPointed k, KApply k) => KApplicative k
