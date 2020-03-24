{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Cat.Common
    ( module Cat.Common
    ) where

import Data.Text (Text)
import Data.Word (Word64)
import Control.Monad.State.Strict
import Control.Lens

-- * Symbols

newtype Symbol = Symbol { _symbolId :: Word64 }
    deriving newtype (Eq, Ord)
    deriving stock   (Show, Read)

-- * Comparisons

data InfixOp
    = OpMult
    | OpDiv
    | OpAdd
    | OpSub
    | OpAnd
    | OpOr
    deriving (Show, Eq, Ord, Enum)

-- * Lenses

makeLenses ''Symbol

-- * Monads

class MonadSymbolGen m where
    newSymbol :: m Symbol

newtype SymbolGenT m a = SymbolGenT { unSymbolGenT :: StateT Word64 m a }
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     )

instance Monad m => MonadSymbolGen (SymbolGenT m) where
    newSymbol = SymbolGenT (gets Symbol <* modify' succ)

runSymbolGenT :: Monad m => SymbolGenT m a -> m a
runSymbolGenT (SymbolGenT m) = evalStateT m 0

