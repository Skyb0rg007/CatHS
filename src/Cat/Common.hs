{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Cat.Common
    ( module Cat.Common
    ) where

import           Control.Lens
import           Data.Text                  (Text)
import           Data.Word                  (Word64)
import           Polysemy
import           Polysemy.State

-- * Symbols

-- | Used to represent variables in LIR
newtype Symbol = Symbol { _symbolId :: Word64 }
    deriving stock (Show, Read, Eq, Ord)

-- * Comparisons

-- | Types of infix operator in Cat
data InfixOp
    = OpMult
    | OpDiv
    | OpAdd
    | OpSub
    | OpAnd
    | OpOr
    deriving (Show, Eq, Ord, Enum)

data InfixSourceOp
    = SOpMult
    | SOpDiv
    | SOpAdd
    | SOpSub
    | SOpAnd
    | SOpOr
    | SOpEq
    | SOpNeq
    | SOpGt
    | SOpLt
    | SOpGte
    | SOpLte
    deriving (Show, Eq, Ord, Enum)

-- * Lenses

makeLenses ''Symbol

-- * Monads

data SymbolGen (m :: * -> *) a where
    NewSymbol :: SymbolGen m Symbol

makeSem ''SymbolGen

runSymbolGenInState :: Member (State Word64) r => Sem (SymbolGen ': r) a -> Sem r a
runSymbolGenInState = interpret $ \NewSymbol -> do
    s <- get
    put (s + 1)
    pure $ Symbol s

runSymbolGen :: forall r a . Sem (SymbolGen ': r) a -> Sem r a
runSymbolGen = evalState @Word64 0 . reinterpret \case
    NewSymbol -> do
        s <- get
        put (s + 1)
        pure $ Symbol s

