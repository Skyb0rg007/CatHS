{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Cat.LIR
    ( module Cat.LIR
    ) where

import           Control.Lens
import           Control.Lens.TH
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Int                  (Int64)
import           Data.Map                  (Map)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Word                 (Word64)
import           Polysemy
import           Polysemy.State

import Cat.Common
import Cat.X64 (Label, Condition)

data LIRLabel
    = LIRLabel !Int
    | LIRLabelAllocate
    | LIRLabelAllocateAndMemset
    | LIRLabelPrintlnInt
    | LIRLabelPrintlnString
    | LIRLabelPrintInt
    | LIRLabelPrintString
    deriving (Show, Eq, Ord)

data LIRLit
    = LitString !Text
    | LitInt !Int64
    deriving (Show, Eq, Ord)

type LIRBinOp = InfixOp

data LIRInstruction
    = LIRNop
    | LIRAssignLit !LIRLit !Symbol
    | LIRStoreToMemoryAtOffset {- Location -} !Symbol {- Offset -} !Symbol {- Value -} !Symbol
    | LIRLoadFromMemoryAtOffset {- Location -} !Symbol {- Offset -} !Symbol {- Assign To -} !Symbol
    | LIRAssign {- Value -} !Symbol {- Assign To -} !Symbol
    | LIRNegate {- Value -} !Symbol {- Assign To -} !Symbol
    | LIRBinOp {- Left -} !Symbol {- Op -} !LIRBinOp {- Right -} !Symbol {- Assign To -} !Symbol
    | LIRCall {- Function -} !LIRLabel {- Args -} ![Symbol] {- Assign To -} !Symbol
    | LIRJump {- To -} !LIRLabel
    | LIRJumpC {- To -} !LIRLabel {- Left -} !Symbol {- Condition -} !Condition {- Right -} !Symbol
    deriving (Show, Eq, Ord)

data LIRAssembly = LIRAsmLabel !LIRLabel | LIRAsmInstruction !LIRInstruction
    deriving (Show, Eq)

data LIRFunction = LIRFunction
    { _lirFunctionLocals :: [Symbol]
    , _lirFunctionArgs   :: [Symbol]
    , _lirFunctionReturnSym :: Symbol
    , _lirFunctionInstructions :: [LIRAssembly]
    } deriving (Show, Eq)

data LIRProgram = LIRProgram
    { _lirProgramMain :: LIRFunction
    , _lirProgramFuns :: Map LIRLabel LIRFunction
    } deriving (Show, Eq)

makeBaseFunctor ''LIRLit
makeBaseFunctor ''LIRInstruction

makePrisms ''LIRInstruction
makePrisms ''LIRAssembly
makeLenses ''LIRFunction
makeLenses ''LIRProgram

lirFunctionSymbols :: Getter LIRFunction [Symbol]
lirFunctionSymbols = to f
    where
        f fun = fun^.lirFunctionLocals ++ fun^.lirFunctionArgs ++ [fun^.lirFunctionReturnSym]

data LIRLabelGen (m :: * -> *) a where
    NewLabel :: LIRLabelGen m LIRLabel

makeSem ''LIRLabelGen

runLIRLabelGen :: Sem (LIRLabelGen ': r) a -> Sem r a
runLIRLabelGen = evalState @Int 0 . reinterpret \case
    NewLabel -> do
        n <- get
        put (n + 1)
        pure $ LIRLabel n


