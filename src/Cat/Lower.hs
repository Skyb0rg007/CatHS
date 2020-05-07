{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TypeApplications           #-}

module Cat.Lower
    ( module Cat.Lower
    ) where

import           Control.Lens.TH
import           Control.Monad.Identity
import           Data.Functor.Foldable.TH
import           Data.Int                 (Int64)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Polysemy
import           Polysemy.Fixpoint
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State

import           Cat.CheckedSyntax
import           Cat.Common
import           Cat.LIR

type Env = [(LIRLabel, LIRFunction)]

lower :: Program -> [(LIRLabel, LIRFunction)] -- LIRProgram
lower decls =
    runIdentity
  . runFinal
  . fixpointToFinal @Identity
  . runLIRLabelGen
  . runSymbolGen
  . runReader @Env []
  $ mdo
    env <- local (const env) $ traverse lowerDecl decls
    pure env    

lowerDecl
    :: Members '[Fixpoint, Reader Env, LIRLabelGen, SymbolGen] r 
    => TopLevelDec
    -> Sem r (LIRLabel, LIRFunction)
lowerDecl (FunDec name args body) = do
    (asm, (locals, retSym)) <- runOutputList . runOutputList $ lowerExp body
    lbl <- newLabel
    pure (lbl, LIRFunction locals args retSym asm)

lowerExp
    :: Members '[Reader Env, LIRLabelGen, SymbolGen, Output LIRAssembly, Output Symbol] r
    => Exp
    -> Sem r Symbol
lowerExp = \case
    ExpNegate e -> do
        inSym <- lowerExp e
        outSym <- newSymbol
        output $ LIRAsmInstruction $ LIRNegate inSym outSym
        pure outSym
    ExpInfix e1 op e2 -> do
        e1Sym <- lowerExp e1
        e2Sym <- lowerExp e2
        outSym <- newSymbol
        output outSym
        output $ LIRAsmInstruction $ LIRBinOp e1Sym op e2Sym outSym
        pure outSym
    ExpLValue (LValueId sym) -> do
        pure sym
    -- ExpLValue (LValueSubscript lv e) -> do
        -- lvSym <- lowerExp (ExpLValue lv)
        -- subscript <- lowerExp e
        -- outSym <- newSymbol
        -- undefined
    ExpSequence [] -> do
        outSym <- newSymbol
        output outSym
        output $ LIRAsmInstruction $ LIRNop
        output outSym
        pure outSym
    ExpSequence exps -> do
        last <$> traverse lowerExp exps
    _ -> undefined

