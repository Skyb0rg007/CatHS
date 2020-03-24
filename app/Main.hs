{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Main where

import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Control.Monad.Fix
import qualified Data.Map as Map

import Cat.X64 as Asm
import Cat.Backend
import Cat.Common
import Cat.LIR

prog :: Text
prog = compileProgram $
    LIRProgram
        { _lirProgramMain = mainFn
        , _lirProgramFuns = Map.singleton addFnLbl addFn
                         <> Map.singleton factorialFnLbl factorialFn
        }
    where
        a = Symbol 0
        b = Symbol 1
        c = Symbol 2
        d = Symbol 3
        e = Symbol 4
        f = Symbol 5
        addFnLbl = LIRLabel 0
        addFn = LIRFunction
            { _lirFunctionArgs = [d, e]
            , _lirFunctionReturnSym = f
            , _lirFunctionLocals = []
            , _lirFunctionInstructions =
                [ LIRAsmInstruction $ LIRBinOp d OpAdd e f
                ]
            } 
        mainFn :: LIRFunction
        mainFn = LIRFunction
            { _lirFunctionArgs = []
            , _lirFunctionReturnSym = c
            , _lirFunctionLocals = [a, b]
            , _lirFunctionInstructions =
                [ LIRAsmInstruction $ LIRAssignLit (LitInt 3) a
                , LIRAsmInstruction $ LIRAssignLit (LitInt 4) b
                , LIRAsmInstruction $ LIRCall addFnLbl [a, b] c
                , LIRAsmInstruction $ LIRCall factorialFnLbl [c] c
                , LIRAsmInstruction $ LIRCall LIRLabelPrintlnInt [c] c
                , LIRAsmInstruction $ LIRAssignLit (LitInt 0) c
                ]
            }
        g = Symbol 6
        h = Symbol 7
        i = Symbol 8
        beginning = LIRLabel 1
        end = LIRLabel 2
        factorialFnLbl = LIRLabel 3
        factorialFn :: LIRFunction
        factorialFn = LIRFunction
            { _lirFunctionArgs = [g]
            , _lirFunctionReturnSym = h
            , _lirFunctionLocals = [i]
            , _lirFunctionInstructions =
                [ LIRAsmInstruction $ LIRAssignLit (LitInt 1) h
                , LIRAsmInstruction $ LIRAssignLit (LitInt 1) i
                , LIRAsmLabel beginning
                -- if (g <= 1) return h
                , LIRAsmInstruction $ LIRJumpC end g LE i
                -- h = g * h
                , LIRAsmInstruction $ LIRBinOp g OpMult h h
                -- g = g - 1
                , LIRAsmInstruction $ LIRBinOp g OpSub i g
                -- goto beginning
                , LIRAsmInstruction $ LIRJump beginning
                , LIRAsmLabel end
                ]
            }

main :: IO ()
main = Text.IO.putStrLn prog
