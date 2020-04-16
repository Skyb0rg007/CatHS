{-# LANGUAGE OverloadedStrings #-}

module ParseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Text (Text)
import Text.Megaparsec
import Cat.Parse
import Cat.Syntax
import Cat.Common

main :: IO ()
main = hspec spec

p :: Text -> Program
p txt =
    case parse parseProgram "" txt of
      Left err -> error (errorBundlePretty err)
      Right prog -> prog

spec :: Spec
spec =
    describe "Parses correctly" $ do
        let mult = flip ExpInfix SOpMult
            add  = flip ExpInfix SOpAdd
            sub  = flip ExpInfix SOpSub
            div  = flip ExpInfix SOpDiv
            gt   = flip ExpInfix SOpGt
            or   = flip ExpInfix SOpOr
        it "Example 1" $
            p "function main () -> int { -(9 + 10 * 10 - (9/10)) }"
            `shouldBe`
            [FunDec "main" "int" []
            (ExpNegate (ExpSequence
                [sub (add (ExpIntLit 9)
                          (mult (ExpIntLit 10)
                                (ExpIntLit 10)))
                     (ExpSequence [div (ExpIntLit 9)
                                       (ExpIntLit 10)])]))]
        it "Example 2" $
            p "type intArray = array of int                     \n\
              \function main () -> int {                        \n\
              \  let var a : intArray := intArray [10] of 20 in \n\
              \    a[9]                                         \n\
              \  end                                            \n\
              \}"
            `shouldBe`
            [TyDecArray "intArray" "int"
            , FunDec "main" "int" []
            (ExpLet
                [VarDec "a" "intArray" (ExpArrayCreate "intArray" (ExpIntLit 10) (ExpIntLit 20))]
                (ExpLValue (LValueSubscript (LValueId "a") (ExpIntLit 9))))
            ]

        it "Example 3" $
            p "function main () -> int {   \n\
              \  let var a : int := 40 in  \n\
              \    (a > 50 or (a:= 10; 0); \n\
              \    a)                      \n\
              \  end                       \n\
              \}"
            `shouldBe`
            [FunDec "main" "int" []
            (ExpLet
                [VarDec "a" "int" (ExpIntLit 40)]
                (ExpSequence [or (gt (ExpLValue (LValueId "a"))
                                     (ExpIntLit 50))
                                 (ExpSequence [ExpAssign (LValueId "a") (ExpIntLit 10)
                                              , ExpIntLit 0])
                             , ExpLValue (LValueId "a")]))]

