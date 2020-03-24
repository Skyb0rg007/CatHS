
module X64Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Compilation" $ do
        it "Compiles" $
            
            
