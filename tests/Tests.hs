module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Hspec

import qualified Day01Spec

main = hspec spec

spec :: Spec
spec = do
    describe "Day01" Day01Spec.spec
