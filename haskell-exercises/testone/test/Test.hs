import Test.Hspec    (describe, hspec, it, shouldBe)

import TestOne (hello)

main :: IO ()
main = hspec $ do
  describe "TestOne" $ do
    it "test" $ do
      hello `shouldBe` "Hello, world!"