-- file test/Playit/CheckASTSpec.hs
module Playit.CheckASTSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "something" $ do
    it "do something" $ do
      pending