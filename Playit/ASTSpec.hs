-- file test/Playit/ASTSpec.hs
module Playit.ASTSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "something" $ do
    it "do something" $ do
      pending