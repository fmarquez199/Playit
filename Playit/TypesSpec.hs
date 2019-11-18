-- file test/Playit/TypesSpec.hs
module Playit.TypesSpec (spec) where

import Test.Hspec
import Playit.Types

spec :: Spec
spec = do
  describe "getRegName" $ do
  --   it "return empty string if empty list is passed as argument" $ do
  --     getRegName [] `shouldBe` ""

  --   it "return a name if the head of the list is a FromReg" $ do
  --     getRegName [FromReg "name"] `shouldBe` "name"

  --   it "what happen when ExtraInfo does not contain an elem FromReg" $ do
  --     pending -- Perhaps a ""?

  --   it "what happen when ExtraInfo does it contain more than one FromReg" $ do
  --     pending -- Should it explode?

  -- describe "getNParams" $ do
  --   it "return Nothing if empty list is passed as argument" $ do
  --     getNParams [] `shouldBe` Nothing

  --   it "return Just Int when a Params is in the list" $ do
  --     getNParams [Params ["p" ++ x | x <- [1..3]]] `shouldBe` (Just (3 :: Int))

    it "what happen when ExtraInfo does not contain an elem Params" $ do
      pending -- Perhaps a Nothing?

    it "What happen when ExtraInfo does it contain more than one Params" $ do
      pending -- Should it explode?