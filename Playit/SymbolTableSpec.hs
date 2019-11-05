-- file test/Playit/SymbolTableSpec.hs
module Playit.SymbolTableSpec (spec) where

import Test.Hspec
import Playit.SymbolTable

spec :: Spec
spec = do
  describe "createInitSymTab" $ do
    it "do something" $ do
      pending