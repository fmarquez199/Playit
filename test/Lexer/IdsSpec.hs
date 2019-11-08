module IdsSpec where

import Test.Hspec
import Playit.Lexer

spec :: Spec
spec = describe "Lexer.ids" $ do
  
  describe "program" $ do
    it "accepts `%cool_program%` as a valid program name" $ do
      let tokens = alexScanTokens "%cool_program%"

      case tokens of
        [TkProgramName name _] -> name `shouldBe` "%cool_program%"
        _ -> error "rejected as an invalid program name"
      
  describe "id" $ do
    it "accepts `var_1` as a valid id" $ do
      let tokens = alexScanTokens "var_1"

      case tokens of
        [TkID tk _] -> tk `shouldBe` "var_1"
        _ -> error "rejected as an invalid id"
      
    it "accepts `var_1'` as a valid id" $ do
      let tokens = alexScanTokens "var_1'"

      case tokens of
        [TkID tk _] -> tk `shouldBe` "var_1'"
        _ -> error "rejected as an invalid id"
      
  describe "typesId" $ do
    it "accepts `Potions` as a valid id for defined types" $ do
      let tokens = alexScanTokens "Potions"

      case tokens of
        [TkIDType tk _] -> tk `shouldBe` "Potions"
        _ -> error "rejected as an invalid id for defined types"
      
    it "accepts `Armors` as a valid id for defined types" $ do
      let tokens = alexScanTokens "Armors"

      case tokens of
        [TkIDType tk _] -> tk `shouldBe` "Armors"
        _ -> error "rejected as an invalid id for defined types"
      
    it "accepts `Weapons` as a valid id for defined types" $ do
      let tokens = alexScanTokens "Weapons"

      case tokens of
        [TkIDType tk _] -> tk `shouldBe` "Weapons"
        _ -> error "rejected as an invalid id for defined types"
      
