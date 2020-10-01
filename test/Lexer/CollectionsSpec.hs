module CollectionsSpec where

import Test.Hspec
import Playit.FontEnd.Lexer

spec :: Spec
spec = describe "Lexer.collections" $ do
  
  describe "lists" $ do
    it "accepts `<< >>` as a valid list" $ do
      let tokens = alexScanTokens "<< >>"

      case tokens of
        [TkOpenList tk1 _, TkCloseList tk2 _] -> tk1 ++ tk2 `shouldBe` "<<>>"
        _ -> error "rejected as an invalid list"

    it "accepts `|> <|` as a valid list indexation" $ do
      let tokens = alexScanTokens "|> <|"

      case tokens of
        [TkOpenListIndex tk1 _, TkCloseListIndex tk2 _] ->
          tk1 ++ tk2 `shouldBe` "|><|"
        _ -> error "rejected as an invalid list indexation"

    it "accepts `:` as a valid anexo elem to list" $ do
      let tokens = alexScanTokens ":"

      case tokens of
        [TkANEXO tk _] -> tk `shouldBe` ":"
        _ -> error "rejected as an invalid anexo elem to list"

    it "accepts `::` as a valid concat two lists" $ do
      let tokens = alexScanTokens "::"

      case tokens of
        [TkCONCAT tk _] -> tk `shouldBe` "::"
        _ -> error "rejected as an invalid concat two lists"

  describe "arrays" $ do
    it "accepts `|} {|` as a valid array" $ do
      let tokens = alexScanTokens "|} {|"

      case tokens of
        [TkOpenArray tk1 _, TkCloseArray tk2 _] -> tk1 ++ tk2 `shouldBe` "|}{|"
        _ -> error "rejected as an invalid array"

    it "accepts `|) (|` as a valid array indexation" $ do
      let tokens = alexScanTokens "|) (|"

      case tokens of
        [TkOpenArrayIndex tk1 _, TkCloseArrayIndex tk2 _] ->
          tk1 ++ tk2 `shouldBe` "|)(|"
        _ -> error "rejected as an invalid array indexation"

  describe "registersUnionsInit" $ do
    it "accepts `{ }` as a valid register / union initialization" $ do
      let tokens = alexScanTokens "{ }"

      case tokens of
        [TkOpenBrackets tk1 _, TkCloseBrackets tk2 _] ->
          tk1 ++ tk2 `shouldBe` "{}"
        _ -> error "rejected as an invalid register / union initialization"

