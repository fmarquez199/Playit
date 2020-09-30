module GeneralSpec where

import Test.Hspec
import Playit.FontEnd.Lexer

spec :: Spec
spec = describe "Lexer.general" $ do

  describe "determinedIterations" $ do
    it "accepts `<-` as a valid for each list / array" $ do
      let tokens = alexScanTokens "<-"

      case tokens of
        [TkIN tk _] -> tk `shouldBe` "<-"
        _ -> error "rejected as an invalid for each list / array"
      
    it "accepts `->` as a valid ranged iteration" $ do
      let tokens = alexScanTokens "->"

      case tokens of
        [TkTO tk _] -> tk `shouldBe` "->"
        _ -> error "rejected as an invalid ranged iteration"
      
  describe "guard" $ do
    it "accepts `|` as a valid guard" $ do
      let tokens = alexScanTokens "|"

      case tokens of
        [TkGUARD tk _] -> tk `shouldBe` "|"
        _ -> error "rejected as an invalid guard"
      
  describe "assig" $ do
    it "accepts `=` as a valid assignation" $ do
      let tokens = alexScanTokens "="

      case tokens of
        [TkASSIG tk _] -> tk `shouldBe` "="
        _ -> error "rejected as an invalid assignation"
      
  describe "expressions" $ do
    it "accepts `(,)` as valid expressions" $ do
      let tokens = alexScanTokens "(,)"

      case tokens of
        [TkOpenParenthesis tk1 _,TkCOMA tk2 _,TkCloseParenthesis tk3 _] ->
            tk1 ++ tk2 ++ tk3 `shouldBe` "(,)"
        _ -> error "rejected as invalid expressions"
      
  describe "comments" $ do
    it "accepts `\"''\"` as valid many lines comment" $ do
      -- let tokens = alexScanTokens "\"''\""

      -- case tokens of
      --   [TkOpenComments tk1 _,TkCloseComments tk2 _] -> tk1 ++ tk2 `shouldBe` "\"''\""
      --   _ -> error $ show tokens ++ "rejected as invalid many lines comment"
      pendingWith "Fix to get accepted"

    it "accepts `@` as valid single comment" $ do
      -- let tokens = alexScanTokens "@"

      -- case tokens of
      --   [TkCOMMENT tk _] -> tk `shouldBe` "@"
      --   _ -> error $ show tokens ++ "rejected as invalid single comment"
      pendingWith "Fix to get accepted"

      