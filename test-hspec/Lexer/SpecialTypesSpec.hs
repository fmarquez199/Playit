module SpecialTypesSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `abyss` as a valid token for comments" $ do
    let x = "abyss"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkNull
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `arrow to` as a valid token for comments" $ do
    let x = "arrow to"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkPointer
      Nothing ->
        error "rejected as an invalid token"
