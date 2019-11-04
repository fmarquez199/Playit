module CollectionTypesSpec where

import Test.Hspec
import Lexer
import Utils (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  
  it "accepts `~un bUEn string 123$%#$%#$@%&*()~` as a valid token for strings" $ do
    tokens <- alexScanTokens "~un bUEn string 123$%#$%#$@%&*()~"

    case tokens of
      (TkRUNES tk _) -> tk `shouldBe` "~un bUEn string 123$%#$%#$@%&*()~"
      _ -> error "rejected as an invalid token"    

  it "accepts `>-chest` as a valid token for arrays" $ do
    let x = ">-chest"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArray
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `<$` as a valid token for array access (open)" $ do
    let x = "<$"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArrayOpen
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `$>` as a valid token for array access (close)" $ do
    let x = "$>"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkArrayClose
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `armor` as a valid token for comments" $ do
    let x = "armor"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSet
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `{$` as a valid token for comments" $ do
    let x = "{$"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSetOpen
      Nothing ->
        error "rejected as an invalid token"

  it "accepts `$}` as a valid token for comments" $ do
    let x = "$}"
    s <- scanTokens x
    case s of
      Just toks -> do
        let atok = getAbstractToken $ head toks
        atok `shouldBe` TkSetClose
      Nothing ->
        error "rejected as an invalid token"
