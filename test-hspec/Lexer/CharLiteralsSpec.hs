module CharLiteralsSpec where

import Test.Hspec
import Playit.Lexer

spec :: Spec
spec = describe "Lexer" $ do

  it "accepts `*a*` as a valid char literal" $ do
    tokens <- alexScanTokens "*a*"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*a*"
      _ -> error "rejected as an invalid token"
    
  it "accepts `*b*` as a valid char literal" $ do
    tokens <- alexScanTokens "*b*"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*b*"
      _ -> error "rejected as an invalid token"

  it "accepts `*\n*` as a valid char literal" $ do
    tokens <- alexScanTokens "*\n*"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*\n*"
      _ -> error "rejected as an invalid token"

  it "accepts `*\0*` as a valid char literal" $ do
    tokens <- alexScanTokens "*\0*"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*\0*"
      _ -> error "rejected as an invalid token"

  it "accepts `*\\*` as a valid char literal" $ do
    tokens <- alexScanTokens "*\\*"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*\\*"
      _ -> error "rejected as an invalid token"

  it "accepts `*\t*` as a valid char literal" $ do
    tokens <- alexScanTokens "*\t*"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*\t*"
      _ -> error "rejected as an invalid token"

  it "accepts `*\\~*` as a valid char literal" $ do
    tokens <- alexScanTokens "*\\~*"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*\\~*"
      _ -> error "rejected as an invalid token"

  it "accepts `*\**` as a valid char literal" $ do
    tokens <- alexScanTokens "*\**"

    case tokens of
      (TkRUNE tk _) -> tk `shouldBe` "*\**"
      _ -> error "rejected as an invalid token"

