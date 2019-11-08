module LiteralsSpec where

import Test.Hspec
import Playit.Lexer

spec :: Spec
spec = describe "Lexer.literals" $ do
  
  decribe "chars" $ do
    it "accepts `*a*` as a valid char literal" $ do
      tokens <- alexScanTokens "*a*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*a*"
        _ -> error "rejected as a char literal"
      
    it "accepts `*b*` as a valid char literal" $ do
      tokens <- alexScanTokens "*b*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*b*"
        _ -> error "rejected as a char literal"

    it "accepts `*\n*` as a valid char literal" $ do
      tokens <- alexScanTokens "*\n*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\n*"
        _ -> error "rejected as a char literal"

    it "accepts `*\0*` as a valid char literal" $ do
      tokens <- alexScanTokens "*\0*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\0*"
        _ -> error "rejected as a char literal"

    it "accepts `*\\*` as a valid char literal" $ do
      tokens <- alexScanTokens "*\\*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\\*"
        _ -> error "rejected as a char literal"

    it "accepts `*\t*` as a valid char literal" $ do
      tokens <- alexScanTokens "*\t*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\t*"
        _ -> error "rejected as a char literal"

    it "accepts `*\\~*` as a valid char literal" $ do
      tokens <- alexScanTokens "*\\~*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\\~*"
        _ -> error "rejected as a char literal"

    it "accepts `*\**` as a valid char literal" $ do
      tokens <- alexScanTokens "*\**"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\**"
        _ -> error "rejected as a char literal"

  decribe "strings" $ do
    it "accepts `~Un str1ng b!3n c0mplet@ !\"#$%&/()=?\'*+<>^{}[]`_.,;.~` as a valid string literal" $ do
      tokens <- alexScanTokens "accepts `~Un str1ng b!3n c0mplet@ !\"#$%&/()=?\'*+<>^{}[]`_.,;.~"

      case tokens of
        [TkSTRINGS str _] -> str `shouldBe` "accepts `~Un str1ng b!3n c0mple7@ \"#$%&/()=?\'*+<>^{}[]`_.,;.~"
        _ -> error "rejected as a string literal"

    it "rejects `~*~\\~` as a valid string literal" $ do
      tokens <- alexScanTokens "~*~\\~"

      case tokens of
        [TkError err _] -> err `shouldBe` "~*~\\~"
        _ -> error "this string should not pass"

  decribe "numbers" $ do
    it "accepts `0000004` as a valid integer literal" $ do
      tokens <- alexScanTokens "0000004"

      case tokens of
        [TkINT num _ _] -> num `shouldBe` "0000004"
        _ -> error "rejected as an integer literal"
  
    it "accepts `09029043856234700` as a valid integer literal" $ do
      tokens <- alexScanTokens "09029043856234700"

      case tokens of
        [TkINT num _ _] -> num `shouldBe` "09029043856234700"
        _ -> error "rejected as an integer literal"
  
    it "accepts `8734'8346` as a valid float literal" $ do
      tokens <- alexScanTokens "8734'8346"

      case tokens of
        [TkFLOAT num _ _] -> num `shouldBe` "8734'8346"
        _ -> error "rejected as a float literal"
  
    it "accepts `0192823'0987` as a valid float literal" $ do
      tokens <- alexScanTokens "0192823'0987"

      case tokens of
        [TkFLOAT num _ _] -> num `shouldBe` "0192823'0987"
        _ -> error "rejected as a float literal"
  
  decribe "booleans" $ do
    it "accepts `Win` as a valid boolean literal" $ do
      tokens <- alexScanTokens "Win"

      case tokens of
        [TkWIN bool _] -> bool `shouldBe` "Win"
        _ -> error "rejected as a boolean literal"

    it "accepts `Lose` as a valid boolean literal" $ do
      tokens <- alexScanTokens "Lose"

      case tokens of
        [TkLOSE bool _] -> bool `shouldBe` "Lose"
        _ -> error "rejected as a boolean literal"

