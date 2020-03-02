module LiteralsSpec where

import Test.Hspec
import Playit.FontEnd.Lexer

spec :: Spec
spec = describe "Lexer.literals" $ do
  
  describe "chars" $ do
    it "accepts `*a*` as a valid char literal" $ do
      let tokens = alexScanTokens "*a*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*a*"
        _ -> error $ show tokens ++ "rejected as a char literal"
      
    it "accepts `*b*` as a valid char literal" $ do
      let tokens = alexScanTokens "*b*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*b*"
        _ -> error $ show tokens ++ "rejected as a char literal"

    it "accepts `*\\n*` as a valid char literal" $ do
      let tokens = alexScanTokens "*\n*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\n*"
        _ -> error $ show tokens ++ "rejected as a char literal"

    it "accepts `*\\0*` as a valid char literal" $ do
      let tokens = alexScanTokens "*\0*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\0*"
        _ -> error $ show tokens ++ "rejected as a char literal"

    it "accepts `*\\\\*` as a valid char literal" $ do
      let tokens = alexScanTokens "*\\\\*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\\\\*"
        _ -> error $ show tokens ++ "rejected as a char literal"

    it "accepts `*\\t*` as a valid char literal" $ do
      let tokens = alexScanTokens "*\t*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\t*"
        _ -> error $ show tokens ++ "rejected as a char literal"

    it "accepts `*\\~*` as a valid char literal" $ do
      let tokens = alexScanTokens "*\\~*"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\\~*"
        _ -> error $ show tokens ++ "rejected as a char literal"

    it "accepts `*\\**` as a valid char literal" $ do
      let tokens = alexScanTokens "*\\**"

      case tokens of
        [TkCHARACTER char _ _] -> char `shouldBe` "*\\**"
        _ -> error $ show tokens ++ "rejected as a char literal"

  describe "strings" $ do
    it "accepts `~Hell0 <\"#$%?&/(=){+}[-]'_.,;^`> w@rld!!~` as a valid string literal" $ do
      let tokens = alexScanTokens "~Hell0 <\"#$%?&/(=){+}[-]'_.,;^`> w@rld!!~"

      case tokens of
        [TkSTRINGS str _ _] -> str `shouldBe` "~Hell0 <\"#$%?&/(=){+}[-]'_.,;^`> w@rld!!~"
        _ -> error $ show tokens ++ "rejected as a string literal"

    it "accepts `~Un str1ng b!3n c0mple7@ \"#$%&/()=?'*+<>^{}[]`_.,;~` as a valid string literal" $ do
      -- let tokens = alexScanTokens "~Un str1ng b!3n c0mple7@ \"#$%&/()=?'*+<>^{}[]`_.,;~"

      -- case tokens of
      --   [TkSTRINGS str _] -> str `shouldBe` "~Un str1ng b!3n c0mple7@ \"#$%&/()=?'*+<>^{}[]`_.,;~"
      --   _ -> error $ show tokens ++ "rejected as a string literal"
      pendingWith "Fix syntax to get accepted. Order??"

    it "rejects `~*~\\~` as a valid string literal" $ do
      -- let tokens = alexScanTokens "~*~\\~"

      -- case tokens of
      --   [TkError err _] -> err `shouldBe` "~*~\\~"
      --   _ -> error $ show tokens ++ "this string should not pass"
      pendingWith "Fix syntax to get accepted"

  describe "numbers" $ do
    it "accepts `0000004` as a valid integer literal" $ do
      let tokens = alexScanTokens "0000004"

      case tokens of
        [TkINT num _ _] -> num `shouldBe` "0000004"
        _ -> error "rejected as an integer literal"
  
    it "accepts `09029043856234700` as a valid integer literal" $ do
      let tokens = alexScanTokens "09029043856234700"

      case tokens of
        [TkINT num _ _] -> num `shouldBe` "09029043856234700"
        _ -> error "rejected as an integer literal"
  
    it "accepts `8734'8346` as a valid float literal" $ do
      let tokens = alexScanTokens "8734'8346"

      case tokens of
        [TkFLOAT num _ _] -> num `shouldBe` "8734'8346"
        _ -> error "rejected as a float literal"
  
    it "accepts `0192823'0987` as a valid float literal" $ do
      let tokens = alexScanTokens "0192823'0987"

      case tokens of
        [TkFLOAT num _ _] -> num `shouldBe` "0192823'0987"
        _ -> error "rejected as a float literal"
  
  describe "booleans" $ do
    it "accepts `Win` as a valid boolean literal" $ do
      let tokens = alexScanTokens "Win"

      case tokens of
        [TkWIN bool _] -> bool `shouldBe` "Win"
        _ -> error "rejected as a boolean literal"

    it "accepts `Lose` as a valid boolean literal" $ do
      let tokens = alexScanTokens "Lose"

      case tokens of
        [TkLOSE bool _] -> bool `shouldBe` "Lose"
        _ -> error "rejected as a boolean literal"

