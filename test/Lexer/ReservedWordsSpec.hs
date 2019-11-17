module ReservedWordsSpec where

import Test.Hspec
import Playit.Lexer

spec :: Spec
spec = describe "Lexer.reservedWords" $ do
  
  describe "definitionOfTheProgramBegin" $ do
    it "accepts `world` as a valid definition of the program begin" $ do
      let tokens = alexScanTokens "world"

      case tokens of
        [TkWORLD tk _] -> tk `shouldBe` "world"
        _ -> error "rejected as a definition of the program begin"
      
  describe "simpleTypes" $ do
    it "accepts `Battle` as a valid simple type" $ do
      let tokens = alexScanTokens "Battle"

      case tokens of
        [TkBATTLE tk _] -> tk `shouldBe` "Battle"
        _ -> error "rejected as a simple type"
      
    it "accepts `Power` as a valid simple type" $ do
      let tokens = alexScanTokens "Power"

      case tokens of
        [TkPOWER tk _] -> tk `shouldBe` "Power"
        _ -> error "rejected as a simple type"
      
    it "accepts `Skill` as a valid simple type" $ do
      let tokens = alexScanTokens "Skill"

      case tokens of
        [TkSKILL tk _] -> tk `shouldBe` "Skill"
        _ -> error "rejected as a simple type"
      
    it "accepts `Rune` as a valid simple type" $ do
      let tokens = alexScanTokens "Rune"

      case tokens of
        [TkRUNE tk _] -> tk `shouldBe` "Rune"
        _ -> error "rejected as a simple type"
      
    it "accepts `Runes` as a valid simple type" $ do
      let tokens = alexScanTokens "Runes"

      case tokens of
        [TkRUNES tk _] -> tk `shouldBe` "Runes"
        _ -> error "rejected as a simple type"
      
  describe "compoundTypes" $ do
    it "accepts `Kit of` as a valid compund type" $ do
      let tokens = alexScanTokens "Kit of"

      case tokens of
        [TkKitOf tk _] -> tk `shouldBe` "Kit of"
        _ -> error "rejected as a compund type"
      
    it "accepts `Inventory` as a valid compund type" $ do
      let tokens = alexScanTokens "Inventory"

      case tokens of
        [TkINVENTORY tk _] -> tk `shouldBe` "Inventory"
        _ -> error "rejected as a compund type"
      
    it "accepts `Items` as a valid compund type" $ do
      let tokens = alexScanTokens "Items"

      case tokens of
        [TkITEMS tk _] -> tk `shouldBe` "Items"
        _ -> error "rejected as a compund type"
      
    it "accepts `spawn` as a valid compund type" $ do
      let tokens = alexScanTokens "spawn"

      case tokens of
        [TkSPAWN tk _] -> tk `shouldBe` "spawn"
        _ -> error "rejected as a compund type"
      
    it "accepts `summon` as a valid compund type" $ do
      let tokens = alexScanTokens "summon"

      case tokens of
        [TkSUMMON tk _] -> tk `shouldBe` "summon"
        _ -> error "rejected as a compund type"
      
  describe "if" $ do
    it "accepts `Button` as a valid if statement" $ do
      let tokens = alexScanTokens "Button"

      case tokens of
        [TkBUTTON tk _] -> tk `shouldBe` "Button"
        _ -> error "rejected as a if statement"
      
    it "accepts `notPressed` as a valid if statement" $ do
      let tokens = alexScanTokens "notPressed"

      case tokens of
        [TkNotPressed tk _] -> tk `shouldBe` "notPressed"
        _ -> error "rejected as a if statement"
      
  describe "subroutines" $ do
    it "accepts `boss` as a valid procedure definition" $ do
      let tokens = alexScanTokens "boss"

      case tokens of
        [TkBOSS tk _] -> tk `shouldBe` "boss"
        _ -> error "rejected as a procedure definition"
      
    it "accepts `monster` as a valid function definition" $ do
      let tokens = alexScanTokens "monster"

      case tokens of
        [TkMONSTER tk _] -> tk `shouldBe` "monster"
        _ -> error "rejected as a function definition"
      
    it "accepts `kill` as a valid subroutine call" $ do
      let tokens = alexScanTokens "kill"

      case tokens of
        [TkKILL tk _] -> tk `shouldBe` "kill"
        _ -> error "rejected as a subroutine call"
      
    it "accepts `unlock` as a valid return value for functions" $ do
      let tokens = alexScanTokens "unlock"

      case tokens of
        [TkUNLOCK tk _] -> tk `shouldBe` "unlock"
        _ -> error "rejected as a return value for functions"
      
  describe "iterations" $ do
    it "accepts `controller` as a valid determined iteration" $ do
      let tokens = alexScanTokens "controller"

      case tokens of
        [TkCONTROLLER tk _] -> tk `shouldBe` "controller"
        _ -> error "rejected as a determined iteration"
      
    it "accepts `play` as a valid indetermined iteration" $ do
      let tokens = alexScanTokens "play"

      case tokens of
        [TkPLAY tk _] -> tk `shouldBe` "play"
        _ -> error "rejected as a indetermined iteration"
      
    it "accepts `lock` as a valid indetermined iteration" $ do
      let tokens = alexScanTokens "lock"

      case tokens of
        [TkLOCK tk _] -> tk `shouldBe` "lock"
        _ -> error "rejected as a indetermined iteration"
      
    it "accepts `gameOver` as a valid break iteration" $ do
      let tokens = alexScanTokens "gameOver"

      case tokens of
        [TkGameOver tk _] -> tk `shouldBe` "gameOver"
        _ -> error "rejected as a break iteration"
      
    it "accepts `keepPlaying` as a valid continue with iteration" $ do
      let tokens = alexScanTokens "keepPlaying"

      case tokens of
        [TkKeepPlaying tk _] -> tk `shouldBe` "keepPlaying"
        _ -> error "rejected as a continue with iteration"
      
  describe "io" $ do
    it "accepts `joystick` as a valid input from console" $ do
      let tokens = alexScanTokens "joystick"

      case tokens of
        [TkJOYSTICK tk _] -> tk `shouldBe` "joystick"
        _ -> error "rejected as a input from console"
      
    it "accepts `drop` as a valid out to console" $ do
      let tokens = alexScanTokens "drop"

      case tokens of
        [TkDROP tk _] -> tk `shouldBe` "drop"
        _ -> error "rejected as a out to console"
      
  describe "pointers" $ do
    it "accepts `DeathZone` as a valid null pointer" $ do
      let tokens = alexScanTokens "DeathZone"

      case tokens of
        [TkDeathZone tk _] -> tk `shouldBe` "DeathZone"
        _ -> error "rejected as a null pointer"
      
    it "accepts `free` as a valid free memory" $ do
      let tokens = alexScanTokens "free"

      case tokens of
        [TkFREE tk _] -> tk `shouldBe` "free"
        _ -> error "rejected as a free memory"
      
    it "accepts `puff` as a valid desrefentiation of variable" $ do
      let tokens = alexScanTokens "puff"

      case tokens of
        [TkPUFF tk _] -> tk `shouldBe` "puff"
        _ -> error "rejected as a desrefentiation of variable"
      

