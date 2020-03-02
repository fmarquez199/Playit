module OperatorsSpec where

import Test.Hspec
import Playit.FontEnd.Lexer

spec :: Spec
spec = describe "Lexer.operators" $ do
  
  describe "numeric" $ do
    it "accepts `5 + 3 * 10` as a valid plus and mult operators" $ do
        let tokens = alexScanTokens "5 + 3 * 10"

        case tokens of
          [TkINT _ _ _,TkADD op1 _,TkINT _ _ _,TkMULT op2 _,TkINT _ _ _] ->
            op1 ++ op2 `shouldBe` "+*"
          _ -> error "rejected as invalid operators"

    it "accepts `7 - 2 / 5` as a valid minus and div operators" $ do
        let tokens = alexScanTokens "7 - 2 / 5"

        case tokens of
          [TkINT _ _ _,TkMIN op1 _,TkINT _ _ _,TkDIV op2 _,TkINT _ _ _] ->
            op1 ++ op2 `shouldBe` "-/"
          _ -> error "rejected as invalid operators"

    it "accepts `//` as a valid integer div operator" $ do
        let tokens = alexScanTokens "//"

        case tokens of
          [TkDivEntera op _] -> op `shouldBe` "//"
          _ -> error "rejected as an invalid operator"

    it "accepts `%` as a valid mod operator" $ do
        let tokens = alexScanTokens "%"

        case tokens of
          [TkMOD op _] -> op `shouldBe` "%"
          _ -> error "rejected as an invalid operator"

    it "accepts `#` as a valid length operator" $ do
        let tokens = alexScanTokens "#"

        case tokens of
          [TkLEN op _] -> op `shouldBe` "#"
          _ -> error "rejected as an invalid operator"

    it "accepts `a++` as a valid increment operator" $ do
        let tokens = alexScanTokens "a++"

        case tokens of
          [TkID _ _, TkINCREMENT op _] -> op `shouldBe` "++"
          _ -> error "rejected as an invalid operator"

    it "accepts `b--` as a valid decrement operator" $ do
        let tokens = alexScanTokens "b--"

        case tokens of
          [TkID _ _, TkDECREMENT op _] -> op `shouldBe` "--"
          _ -> error "rejected as an invalid operator"

  describe "comparison" $ do
    it "accepts `a || b && c` as a valid disjuction and conjuction operators" $ do
        let tokens = alexScanTokens "a || b && c"

        case tokens of
          [TkID _ _,TkOR op1 _,TkID _ _,TkAND op2 _,TkID _ _] ->
            op1 ++ op2 `shouldBe` "||&&"
          _ -> error "rejected as an invalid operator"

    it "accepts `a <= b == c` as a valid less equal and equal operators" $ do
        let tokens = alexScanTokens "a <= b == c"

        case tokens of
          [TkID _ _,TkLessEqual op1 _,TkID _ _,TkEQUAL op2 _,TkID _ _] ->
            op1 ++ op2 `shouldBe` "<==="
          _ -> error "rejected as invalid operators"

    it "accepts `a >= b < c` as a valid greater equal and less than operators" $ do
        let tokens = alexScanTokens "a >= b < c"

        case tokens of
          [TkID _ _,TkGreaterEqual op1 _,TkID _ _,TkLessThan op2 _,TkID _ _] ->
            op1 ++ op2 `shouldBe` ">=<"
          _ -> error "rejected as invalid operators"

    it "accepts `a > b != c` as a valid greater than and not equal operators" $ do
        let tokens = alexScanTokens "a > b != c"

        case tokens of
          [TkID _ _,TkGreaterThan op1 _,TkID _ _,TkNotEqual op2 _,TkID _ _] ->
            op1 ++ op2 `shouldBe` ">!="
          _ -> error "rejected as invalid operators"

    it "accepts `!` as a valid not operator" $ do
        let tokens = alexScanTokens "!"

        case tokens of
          [TkNOT op _] -> op `shouldBe` "!"
          _ -> error "rejected as an invalid operator"

  describe "chars" $ do
    it "accepts `^` as a valid uppercase operator" $ do
        let tokens = alexScanTokens "^"

        case tokens of
          [TkUPPER op _] -> op `shouldBe` "^"
          _ -> error "rejected as an invalid operator"

    it "accepts `.` as a valid loercase operator" $ do
        let tokens = alexScanTokens "."

        case tokens of
          [TkLOWER op _] -> op `shouldBe` "."
          _ -> error "rejected as an invalid operator"

  describe "referentiation" $ do
    it "accepts `?` as a valid referentiation operator" $ do
        let tokens = alexScanTokens "?"

        case tokens of
          [TkREF op _] -> op `shouldBe` "?"
          _ -> error "rejected as an invalid operator"
