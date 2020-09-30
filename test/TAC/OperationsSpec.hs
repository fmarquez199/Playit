module OperationsSpec where

import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "Arithmetic Operations" $ do
    it "calculates 3 + 4" $
      runTestForValidTAC intAddition intAddition'

    it "calculates 3 - 4" $
      runTestForValidTAC intSubstraction intSubstraction'

    it "calculates 3 * 4" $
      runTestForValidTAC intMultiplication intMultiplication'

    it "calculates 3 / 4" $
      runTestForValidTAC intsDivision intsDivision'

    it "calculates 3 // 4" $
      runTestForValidTAC intDivision intDivision'

    it "calculates 3 % 4" $
      runTestForValidTAC intMod intMod'

    it "calculates -(-3)" $
      pendingWith "Thinking about how write this test"

    it "calculates 3'0 + 4'0" $
      runTestForValidTAC floatAddition floatAddition'

    it "calculates 3'0 - 4'0" $
      runTestForValidTAC floatSubstraction floatSubstraction'

    it "calculates 3'0 * 4'0" $
      runTestForValidTAC floatMultiplication floatMultiplication'

    it "calculates 3'0 / 4'0" $
      runTestForValidTAC floatDivision floatDivision'

    it "calculates -(-3'0)" $
      pendingWith "Thinking about how write this test"

  describe "Boolean operations" $ do
    it "calculates Win && Lose" $
      runTestForValidTAC conjuction conjuction'

    it "calculates Win || Lose" $
      runTestForValidTAC disjunction disjunction'
    
    it "calculates !Win" $
      pendingWith "Thinking about how write this test"
  
  describe "Artihmethic Comparations" $ do
    it "calculates 3 < 4" $
      runTestForValidTAC intLessThan intLessThan'

    it "calculates 3 <= 4" $
      runTestForValidTAC intLessEq intLessEq'

    it "calculates 3 == 4" $
      runTestForValidTAC intEq intEq'

    it "calculates 3 != 4" $
      runTestForValidTAC intNotEq intNotEq'

    it "calculates 3 > 4" $
      runTestForValidTAC intGreaterThan intGreaterThan'

    it "calculates 3 >= 4" $
      runTestForValidTAC intGreaterEq intGreaterEq'

    it "calculates 3'0 < 4'0" $
      runTestForValidTAC floatLessThan floatLessThan'

    it "calculates 3'0 <= 4'0" $
      runTestForValidTAC floatLessEq floatLessEq'

    it "calculates 3'0 == 4'0" $
      runTestForValidTAC floatEq floatEq'

    it "calculates 3'0 != 4'0" $
      runTestForValidTAC floatNotEq floatNotEq'

    it "calculates 3'0 > 4'0" $
      runTestForValidTAC floatGreaterThan floatGreaterThan'

    it "calculates 3'0 >= 4'0" $
      runTestForValidTAC floatGreaterEq floatGreaterEq'

  describe "Char Operations" $ do
    it "calculates ^*x*" $
      runTestForValidTAC upperCase upperCase'
    
    it "calculates .*x*" $
      runTestForValidTAC lowerCase lowerCase'
    
    it "calculates *x* < *y*" $
      runTestForValidTAC charLessThan charLessThan'

    it "calculates *x* <= *y*" $
      runTestForValidTAC charLessEq charLessEq'

    it "calculates *x* == *y*" $
      runTestForValidTAC charEq charEq'

    it "calculates *x* != *y*" $
      runTestForValidTAC charNotEq charNotEq'

    it "calculates *x* > *y*" $
      runTestForValidTAC charGreaterThan charGreaterThan'

    it "calculates *x* >= *y*" $
      runTestForValidTAC charGreaterEq charGreaterEq'
