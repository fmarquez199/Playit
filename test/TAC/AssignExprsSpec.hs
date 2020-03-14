module AssignExprsSpec where

import Test.Hspec
import Utils
import Playit.FrontEnd.Types
import Playit.BackEnd.Utils                  (tacConstant, tacVariable)
import qualified Playit.BackEnd.TACType as T


spec :: Spec
spec = do
  describe "Assign simple types literals" $ do
    it "assigns boolean literals" $ do
      let p = "world %HolaMundo%:\n\
        \  Battle p = Win\n\
        \.~"
      runTestForValidTAC p [
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0" TBool 1 Constants ("lit",-1) []) (tacConstant ("True", TBool)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "p" TBool 1 Variables ("global",0) []) (tacVariable $ SymbolInfo "$t0" TBool 1 Constants ("lit",-1) []) Nothing]

    it "assigns integer literals" $ do
      let p = "world %HolaMundo%:\n\
        \  Power i = 1\n\
        \.~"
      runTestForValidTAC p [
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0" TInt 1 Constants ("lit",-1) []) (tacConstant ("1", TInt)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "i" TInt 1 Variables ("global",0) []) (tacVariable $ SymbolInfo "$t0" TInt 1 Constants ("lit",-1) []) Nothing]

    it "assigns floating literals" $ do
      let p = "world %HolaMundo%:\n\
        \  Skill f = 3'0\n\
        \.~"
      runTestForValidTAC p [
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0" TFloat 1 Constants ("lit",-1) []) (tacConstant ("3.0", TFloat)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "f" TFloat 1 Variables ("global",0) []) (tacVariable $ SymbolInfo "$t0" TFloat 1 Constants ("lit",-1) []) Nothing]
      
    it "assigns char literals" $ do
      let p = "world %HolaMundo%:\n\
        \  Rune c = *g*\n\
        \.~"
      runTestForValidTAC p [
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0" TChar 1 Constants ("lit",-1) []) (tacConstant ("'g'", TChar)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "c" TChar 1 Variables ("global",0) []) (tacVariable $ SymbolInfo "$t0" TChar 1 Constants ("lit",-1) []) Nothing]

  describe "Assign complex type literals" $ do
    it "assigns strings literals" $ do
      let p = "world %HolaMundo%:\n\
        \  Runes s = ~g4~\n\
        \.~"
      runTestForValidTAC p [
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0[0]" TChar 1 Constants ("lit",-1) []) (tacConstant ("'g'", TChar)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0[1]" TChar 1 Constants ("lit",-1) []) (tacConstant ("'4'", TChar)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "s" TStr 1 Variables ("global",0) []) (tacVariable $ SymbolInfo "$t0" TStr 1 Constants ("lit",-1) []) Nothing]

    it "assigns array literals" $ do
      let p = "world %HolaMundo%:\n\
        \  Power|}2{| array = |)2, 3(|\n\
        \.~"
      runTestForValidTAC p [
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0[0]" TInt 1 Constants ("lit",-1) []) (tacConstant ("2", TInt)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0[1]" TInt 1 Constants ("lit",-1) []) (tacConstant ("3", TInt)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "array" (TArray (Literal (Integer 2) TInt) TInt) 1 Variables ("global",0) []) (tacVariable $ SymbolInfo "$t0" (TArray (Literal (Integer 2) TInt) TInt) 1 Constants ("arr/list",-1) []) Nothing]

    it "assigns list literals" $ do
      let p = "world %HolaMundo%:\n\
        \  Kit of Power list = <<2, 3>>\n\
        \.~"
      runTestForValidTAC p [
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0[0]" TInt 1 Constants ("lit",-1) []) (tacConstant ("2", TInt)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "$t0[1]" TInt 1 Constants ("lit",-1) []) (tacConstant ("3", TInt)) Nothing, 
        T.ThreeAddressCode T.Assign (tacVariable $ SymbolInfo "list" (TList TInt) 1 Variables ("global",1) []) (tacVariable $ SymbolInfo "$t0" (TList TInt) 1 Constants ("arr/list",-1) []) Nothing]

    it "assigns struct literals" $ do
      pendingWith "We hope arriving here somehow, sometime, and we hope arriving alive"