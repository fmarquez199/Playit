{- |
 * Flow Graph
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.FlowGraph (genFlowGraph, printFGNodes) where

import Playit.BackEnd.Types
import Playit.BackEnd.Utils (tacNewLabel, tacLabel)
import qualified Data.Graph as G
import qualified TACType    as T


genFlowGraph :: [TAC] -> (FlowGraph, [Int])
genFlowGraph tac = (G.graphFromEdges blocks,leaders)
  where
    leaders = 0 : getLeaders tac 0
    block1  = tac !! head leaders
    entry   = ([],tacNewLabel $ tacLabel "ENTRY",[block1])
    blocks  = entry : genBlocks tac leaders

-- 
genBlocks :: [TAC] -> [Int]-> [(FGNode,FGKey,[FGKey])]
genBlocks _ []                 = [([],tacNewLabel $ tacLabel "EXIT",[])]
genBlocks tac (leader:leaders) = (block,begin,edges) : genBlocks tac leaders
  where
    exit  = tacNewLabel $ tacLabel "EXIT"
    begin = tac !! leader
    next  = if null leaders then length tac else head leaders
    lastI = tac !! (next - 1)
    block = take (next - leader) . drop leader $ tac
    nextB = if null leaders then exit else tac !! next
    ciclo = getLabel (T.tacRvalue2 lastI)
    noRet = T.tacOperand lastI /= T.Return
    edges
      | null ciclo && noRet = [nextB]
      | not noRet           = exit : map (tac !!) leaders
      | not (null ciclo)    = [nextB,tacNewLabel $ tacLabel ciclo]
      | otherwise = [tacNewLabel $ tacLabel "what?"]
    

getLeaders :: [TAC] -> Int -> [Int]
getLeaders [] _         = []
getLeaders (i:is) index
  | isLabel i = index : getLeaders is (index + 1)
  | isJump i && not (isLabel $ head is) = index + 1 : getLeaders is (index + 1)
  | otherwise = getLeaders is (index + 1)

  where
    jumps = [T.Call,T.If,T.GoTo,T.Lt,T.Lte,T.Gt,T.Gte,T.Eq,T.Neq]
    isJump :: TAC -> Bool
    isJump instr = T.tacOperand instr `elem` jumps

    isLabel :: TAC -> Bool
    isLabel instr = T.tacOperand instr == T.NewLabel


getLabel :: TACOP -> String
getLabel (Just (T.Label l)) = l
getLabel _                  = ""
    


printFGNodes :: [(FGNode, FGKey, [FGKey])] -> String
printFGNodes [] = ""
printFGNodes ((node,key,succs):nodes) = 
  "\n\nNode Key(" ++ show key ++ ")\n" ++
  show node ++ "\n" ++
  concatMap ("\t->" ++ ) (map show succs) ++
  printFGNodes nodes


