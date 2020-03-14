{- |
 * Flow Graph
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.FlowGraph (genFlowGraph) where

import Playit.BackEnd.Types
import Playit.BackEnd.Utils (tacNewLabel, tacLabel)
import qualified Data.Graph as G
import qualified TACType    as T


genFlowGraph :: [TAC] -> (FlowGraph, [Int])
genFlowGraph tac = (G.graphFromEdges blocks,leaders)
  where
    leaders = getLeaders tac 0
    block1  = tac !! head leaders
    blocks  = ([],tacNewLabel $ tacLabel "ENTRY",[block1]) : genBlocks tac leaders

-- 
genBlocks :: [TAC] ->  [Int]-> [([TAC],FGKey,[FGKey])]
genBlocks _ []                 = [([],tacNewLabel $ tacLabel "EXIT",[])]
genBlocks tac (leader:leaders) = (block,key,edges) : genBlocks tac leaders
  where
    begin = tac !! leader
    next  = if null leaders then length tac else head leaders
    nextI = tac !! next
    lastI = tac !! (next - 1)
    block = take (next - leader) . drop leader $ tac
    key   = begin
    nextB = if null leaders then tacNewLabel $ tacLabel "EXIT" else nextI
    ciclo = getLabel (T.tacRvalue2 lastI)
    edges = if null ciclo then [nextB] else [nextB,tacNewLabel $ tacLabel ciclo]
    -- si es un return sus arcos son a todos despues de el

getLeaders :: [TAC] -> Int -> [Int]
getLeaders [] _         = []
getLeaders (i:is) index
  | isJump i  = index : getLeaders is (index + 1)
  | isGoto i  = index + 1 : getLeaders is (index + 1)
  | otherwise = getLeaders is (index + 1)


isGoto :: TAC -> Bool
isGoto instr = T.tacOperand instr `elem` jumps
  where
    jumps = [T.If,T.GoTo,T.Lt,T.Lte,T.Gt,T.Gte,T.Eq,T.Neq]


isJump :: TAC -> Bool
isJump instr = T.tacOperand instr `elem` jumps
  where
    jumps = [T.NewLabel,T.Call]


getLabel :: TACOP -> String
getLabel (Just (T.Label l)) = l
getLabel _                  = ""
    
