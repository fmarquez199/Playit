{- |
 * Flow Graph
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.FlowGraph (genFlowGraph) where

import Playit.BackEnd.Types
import qualified Data.Graph             as G
import qualified Playit.BackEnd.TACType as T


genFlowGraph :: [TAC] -> ((G.Graph, G.Vertex -> (FGNode, FGKey, [FGKey]), FGKey -> Maybe G.Vertex), [Int])
genFlowGraph tac = (G.graphFromEdges blocks,leaders)
  where
    leaders = getLeaders tac 0
    blocks  = ([],"ENTRY",["B1"]) : genBlocks tac leaders 1

-- poner arista del ultimo al key EXIT
genBlocks :: [TAC] ->  [Int] -> Int -> [([TAC],FGKey,[FGKey])]
genBlocks _ [] _                 = [([],"EXIT",[])]
genBlocks tac (leader:leaders) i = (block,key,edges) : genBlocks tac leaders (i + 1)
  where
    next  = if null leaders then length tac else head leaders
    begin = tac !! leader
    end   = if null leaders then tac !! (next - 1) else tac !! next -- <<<<
    block = take (next - leader) . drop leader $ tac
    key   = "B" ++ show i
    edge  = if null leaders then "EXIT"  else "B" ++ show (i + 1)
    ciclo = if isGoto end && gotoB then True else False -- <<<<
    edges = if ciclo then [key,edge] else [edge]
    rv1   = T.tacRvalue1 end
    rv2   = T.tacRvalue2 end

    isLab :: TACOP -> Bool
    isLab (Just (T.Label l)) = True
    isLab _                  = False

    getLabel :: TACOP -> String
    getLabel (Just (T.Label l)) = l
    getLabel _                  = ""
    
    gotoB :: Bool
    gotoB
      | isLab rv1 = getLabel rv1 == getLabel (T.tacLvalue begin)
      | isLab rv2 = getLabel rv2 == getLabel (T.tacLvalue begin)
      | otherwise = False


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

