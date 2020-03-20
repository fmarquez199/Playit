module Playit.BackEnd.RegAlloc.InterferenceGraph (genInterferenceGraph, printIGNodes) where

import Playit.BackEnd.Types
import qualified Data.Graph as G
import qualified Data.Set   as S


genInterferenceGraph :: S.Set (S.Set TACInfo) -> InterfGraph
genInterferenceGraph liveVars = G.graphFromEdges edges
  where
    edges = S.toList $ S.unions $ S.toList $ S.map (createEdge liveVars) liveVars


createEdge :: S.Set (S.Set TACInfo) -> S.Set TACInfo -> S.Set (TACInfo,TACInfo,[TACInfo])
createEdge liveVars vars = S.map (addEdge liveVars vars) vars


addEdge :: S.Set (S.Set TACInfo) -> S.Set TACInfo -> TACInfo -> (TACInfo,TACInfo,[TACInfo])
addEdge liveVars vars var = (var,var,vars')
  where
    others = S.map (\lvs -> if S.member var lvs then S.delete var lvs else S.empty) (S.delete vars liveVars)
    vars' = S.toList $ S.unions $ S.toList (S.union others $ S.singleton $ S.delete var vars)


printIGNodes :: [(TACInfo, TACInfo, [TACInfo])] -> String
printIGNodes [] = ""
printIGNodes ((node,key,succs):nodes) = 
  "\n\nNode Key(" ++ show key ++ ")\n" ++
  show node ++ "\n" ++
  concatMap (("\t->" ++ ) . show) succs ++
  printIGNodes nodes


