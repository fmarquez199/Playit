module Playit.BackEnd.RegAlloc.LiveVariables (
    getLiveVars
  , initRegAlloc
  , printLiveVars
  ) where

import Control.Monad             (when, mapM_)
import Control.Monad.Trans.State (get, put)
import Data.List                 (intercalate)
import Data.Maybe                (mapMaybe, isJust, fromJust, fromMaybe)
import Playit.BackEnd.Types
import Playit.FrontEnd.Types     (Var(..),Type(..))
import Playit.BackEnd.Utils      (tacNewLabel, tacLabel)
import qualified Data.Graph      as G
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified TACType         as T



initRegAlloc :: [(FGNode, FGKey, [FGKey])] -> RegAlloc
initRegAlloc fgNodes = RegAlloc fgNodes M.empty True


{-
  Mientras haya cambios en algun liveVars
    En todos los bloques B distintos de EXIT
      OUT de B son todas las variables vivas de los bloques a los que va
      liveVars es las vars que usa B unido con su OUT al que se le restan sus definiciones
-}
outB :: [FGKey] -> RegAllocMonad (S.Set TACInfo)
outB succs = do
  bLV <- bLiveVars <$> get
  let lvSuccs = map (\key -> let keyLV = M.lookup key bLV in fromMaybe S.empty keyLV) succs
  return $ foldl S.union S.empty lvSuccs


-- Variables posiblemente usadas en B antes de ser definidas en B
-- Estan vivas al entrar a B (en liveVars de B)
useB :: FGNode -> S.Set TACInfo
useB b = S.unions $ map S.singleton (concatMap getRvals b)
  where
    getRvals :: TAC -> [TACInfo]
    getRvals instr = mapMaybe getId $ getValues instr


-- Variables definidas en B antes de ser usadas en B
-- Estan muertas al entrar a B (en liveVars de B)
defB :: FGNode -> S.Set TACInfo
defB b = S.unions $ map defI b
  where        
    defI :: TAC -> S.Set TACInfo
    defI instr = if isAssign op && isJust id' then S.singleton $ fromJust id'
                 else S.empty
      where
        op   = T.tacOperand instr
        lVal = T.tacLvalue instr
        id'  = getId lVal

        isAssign :: T.Operation -> Bool
        isAssign op = op `elem` assigns
          where
            assigns = [T.Assign, T.Add, T.Sub, T.Minus, T.Mult,
                      T.Div, T.Mod,  T.Get, T.Ref, T.Call]


blockLiveVars :: (FGNode, FGKey, [FGKey]) -> RegAllocMonad ()
blockLiveVars (b,key,succs) = do
  state@RegAlloc{bLiveVars = bLV} <- get
  outB' <- outB succs
  let
    -- Just oldLiveVars = M.lookup key bLV
    liveVars = useB b `S.union` (outB' `S.difference` defB b)
    newBLiveV = M.insert key liveVars bLV
    -- changed = liveVars /= oldLiveVars
  put state{bLiveVars = newBLiveV, lvChanged = key == tacNewLabel (tacLabel "EXIT")}


getLiveVars :: FlowGraph -> RegAllocMonad ()
getLiveVars fg@(graph, getNodeFromVertex, getVertexFromKey) = do
  state <- get
  when (lvChanged state) $ do
    put state{lvChanged = False}
    mapM_ blockLiveVars (blocks state)
    getLiveVars fg


---- Auxs

getValues :: TAC -> [TACOP]
getValues instr = [T.tacLvalue instr, T.tacRvalue1 instr, T.tacRvalue2 instr] 


getId :: TACOP -> Maybe TACInfo
getId val =
  case val of
    Just (T.Id temp) -> Just temp
    _                -> Nothing


printLiveVars :: [(TAC,S.Set TACInfo)] -> String
printLiveVars [] = ""
printLiveVars ((key,liveVars):nextLiveVars) =
  "\nNode Key(" ++ show key ++ "): " ++
  "{" ++ intercalate "," (map show (S.toList liveVars)) ++ "}" ++
  printLiveVars nextLiveVars
