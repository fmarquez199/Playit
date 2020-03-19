module Playit.BackEnd.LiveVariables (getLiveVars, initLiveVars, printLiveVars) where

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



initLiveVars :: [(FGNode, FGKey, [FGKey])] -> LiveVars
initLiveVars fgNodes = LiveVars fgNodes M.empty True


{-
  Mientras haya cambios en algun liveVars
    En todos los bloques B distintos de EXIT
      OUT de B son todas las variables vivas de los bloques a los que va
      liveVars es las vars que usa B unido con su OUT al que se le restan sus definiciones
-}
outB :: [FGKey] -> LVMonad (S.Set Var)
outB succs = do
  bLV <- bLiveVars <$> get
  let lvSuccs = map (\key -> let keyLV = M.lookup key bLV in fromMaybe S.empty keyLV) succs
  return $ foldl S.union S.empty lvSuccs


-- Variables posiblemente usadas en B antes de ser definidas en B
-- Estan vivas al entrar a B (en liveVars de B)
useB :: FGNode -> [(Var,TACOP)] -> S.Set Var
useB b vars = S.unions $ map S.singleton (concatMap (getRvals vars) b)
  where
    getRvals :: [(Var,TACOP)] -> TAC -> [Var]
    getRvals vars instr = mapMaybe (getId vars) $ getValues instr


-- Variables definidas en B antes de ser usadas en B
-- Estan muertas al entrar a B (en liveVars de B)
defB :: FGNode -> [(Var,TACOP)] -> S.Set Var
defB b vars = S.unions $ map defI b
  where        
    defI :: TAC -> S.Set Var
    defI instr = if isAssign op && isJust id' then S.singleton $ fromJust id'
                 else S.empty
      where
        op   = T.tacOperand instr
        lVal = T.tacLvalue instr
        id'  = getId vars lVal

        isAssign :: T.Operation -> Bool
        isAssign op = op `elem` assigns
          where
            assigns = [T.Assign, T.Add, T.Sub, T.Minus, T.Mult,
                      T.Div, T.Mod,  T.Get, T.Ref, T.Call]


blockLiveVars :: [(Var,TACOP)] -> (FGNode, FGKey, [FGKey]) -> LVMonad ()
blockLiveVars vars (b,key,succs) = do
  outB' <- outB succs
  let liveVars = useB b vars `S.union` (outB' `S.difference` defB b vars)
  state@LiveVars{bLiveVars = bLV} <- get
  put state{bLiveVars = M.insert key liveVars bLV, changed = key == tacNewLabel (tacLabel "EXIT")}


getLiveVars :: FlowGraph -> [(Var,TACOP)] -> LVMonad ()
getLiveVars fg@(graph, getNodeFromVertex, getVertexFromKey) vars = do
  state <- get
  when (changed state) $ do
    put state{changed = False}
    mapM_ (blockLiveVars vars) (blocks state)
    getLiveVars fg vars


---- Auxs

getValues :: TAC -> [TACOP]
getValues instr =
  if T.tacOperand instr `elem` condJumps then
    [T.tacLvalue instr, T.tacRvalue1 instr] 
  else
    [T.tacRvalue1 instr, T.tacRvalue2 instr]

  where
    condJumps = [T.If, T.IfFalse, T.Eq, T.Neq, T.Lt, T.Gt, T.Lte, T.Gte]


getId :: [(Var,TACOP)] -> TACOP -> Maybe Var
getId vars val =
  case val of
    Just (T.Id temp) -> var
      where
        varOfTemp = filter (\(var,Just (T.Id temp'))-> temp == temp') vars
        var       = if null varOfTemp then Nothing else Just (fst . head $ varOfTemp)
    _               -> Nothing


printLiveVars :: [(TAC,S.Set Var)] -> String
printLiveVars [] = ""
printLiveVars ((key,liveVars):nextLiveVars) =
  "\nNode Key(" ++ show key ++ "): " ++
  "{" ++ intercalate "," (map show (S.toList liveVars)) ++ "}" ++
  printLiveVars nextLiveVars
