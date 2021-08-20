{- |
 * TAC types
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.Types where

import Control.Monad.Trans.RWS
import Control.Monad.Trans.State
import Playit.FrontEnd.Types
import qualified Data.Map       as M
import qualified Data.Graph     as G
import qualified Data.Set       as S
import qualified TACType        as TACT

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                               TAC creation
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


type TempReg = String
type OffSet  = Int
type Width   = Int
type TAC     = TACT.ThreeAddressCode TACInfo Type
type TACOP   = Maybe (TACT.Operand TACInfo Type)

-- tac operands
-- include 
--    Constant (String, Type)
--    Label String
data TACInfo = Temp Id Type (OffSet, Width)
             | TACVar SymbolInfo (OffSet, Width)
             deriving (Eq, Ord)

instance TACT.SymEntryCompatible TACInfo where
  getSymID (Temp n _ (o, w))    = n
  -- getSymID (Temp n _ (o, w))    = "[" ++ show o ++ "]->(" ++ n ++ ")"
  getSymID (TACVar info (o, w)) = symId info
  -- getSymID (TACVar info (o, w)) = "[" ++ show o ++ "]->(" ++ symId info ++ ")"

instance Show TACInfo where
  show = TACT.getSymID

{- | Guarda las variables del codigo del programa, los registros temporales,
  literales, labels, los labels a donde ir cuando se hace break y continue,
  la pila de offsets(continua desde donde quedo luego de crear el AST) y la
  tabla de simbolos ya creada
-}
data Operands = Operands {
  vars  :: M.Map Var TACOP, -- (var, temp donde esta guardada)
  temps :: M.Map TACInfo Bool, -- (temp, ocupado)
  lits  :: M.Map TACOP TACOP, -- (literal, temp donde esta guardado)
  labs  :: [String],   -- Labels
  brkL  :: TACOP,   -- brake label
  contL :: TACOP,   -- continue label
  base  :: OffSet,
  corr :: Bool, -- dice si generar la subrutina para correcursion
  -- fp    :: OffSet,
  callF :: Bool, -- Dice si generar TAC para free
  callM :: Bool, -- Dice si generar TAC para malloc
  subs  :: [(Id, Params, InstrSeq, Bool)], -- subrutinas a generar su codigo
  astST :: SymTab
  -- _data :: [(tipo de .data, label, valor)]
} deriving (Eq, Ord)

instance Show Operands where
  show (Operands vs ts ls lbs brk con b _ _ _ s st) = 
    "\n vars: " ++ show vs ++ "\n temps: " ++ show ts ++
    "\n lits: " ++ show ls ++ "\n labels: " ++ show lbs {- ++ -}
    -- "\n break: " ++ show brk ++ "\n continue: " ++ show con ++
    -- "\n base: " ++ show b ++ "\nsubroutines: " ++ show s {- ++ show st -} ++ "\n"

-- Monad para manejar los operandos, writer tiene la lista de las instrucciones
-- de tres direcciones, reader tiene el AST que sale del parser
type TACMonad a = RWST Instr [TAC] Operands IO a


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Register Allocation
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type FGNode         = [TAC]
type FGKey          = TAC
type NodeFromVertex = G.Vertex -> (FGNode, FGKey, [FGKey])
type VertexFromKey  = FGKey -> Maybe G.Vertex
type FlowGraph      = (G.Graph, NodeFromVertex, VertexFromKey)
type InterfGraph    = (G.Graph, G.Vertex -> IGraphEdge, TACInfo -> Maybe G.Vertex)
type IGraphEdge     = (TACInfo, TACInfo, [TACInfo])
type Reg = Int

-- 
data RegAlloc = RegAlloc {
  blocks    :: [(FGNode, FGKey, [FGKey])], -- Basic blocks
  bLiveVars :: M.Map FGKey (S.Set TACInfo), -- Variables vivas por bloque
  lvChanged :: Bool -- Indica si algun conjunto de variables vivas cambio
  -- varInReg  :: M.Map Var Reg, -- Mantiene que variables tienen aisgnado que registro
  -- interfG   :: InterfGraph -- Grafo de interferencia
  -- spills    :: S.Set Var, -- Conjunto de variables que son spill
  -- , -- 
  -- , -- 
  -- tac'      :: [TAC] -- TAC modificado si se genera un spill
} deriving (Eq, Ord, Show)

-- instance Show RegAlloc where
--   show (RegAlloc b bLV _) =
--     "\n blocks: " ++ show b ++ "\n\n blocks liveVars: " ++ show bLV ++ "\n"


-- 
type RegAllocMonad a = StateT RegAlloc IO a
