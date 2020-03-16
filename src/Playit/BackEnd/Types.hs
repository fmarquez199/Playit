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
type TAC     = TACT.ThreeAddressCode TACInfo Type
type TACOP   = Maybe (TACT.Operand TACInfo Type)

data TACInfo = Temp Id OffSet | TACVar SymbolInfo OffSet  deriving (Eq, Ord)

instance TACT.SymEntryCompatible TACInfo where
  getSymID (Temp n o)      = "[" ++ show o ++ "]->(" ++ n ++ ")"
  getSymID (TACVar info o) = "[" ++ show o ++ "]->(" ++ symId info ++ ")"

instance Show TACInfo where
  show = TACT.getSymID

{- | Guarda las variables del codigo del programa, los registros temporales,
  literales, labels, los labels a donde ir cuando se hace break y continue,
  la pila de offsets(continua desde donde quedo luego de crear el AST) y la
  tabla de simbolos ya creada
-}
data Operands = Operands {
  vars  :: M.Map Var TACOP,
  temps :: M.Map TACInfo Bool,
  lits  :: M.Map TACOP TACOP,
  labs  :: [Int],   -- Labels
  brkL  :: TACOP,   -- brake label
  contL :: TACOP,   -- continue label
  base  :: OffSet,
  -- fp    :: OffSet,
  callF :: Bool, -- Dice si generar TAC para free
  callM :: Bool, -- Dice si generar TAC para malloc
  subs  :: [(Id, InstrSeq, Bool)], -- subrutinas a generar su codigo
  astST :: SymTab
} deriving (Eq, Ord)

instance Show Operands where
  show (Operands vs ts ls lbs brk con b _ _ s st) = 
    "\n vars: " ++ show vs ++ "\n temps: " ++ show ts ++
    "\n lits: " ++ show ls ++ "\n labels: " ++ show lbs {- ++ -}
    -- "\n break: " ++ show brk ++ "\n continue: " ++ show con ++
    -- "\n base: " ++ show b ++ "\nsubroutines: " ++ show s {- ++ show st -} ++ "\n"

-- Monad para manejar los operandos, writer tiene la lista de las instrucciones
-- de tres direcciones, reader tiene el AST que sale del parser
type TACMonad a = RWST Instr [TAC] Operands IO a


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Flow Graph creation
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type FGNode         = [TAC]
type FGKey          = TAC
type NodeFromVertex = G.Vertex -> (FGNode, FGKey, [FGKey])
type VertexFromKey  = FGKey -> Maybe G.Vertex
type FlowGraph      = (G.Graph, NodeFromVertex, VertexFromKey)


data LiveVars = LiveVars {
  blocks    :: [(FGNode, FGKey, [FGKey])],
  bLiveVars :: M.Map FGKey (S.Set Var),
  changed   :: Bool
} deriving (Eq, Ord, Show)

-- instance Show LiveVars where
--   show (LiveVars b bLV _) =
--     "\n blocks: " ++ show b ++ "\n\n blocks liveVars: " ++ show bLV ++ "\n"

type LVMonad a = StateT LiveVars IO a
