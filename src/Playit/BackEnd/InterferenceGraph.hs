module Playit.BackEnd.InterferenceGraph (genInterferenceGraph) where

import Playit.BackEnd.GraphColoring (colorDsatur,VertColorMap)
import qualified Data.IntMap as IntMap
import qualified Data.Graph as Graph


genInterferenceGraph :: Graph.Graph -> VertColorMap
genInterferenceGraph = colorDsatur