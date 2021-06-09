{- |
 *  Imports BackEnd
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd (module BackEnd) where

import Playit.BackEnd.FinalCode                  as BackEnd
import Playit.BackEnd.RegAlloc.FlowGraph         as BackEnd
import Playit.BackEnd.RegAlloc.GraphColoring     as BackEnd
import Playit.BackEnd.RegAlloc.InterferenceGraph as BackEnd
import Playit.BackEnd.RegAlloc.LiveVariables     as BackEnd
import Playit.BackEnd.TAC                        as BackEnd
import Playit.BackEnd.Types                      as BackEnd
import Playit.BackEnd.Utils                      as BackEnd
