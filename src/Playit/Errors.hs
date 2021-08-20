{- |
 * Errors handler
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.Errors
  ( Error(..)
  )
  where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Utils               as U


-- -----------------------------------------------------------------------------
data Error = Error
  { errorMsg     :: String
  , errorContext :: [BL.ByteString] -- ^ Context of the error, portion of the source code
  , errorFile    :: String
  , errorPos     :: U.Position
  }

instance Show Error where
  show (Error msg context file (r,c))
    =  "\n\n\x1b[1;36m" ++ msg ++ "\x1b[94m: " ++ file ++ ":"
    ++ concatMap (formatContex r) context
    ++ errorRuler c


formatContex :: Int -> BL.ByteString -> String
formatContex r context = (BLC.unpack . BL.concat)
  [ U.newLine, U.yellow, U.pipeline
  , U.newLine, U.pipeline, BLC.pack (show r), U.tab, U.cyan', context
  , U.newLine
  ]


errorRuler :: Int -> String
errorRuler c = (BLC.unpack . BL.concat)
  [ U.tab, U.yellow', BL.replicate (fromIntegral c - 1) 46, U.tilt, BLC.pack "^"
  , U.nocolor, U.newLine
  ]
