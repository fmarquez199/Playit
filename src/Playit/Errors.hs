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
  , errorMsg'
  , showLexerErrors
  )
  where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Utils               as U


-- -----------------------------------------------------------------------------
data Error = Error
  { errorMsg     :: BL.ByteString   -- ^ Invalid token, message
  , errorContext :: [BL.ByteString] -- ^ Context of the error, portion of the source code
  , errorPos     :: U.Position
  }

instance Show Error where
  show (Error msg context (r,c))
    =  BLC.unpack msg
    ++ "\n\x1b[93m|\n| " ++ show r ++ "\t\x1b[0;96m" ++ show (context)
    -- ++ errorRuler c


-- -----------------------------------------------------------------------------
-- | Show all lexical errors
showLexerErrors :: [Error] -> (BL.ByteString, [BL.ByteString]) -> IO ()
showLexerErrors [] _                                  = BLC.putStrLn $ BLC.pack ""
showLexerErrors (Error err context p : errs) fileCode =
  let msg = BL.concat [BLC.pack "Bug found ", err]
  in BLC.putStrLn (errorMsg' msg fileCode p) >> showLexerErrors errs fileCode


errorMsg' :: BL.ByteString -> (BL.ByteString, [BL.ByteString]) -> U.Position -> BL.ByteString
errorMsg' msg (file, code) (r,c) = BL.concat
  [U.newLine, U.cyan, msg, U.blue, U.twoDots, file, U.twoDots, U.newLine, U.yellow, U.pipeline,
   U.newLine, U.pipeline, BLC.pack (show r), U.tab, U.cyan', code !! r, errorRuler c]


errorRuler :: Int -> BL.ByteString
errorRuler c = BL.concat 
  [U.tab, U.yellow', BL.replicate (fromIntegral c - 1) 46, U.tilt, BLC.pack "^", U.nocolor, U.newLine]


