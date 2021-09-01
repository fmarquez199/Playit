module Playit.FrontEnd.Errors
  (notInLoop
  )
  where

import Control.Monad.Trans.RWS (ask, tell)

import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Errors              as E
import qualified Playit.Utils               as U

import qualified Playit.FrontEnd.Lexer      as Lex
import qualified Playit.FrontEnd.ParserM    as PM


notInLoop :: U.Position -> PM.ParserM ()
notInLoop posn@(r, _) = do
  PM.ParserR{PM.prFilename = filename, PM.prCode = code} <- ask
  -- break/continue statement not within loop
  tell [E.Error "You are not in a loop" [code !! max 0 (r - 1)] filename posn]
