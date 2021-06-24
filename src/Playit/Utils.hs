{- |
 *  Escape sequences for colors and common used symbols
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.Utils where

import           Data.List (intercalate)

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC


-- -----------------------------------------------------------------------------
-- | Position in source code
type Position = (Int, Int)
-- data Position = Position {row :: !Int, col :: !Int}

-- Symbol scope
type Scope = Integer
-- data Scope = Scope !Integer


-- -----------------------------------------------------------------------------
-- joinWith :: (Show a) => String -> (a -> [String]) -> [a] -> String
-- joinWith s f = intercalate s . f

joinWith :: (Show a) => String -> [a] -> String
joinWith s = intercalate s . map show


-- -----------------------------------------------------------------------------
newLine :: BL.ByteString
newLine = BLC.pack "\n"

tab :: BL.ByteString
tab = BLC.pack "\t"

pipeline :: BL.ByteString
pipeline = BLC.pack "| "

twoDots :: BL.ByteString
twoDots = BLC.pack ": "

nocolor :: BL.ByteString
nocolor = BLC.pack "\x1b[0m"

italic :: BL.ByteString
italic = BLC.pack "\x1b[3m"

bold :: BL.ByteString
bold = BLC.pack "\x1b[1m"

underline :: BL.ByteString
underline = BLC.pack "\x1b[4m"

tilt :: BL.ByteString
tilt = BLC.pack "\x1b[5;31m"

-- dim :: BL.ByteString
-- dim = BLC.pack "\x1b[2m"

blue :: BL.ByteString
blue = BLC.pack "\x1b[94m"

blue' :: BL.ByteString
blue' = BLC.pack "\x1b[1;94m"

yellow :: BL.ByteString
yellow = BLC.pack "\x1b[93m"

yellow' :: BL.ByteString
yellow' = BLC.pack "\x1b[1;93m"

red :: BL.ByteString
red = BLC.pack "\x1b[1;91m"

cyan :: BL.ByteString
cyan = BLC.pack "\x1b[1;36m"

cyan' :: BL.ByteString
cyan' = BLC.pack "\x1b[0;96m"

-- magenta :: BL.ByteString
-- magenta = BLC.pack "\x1b[35m"

-- brightMagenta :: BL.ByteString
-- brightMagenta = BLC.pack "\x1b[95m"

-- brightRed :: BL.ByteString
-- brightRed = BLC.pack "\x1b[91m"

-- green :: BL.ByteString
-- green = BLC.pack "\x1b[32m"
