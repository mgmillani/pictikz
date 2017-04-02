--  Copyright 2017 Marcelo Garlet Millani
--  This file is part of pictikz.

--  pictikz is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  pictikz is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with pictikz.  If not, see <http://www.gnu.org/licenses/>.

module Main where

import Pictikz.Drawing
import Pictikz.Organizer
import Pictikz.Graph
import Pictikz.Loader
import Pictikz.Parser
import System.Environment

appname = "pictikz"
appver  = "1.0.0.0"
applicense = ", released under GPLv3"

data Action a t =
  Action
  { help    :: Bool
  , version :: Bool
  , nodeF   :: [t a] -> [t a]
  , outputF :: Graph a -> IO ()
  , inF     :: FilePath
  }

printGraph g = putStrLn $ draw g

writeGraph outF g = writeFile outF $ draw g

defaultPercent = 0.1
defaultAction = Action{help = False, version = False, nodeF = id, outputF = printGraph, inF = ""} :: Action Float Node

parseArgs action args = case args of
  "-h":r      -> parseArgs (action{help = True}) r
  "--help":r  -> parseArgs (action{help = True}) r
  "-v":r         -> parseArgs (action{version = True}) r
  "--version":r  -> parseArgs (action{version = True}) r
  "-o":f:r        -> parseArgs (action{outputF = writeGraph f}) r
  "--output":f:r  -> parseArgs (action{outputF = writeGraph f}) r
  "-f":ws:hs:r    ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (fitToBox w h) . (nodeF action)}) r
  "--fit":ws:hs:r  ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (fitToBox w h) . (nodeF action)}) r
  "-s":ws:hs:r    ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (fitToBox w h) . (nodeF action)}) r
  "--scale":ws:hs:r  ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (scaleToBox w h) . (nodeF action)}) r
  "-u":('-':o):r         -> parseArgs (action{nodeF = (uniformCoordinatesBy distanceGroup defaultPercent) . (nodeF action)}) (('-':o):r)
  "--uniform":('-':o):r  -> parseArgs (action{nodeF = (uniformCoordinatesBy distanceGroup defaultPercent) . (nodeF action)}) (('-':o):r)
  "-u":ps:r              -> parseArgs (action{nodeF = (uniformCoordinatesBy distanceGroup (read ps / 100 :: Float)) . (nodeF action)}) r
  "--uniform":ps:r       -> parseArgs (action{nodeF = (uniformCoordinatesBy distanceGroup (read ps / 100 :: Float)) . (nodeF action)}) r
  "-u":r                 -> parseArgs (action{nodeF = (uniformCoordinatesBy distanceGroup defaultPercent) . (nodeF action)}) r
  "--uniform":r          -> parseArgs (action{nodeF = (uniformCoordinatesBy distanceGroup defaultPercent) . (nodeF action)}) r
  "-g":('-':o):r      -> parseArgs (action{nodeF = (uniformCoordinatesBy isometricGroup defaultPercent) . (nodeF action)}) (('-':o):r)
  "--grid":('-':o):r  -> parseArgs (action{nodeF = (uniformCoordinatesBy isometricGroup defaultPercent) . (nodeF action)}) (('-':o):r)
  "-g":ps:r           -> parseArgs (action{nodeF = (uniformCoordinatesBy isometricGroup (read ps / 100 :: Float)) . (nodeF action)}) r
  "--grid":ps:r       -> parseArgs (action{nodeF = (uniformCoordinatesBy isometricGroup (read ps / 100 :: Float)) . (nodeF action)}) r
  "-g":r              -> parseArgs (action{nodeF = (uniformCoordinatesBy isometricGroup defaultPercent) . (nodeF action)}) r
  "--grid":r          -> parseArgs (action{nodeF = (uniformCoordinatesBy isometricGroup defaultPercent) . (nodeF action)}) r
  f:r   -> parseArgs action{inF = f} r
  [] -> action

showHelp = do
  mapM_ putStrLn $
    [ appname ++ appver ++ applicense
    , ""
    , "-f, --fit WIDTH HEIGHT     fit coordinates into a box of size WIDTH x HEIGHT without"
    , "                             changing aspect ratio."
    , "-g, --grid [PERCENT]       fit coordinates into a grid (implies --uniform [PERCENT])."
    , "                             By default PERCENT = " ++ show (floor $ defaultPercent * 100) ++ "."
    , "-h, --help                 show help."
    , "-o, --output FILE          writes output into FILE instead of stdout."
    , "-s, --scale WIDTH HEIGHT   scale coordinates into a box of size WIDTH x HEIGHT."
    , "-u, --uniform [PERCENT]    group coordinates by distance. Maximum distance for grouping"
    , "                             is PERCENT of the axis in question."
    , "-v, --version              output version and exit."
    ]

showVersion = putStrLn $ appname ++ appver ++ applicense

execute action
  | help action      = showHelp
  | version action   = showVersion
  | inF action == "" = showHelp
  | otherwise = do
    svg <- readFile $ inF action
    let (Graph nodes edges) = loadGraph svg
    (outputF action) $ Graph ((nodeF action) nodes) edges

main :: IO ()
main = do
  args <- getArgs
  let action = parseArgs defaultAction args :: Action Float Node
  execute action

