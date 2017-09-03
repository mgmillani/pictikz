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
import Data.Char

appname = "pictikz"
appver  = "1.2.0.0"
applicense = ", released under GPLv3"

data Action a t =
  Action
  { help      :: Bool
  , version   :: Bool
  , latexColors :: Bool
  , colorFile :: FilePath
  , colors    :: [(Color, String)]
  , nodeF     :: [t a] -> [t a]
  , outputF   :: Graph a -> IO ()
  , inF       :: FilePath
  }

printGraph g = putStrLn $ draw g

writeGraph outF g = writeFile outF $ draw g

defaultPercent = 0.2
defaultAction = Action
  { help = False
  , version = False
  , nodeF = id
  , outputF = printGraph
  , latexColors = False
  , inF = ""
  , colorFile = ""
  , colors = defaultColors
  } :: Action Float Node

parseArgs action args = case args of
  "-c"       :f:r -> parseArgs (action{colorFile = f}) r
  "--colors" :f:r -> parseArgs (action{colorFile = f}) r
  "--colours":f:r -> parseArgs (action{colorFile = f}) r
  "-h"    :r  -> parseArgs (action{help = True}) r
  "--help":r  -> parseArgs (action{help = True}) r
  "-v"       :r -> parseArgs (action{version = True}) r
  "--version":r -> parseArgs (action{version = True}) r
  "-o"      :f:r  -> parseArgs (action{outputF = writeGraph f}) r
  "--output":f:r  -> parseArgs (action{outputF = writeGraph f}) r
  "-f"   :ws:hs:r  ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (fitToBox w h) . (nodeF action)}) r
  "--fit":ws:hs:r  ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (fitToBox w h) . (nodeF action)}) r
  "--latex-colours":r -> parseArgs action{latexColors = True} r
  "--latex-colors" :r -> parseArgs action{latexColors = True} r
  "-s"     :ws:hs:r  ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (scaleToBox w h) . (nodeF action)}) r
  "--scale":ws:hs:r  ->
    let w = read ws :: Float
        h = read hs :: Float
    in parseArgs (action{nodeF = (scaleToBox w h) . (nodeF action)}) r
  "-u"       :r   -> parseUniform (genGroup distanceGroup) action r
  "--uniform":r   -> parseUniform (genGroup distanceGroup) action r
  "-g"    :r      -> parseUniform isometricGroup action r
  "--grid":r      -> parseUniform isometricGroup action r
  f:r   -> parseArgs action{inF = f} r
  [] -> action
  where
    isFloat n = and $ map (\x -> isNumber x || x =='.') n
    parseUniform f action [] = action{nodeF = (uniformCoordinatesBy f defaultPercent) . (nodeF action)}
    parseUniform f action (ps:r)
      | isFloat ps = parseArgs (action{nodeF = (uniformCoordinatesBy f (read ps / 100 :: Float)) . (nodeF action)}) r
      | otherwise  = parseArgs (action{nodeF = (uniformCoordinatesBy f defaultPercent) . (nodeF action)}) (ps:r)

showHelp = do
  mapM_ putStrLn $
    [ appname ++ appver ++ applicense
    , "usage: pictikz [OPTION...] <FILE>"
    , ""
    , "where"
    , " OPTION:"
    , "  -c, --colours <FILE>       load colour definitions from FILE. See manpage for more information."
    , "  -f, --fit WIDTH HEIGHT     fit coordinates into a box of size WIDTH x HEIGHT without"
    , "                               changing aspect ratio."
    , "  -g, --grid [PERCENT]       fit coordinates into a grid (implies --uniform [PERCENT])."
    , "                               By default PERCENT = " ++ show (floor $ defaultPercent * 100) ++ "."
    , "  -h, --help                 show help."
    , "      --latex-colours        output colours in LaTeX."
    , "  -o, --output FILE          writes output into FILE instead of stdout."
    , "  -s, --scale WIDTH HEIGHT   scale coordinates into a box of size WIDTH x HEIGHT."
    , "  -u, --uniform [PERCENT]    group coordinates by distance. Maximum distance for grouping"
    , "                               is PERCENT of the axis in question."
    , "  -v, --version              output version and exit."
    ]

showVersion = putStrLn $ appname ++ appver ++ applicense

defaultColors =
  [ (RGB    0    0    0, "pictikz-black")
  , (RGB  255  255  255, "pictikz-white")
  , (RGB  128  128    0, "pictikz-gray")
  , (RGB  255   83   83, "pictikz-red")
  , (RGB  101  237  101, "pictikz-green")
  , (RGB  123  168  255, "pictikz-blue")
  , (RGB  246  246   82, "pictikz-yellow")
  , (RGB   87  239  239, "pictikz-cyan")
  , (RGB  225  103  255, "pictikz-purple")
  , (RGB  178  255   66, "pictikz-light-green")
  , (RGB  255  166   61, "pictikz-orange")
  , (RGB  255  141  255, "pictikz-pink")
  ]



execute action
  | help action      = showHelp
  | version action   = showVersion
  | colorFile action /= "" = do
    colorF <- readFile $ colorFile action
    let newColors = loadColors colorF
    execute action{colors = newColors, colorFile = ""}
  | latexColors action = do
    mapM_ (putStrLn . toLatex) $ colors action
  | inF action == "" = showHelp
  | otherwise = do
    svg <- readFile $ inF action
    let (Graph nodes edges) = loadGraph svg (colors action)
    (outputF action) $ Graph ((nodeF action) nodes) edges
  where
    toLatex (color, cname) = "\\definecolor{" ++ cname ++"}" ++ draw color

main :: IO ()
main = do
  args <- getArgs
  let action = parseArgs defaultAction args :: Action Float Node
  execute action
