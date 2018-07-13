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

import qualified Debug.Trace as D (trace)

appname = "pictikz"
appVersion = "1.3.0.0"
applicense = "released under GPLv3"

data Action a t =
  Action
    { colorFile :: FilePath
    , colors    :: [(Color, String)]
    , help      :: Bool
    , inF       :: FilePath
    , latexColors :: Bool
    , nodeF     :: [t a] -> [t a]
    , outputF   :: Graph a -> IO ()
    , startTime :: Int
    , temporal  :: Bool
    , version   :: Bool
    }

printGraph g = putStrLn $ draw g

writeGraph outF g = writeFile outF $ draw g

defaultPercent = 0.2
defaultAction = Action
  { colorFile = ""
  , colors = defaultColors
  , help = False
  , inF = ""
  , latexColors = False
  , nodeF = id
  , outputF = printGraph
  , startTime = 0
  , temporal = False
  , version = False
  } :: Action Double Node

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
    let w = read ws
        h = read hs
    in parseArgs (action{nodeF = (fitToBox w h) . (nodeF action)}) r
  "--fit":ws:hs:r  ->
    let w = read ws
        h = read hs
    in parseArgs (action{nodeF = (fitToBox w h) . (nodeF action)}) r
  "--latex-colours":r -> parseArgs action{latexColors = True} r
  "--latex-colors" :r -> parseArgs action{latexColors = True} r
  "-s"     :ws:hs:r  ->
    let w = read ws
        h = read hs
    in parseArgs (action{nodeF = (scaleToBox w h) . (nodeF action)}) r
  "--scale":ws:hs:r  ->
    let w = read ws
        h = read hs
    in parseArgs (action{nodeF = (scaleToBox w h) . (nodeF action)}) r
  "-u"       :r   -> parseUniform (genGroup distanceGroup) action r
  "--uniform":r   -> parseUniform (genGroup distanceGroup) action r
  "-t"        :r  -> parseTemporal action r
  "--temporal":r  -> parseTemporal action r
  "-g"    :r      -> parseUniform isometricGroup action r
  "--grid":r      -> parseUniform isometricGroup action r
  f:r   -> parseArgs action{inF = f} r
  [] -> action
  where
    isFloat n = and $ map (\x -> isNumber x || x =='.') n
    isInt n = and $ map isNumber n
    parseUniform f action [] = action{nodeF = (uniformCoordinatesBy f defaultPercent) . (nodeF action)}
    parseUniform f action (ps:r)
      | isFloat ps = parseArgs (action{nodeF = (uniformCoordinatesBy f (read ps / 100)) . (nodeF action)}) r
      | otherwise  = parseArgs (action{nodeF = (uniformCoordinatesBy f defaultPercent) . (nodeF action)}) (ps:r)
    parseTemporal action (t0:r)
      | isInt t0  = parseArgs (action{temporal = True, startTime = (read t0)}) r
      | otherwise = parseArgs (action{temporal = True, startTime = 1}) (t0:r)

showHelp = do
  mapM_ putStrLn $
    [ appname ++ " " ++ appVersion ++ ", " ++ applicense
    , "usage: pictikz [OPTION...] <FILE>"
    , ""
    , "where"
    , " OPTION:"
    , "  -c, --colours <FILE>       Load colour definitions from FILE. See manpage for more information."
    , "  -f, --fit WIDTH HEIGHT     Fit coordinates into a box of size WIDTH x HEIGHT without"
    , "                               changing aspect ratio."
    , "  -g, --grid [PERCENT]       Fit coordinates into a grid (implies --uniform [PERCENT])."
    , "                               By default PERCENT = " ++ show (floor $ defaultPercent * 100) ++ "."
    , "  -h, --help                 Show help."
    , "      --latex-colours        Output colours in LaTeX."
    , "  -o, --output FILE          Write output into FILE instead of stdout."
    , "  -s, --scale WIDTH HEIGHT   Scale coordinates into a box of size WIDTH x HEIGHT."
    , "  -t, --temporal [START]     Treat SVG layers as frames, using overlay specifications in the output."
    , "  -u, --uniform [PERCENT]    Group coordinates by distance. Maximum distance for grouping"
    , "                               is PERCENT of the axis in question."
    , "  -v, --version              Output version and exit."
    ]

showVersion = putStrLn $ appname ++ appVersion ++ applicense

defaultColors =
  [ (RGB    0    0    0, "pictikz-black")
  , (RGB  255  255  255, "pictikz-white")
  , (RGB  128  128  128, "pictikz-gray")
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
    let layers = filter (\(Graph n e) -> not $ null n) $ loadGraph svg (colors action)
        bbs = map boundingBox layers
        (x0,y0,x1,y1) = foldl1 (\(xa,ya,xb,yb) (xc,yc,xd,yd) -> (min xa xc, min ya yc, max xb xd, max yb yd)) bbs
        w = x1 - x0
        h = y1 - y0
        epsilon = defaultPercent * (min w h) * 0.5
        (Graph nodes edges) = if temporal action then foldr1 (mergeLayers epsilon) layers else foldr1 (\(Graph n0 e0) (Graph n1 e1) -> Graph (n0 ++ n1) (e0 ++ e1)) layers
        -- shift time stamps if necessary, or disable them by setting them to 0
        nodes' = map (fTime $ if temporal action then shiftTime $ startTime action - 1 else (\x -> (0,0))) nodes
        edges' = map (fTime $ if temporal action then shiftTime $ startTime action - 1 else (\x -> (0,0))) edges
    (outputF action) $ Graph ((nodeF action) nodes') edges'
  where
    toLatex (color, cname) = "\\definecolor{" ++ cname ++"}" ++ draw color
    shiftTime t (t0, t1) = (t0 + t, t1 + t)

main :: IO ()
main = do
  args <- getArgs
  let action = parseArgs defaultAction args :: Action Double Node
  execute action
