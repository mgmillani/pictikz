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

module Pictikz.Loader (loadGraph, loadColors) where

--  import Prelude hiding (read)
--  import qualified Prelude as P (read)
import Data.Matrix hiding (trace)
import Pictikz.Geometry
import Pictikz.Drawing
import Pictikz.Parser
import qualified Pictikz.Graph as G
import Data.List
import Data.Char
import Data.Maybe
import qualified Debug.Trace as D (trace)

import qualified Text.XML.Light as X

data Element a b = Object (Shape a) String String [b] | Line a a a a [b] | Text a a String deriving (Show, Eq)

--  read x = (D.trace ("read:" ++ show x) $ P.read x)

isObject (Object _ _ _ _) = True
isObject _ = False
isLine (Line _ _ _ _ _) = True
isLine _ = False
isText (Text _ _ _) = True
isText _ = False

getStyle (Line _ _ _ _ style) = style
getStyle (Object _ _ _ style) = style

fStyle f (Line x0 y0 x1 y1 style) = Line x0 y0 x1 y1 (f style)
fStyle f (Object s iD name style) = Object s iD name (f style)

defaultText = Text 0 0 "ERROR"
defaultRectangle = (Rectangle 0 0 0 0, "", "", [(G.Rectangle, 0)], identity 3)
defaultEllipsis  = (Ellipsis  0 0 0 0, "", "", [(G.Circle, 0)], identity 3)
defaultGNode = G.Node 0 0 "" ""

loadGraph svg colors =
  let contents = X.parseXML svg
      elements = concatMap (parseElements (identity 3)) contents
      gnames = filter isText elements
      gnodes = assignNames (fixGraphStyle colors (fixIDs $ filter isObject elements)) gnames
      gedges = map (closest gnodes) $ fixGraphStyle colors $ filter isLine elements
  in G.Graph (map (fPos (\(x,y) -> (x,-y))) (map toNode gnodes)) gedges
  where
    parseElements matrix (X.Elem element)
      -- Objects
      | (X.qName $ X.elName element) `elem` ["rect"] =
        let (shape, id, name, style, m2) = foldl parseRectangle defaultRectangle $ X.elAttribs element
        in [transform (matrix * m2) (Object shape id name style)]
      | (X.qName $ X.elName element) `elem` ["ellipse", "circle"] =
        let (shape, id, name, style, m2) = foldl parseEllipsis defaultEllipsis $ X.elAttribs element
        in [transform (matrix * m2) (Object shape id name style)]
      -- Transformations
      | (X.qName $ X.elName element) `elem` ["defs"] = []
      | (X.qName $ X.elName element) `elem` ["g"] =
        let matrix' = foldl parseG matrix $ X.elAttribs element
        in concatMap (parseElements (matrix * matrix') ) $ X.elContent element
      -- Edges
      | (X.qName $ X.elName element) == "path" =  [transform matrix $ foldl parseEdge (Line 0 0 0 0 []) $ X.elAttribs element]
      -- Text
      | (X.qName $ X.elName element) == "text" =  [transform matrix $ parseName defaultText (X.Elem element)]
      | otherwise = concatMap (parseElements matrix ) $ X.elContent element
    parseElements matrix _ = []
    transform matrix ( Object (Rectangle x y w h) id name style) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
          [w1,h1,w2,h2,_,_] = toList $ matrix * (fromList 3 2 [w,0,0,h,0,0])
          w' = maximum [w1,h1,0] - minimum [w1,h1,0]
          h' = maximum [w2,h2,0] - minimum [w2,h2,0]
      in (Object (Rectangle x' y' w' h') id name style)
    transform matrix (Object (Ellipsis x y rx ry) id name style) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
          [rx1,ry1,rx2,ry2,_,_] = toList $ matrix * (fromList 3 2 [rx,0,0,ry,0,0])
          rx' = maximum [rx1,ry1,0] - minimum [rx1,ry1,0]
          ry' = maximum [rx2,ry2,0] - minimum [rx2,ry2,0]
      in (Object (Ellipsis x' y' rx' ry') id name style)
    transform matrix (Line x0 y0 x1 y1 a) =
      let [x0', x1', y0',y1',_, _] = toList $ matrix * (fromList 3 2 [x0,x1,y0,y1,1,1])
      in Line x0' y0' x1' y1' a
    transform matrix (Text x y str) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
      in (Text x' y' str)
    parseG matrix attr
      | "transform" == (X.qName $ X.attrKey attr) = matrix * (parseTransform $ X.attrVal attr)
      | otherwise = matrix
    parseRectangle (Rectangle x y w h, id, name, style, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Rectangle x y w h, (X.attrVal attr), name, style,  matrix)
      | "x"  == (X.qName $ X.attrKey attr) = (Rectangle (read $ X.attrVal attr :: Float) y w h, id, name, style, matrix)
      | "y"  == (X.qName $ X.attrKey attr) = (Rectangle x (read $ X.attrVal attr :: Float) w h, id, name, style, matrix)
      | "height" == (X.qName $ X.attrKey attr) = (Rectangle x y w (read $ X.attrVal attr :: Float), id, name, style, matrix)
      | "width"  == (X.qName $ X.attrKey attr) = (Rectangle x y (read $ X.attrVal attr :: Float) h, id, name, style, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Rectangle x y (read $ X.attrVal attr :: Float) h, id, name, style, matrix * (parseTransform $ X.attrVal attr))
      | "style"      == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style' = graphStyle fields
        in (Rectangle x y w h, id, name, style ++ style', matrix)
      | otherwise = (Rectangle x y w h, id, name, style, matrix)
    parseEllipsis (Ellipsis x y rx ry, id, name, style, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Ellipsis x y rx ry, (X.attrVal attr), name, style, matrix)
      | "cx" == (X.qName $ X.attrKey attr) = (Ellipsis (read $ X.attrVal attr :: Float) y rx ry, id, name, style, matrix)
      | "cy" == (X.qName $ X.attrKey attr) = (Ellipsis x (read $ X.attrVal attr :: Float) rx ry, id, name, style, matrix)
      | "rx" == (X.qName $ X.attrKey attr) = (Ellipsis x y (read $ X.attrVal attr :: Float)  ry, id, name, style, matrix)
      | "ry" == (X.qName $ X.attrKey attr) = (Ellipsis x y rx (read $ X.attrVal attr :: Float) , id, name, style, matrix)
      | "r"  == (X.qName $ X.attrKey attr) = (Ellipsis x y (read $ X.attrVal attr :: Float) (read $ X.attrVal attr :: Float), id, name, style, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Ellipsis x y rx ry, id, name, style, matrix * (parseTransform $ X.attrVal attr))
      | "style"      == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style' = graphStyle fields
        in (Ellipsis x y rx ry, id, name, style ++ style', matrix)
      | otherwise = (Ellipsis x y rx ry, id, name, style, matrix)
    toNode (Object (Rectangle x y w h) id name style) = G.Node (x + w/2) (y + h/2) id name style
    toNode (Object (Ellipsis x y _ _)  id name style) = G.Node x y id name style
    parseName (Text x y n) (X.Text text) = (Text x y (X.cdData text))
    parseName (Text _ _ n) (X.Elem element) =
      let (x,y, matrix) = foldl parseCoordinates (0,0, identity 3) $ X.elAttribs element
      in  foldl parseName (Text x y n) $ X.elContent element
    parseEdge (Line xa ya xb yb a) attr
      | "d" == (X.qName $ X.attrKey attr) =
        let path = parsePath $ X.attrVal attr
            ((x0,y0), (x1,y1)) = ((head path :: (Float, Float)), (last path :: (Float, Float)))
        in Line x0 y0 x1 y1 a
      | "style"        == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style = graphStyle fields
        in Line xa ya xb yb style
      | otherwise = Line xa ya xb yb a
    graphStyle :: [(String, String)] -> [(G.Style, Float)]
    graphStyle [] = []
    graphStyle (x:xs) = case x of
      ("marker-end", "none")        -> (G.Arrow G.ArrowNone, undefined) : graphStyle xs
      ("marker-end", _)             -> (G.Arrow G.ArrowTo,   undefined) : graphStyle xs
      ("marker-start", "none")      -> (G.Arrow G.ArrowNone, undefined) : graphStyle xs
      ("marker-start", _)           -> (G.Arrow G.ArrowFrom, undefined) : graphStyle xs
      ("fill", "none")              -> graphStyle xs
      ("fill", color)               -> (G.Fill color,  undefined) : graphStyle xs
      ("stroke", "none")            -> graphStyle xs
      ("stroke", color)             -> (G.Stroke color,  undefined) : graphStyle xs
      ("stroke-width", len)         -> (G.Thick, (readLength len)) : graphStyle xs
      ("stroke-dasharray", "none")  -> graphStyle xs
      ("stroke-dasharray", dashes)  -> let dash = takeWhile (/=',') dashes in (G.Dashed, (read dash :: Float)) : graphStyle xs
      _ -> graphStyle xs
    fixGraphStyle :: [(Color, String)] -> [Element Float (G.Style, Float)] -> [Element Float G.Style]
    fixGraphStyle colors ls =
      let strokeWs = map snd $ filter (\(s,v) -> s == G.Thick) $ concatMap getStyle ls
          minStroke = minimum strokeWs
          maxStroke = maximum strokeWs
          midStroke = minStroke + (maxStroke - minStroke) / 2
          fixLine arrow [] = [G.Arrow arrow]
          fixLine arrow (s:ss) = case s of
            (G.Thick, v)   -> if v > midStroke && v > minStroke * 1.4 then G.Thick : fixLine arrow ss else (fixLine arrow ss)
            (G.Dashed, v)  -> (if v > 2*minStroke then G.Dashed else G.Dotted) : (fixLine arrow ss)
            (G.Arrow a, _) -> fixLine (G.joinArrow a arrow) ss
            (G.Fill c, _)  ->
              let color = readColor c
                  dists = map (\(c', n) -> (rgbDist c' color, n)) colors
                  (_, cname) = minimumBy (\(c0, _) (c1, _) -> compare c0 c1) dists
              in G.Fill cname : fixLine arrow ss
            (G.Stroke c, _)  ->
              let color = readColor c
                  dists = map (\(c', n) -> (rgbDist c' color, n)) colors
                  (_, cname) = minimumBy (\(c0, _) (c1, _) -> compare c0 c1) dists
              in G.Stroke cname : fixLine arrow ss
            (s, _) -> s : (fixLine arrow ss)
      in map (fStyle (fixLine G.ArrowNone)) ls
      --in map (\(Line x0 y0 x1 y1 s) -> Line x0 y0 x1 y1 (fixLine G.ArrowNone s)) ls
    parseCoordinates (x,y, matrix) attr
      | "x" == (X.qName $ X.attrKey attr) = ((read $ X.attrVal attr :: Float), y, matrix)
      | "y" == (X.qName $ X.attrKey attr) = (x,(read $ X.attrVal attr :: Float), matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (x,y, matrix * (parseTransform $ X.attrVal attr))
      | otherwise = (x,y, matrix)
    fixIDs objs = zipWith (\(Object s iD name style) i -> if null iD then (Object s (show i) name style) else (Object s iD name style)) objs [1..]
    assignNames [] _ = []
    assignNames gnodes [] = gnodes
    assignNames gnodes (t:ts) =
      let (Object s iD n style) = assignName gnodes t
      in (Object s iD n style) : assignNames (filter (\(Object _ iD1 _ _) -> iD1 /= iD) gnodes) ts
    assignName gnodes (Text x0 y0 n) =
      let dist s = squareDistance (x0,y0) s
          (Object s iD _ style) = minimumBy (\(Object s0 _ _ _) (Object s1 _ _ _) -> compare (dist s0) (dist s1) ) gnodes
      in (Object s iD n style)
    closest vertices (Line x0 y0 x1 y1 a) =
      let p0 = (x0, y0)
          p1 = (x1, y1)
          (n0,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _ _) -> (id, squareDistance p0 shape)) vertices
          (n1,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _ _) -> (id, squareDistance p1 shape)) vertices
      in G.Edge n0 n1 a

loadColors str = map (getColor . words) $ lines str
  where
    getColor (name:space:value)
      | (map toLower space) == "rgb" =
        let (r,g,b) = parseValue value 255 255 255
        in (RGB r g b, name)
      | (map toLower space) == "hsl" =
        let (h,s,l) = parseValue value 360 100 100
        in (fromHSL (fromIntegral h) (fromIntegral s / 100) (fromIntegral l / 100), name)
    parseValue (('#':hex):_) _ _ _ = readHexa hex
    parseValue (x:y:z:_) mx my mz =
      let [x1,y1,z1] = zipWith parseNumber [x, y, z] [mx, my, mz] in (x1, y1, z1)
    parseNumber x mx
      | '.' `elem` x = round $ (read x :: Float) * mx
      | otherwise = read x :: Int
