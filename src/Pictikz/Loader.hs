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

module Pictikz.Loader where

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

data Element a b = Object (Shape a) String String | Line a a a a [b] | Text a a String deriving (Show, Eq)

isObject (Object _ _ _) = True
isObject _ = False
isLine (Line _ _ _ _ _) = True
isLine _ = False
isText (Text _ _ _) = True
isText _ = False

lineStyle (Line _ _ _ _ style) = style

defaultText = Text 0 0 "ERROR"
defaultRectangle = (Rectangle 0 0 0 0, "", "", identity 3)
defaultEllipsis  = (Ellipsis  0 0 0 0, "", "", identity 3)
defaultGNode = G.Node 0 0 "" ""

loadGraph svg =
  let contents = X.parseXML svg
      elements = concatMap (parseElements (identity 3)) contents
      gnames = filter isText elements
      gnodes = assignNames (filter isObject elements) gnames
      gedges = map (closest gnodes) $ fixEdgeStyle $ filter isLine elements
  in G.Graph (map (fPos (\(x,y) -> (x,-y))) (map toNode gnodes)) gedges
  where
    parseElements matrix (X.Elem element)
      -- Objects
      | (X.qName $ X.elName element) `elem` ["rect"] =
        let (shape, id, name, m2) = foldl parseRectangle defaultRectangle $ X.elAttribs element
        in [transform (matrix * m2) (Object shape id name)]
      | (X.qName $ X.elName element) `elem` ["ellipse", "circle"] =
        let (shape, id, name, m2) = foldl parseEllipsis defaultEllipsis $ X.elAttribs element
        in [transform (matrix * m2) (Object shape id name)]
      -- Transformations
      | (X.qName $ X.elName element) `elem` ["defs"] = []
      | (X.qName $ X.elName element) `elem` ["g"] =
        let matrix' = foldl parseG matrix $ X.elAttribs element
        in concatMap (parseElements (matrix * matrix') ) $ X.elContent element
      -- Edges
      | (X.qName $ X.elName element) == "path" =  [transform matrix $ foldl parseEdge (Line 0 0 0 0 []) $ X.elAttribs element]
      -- Text
      | (X.qName $ X.elName element) == "text" =  [transform matrix $ parseName $ X.elContent element]
      | otherwise = concatMap (parseElements matrix ) $ X.elContent element
    parseElements matrix _ = []
    transform matrix ( Object (Rectangle x y w h) id name) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
          [w1,h1,w2,h2,_,_] = toList $ matrix * (fromList 3 2 [w,0,0,h,0,0])
          w' = maximum [w1,h1,0] - minimum [w1,h1,0]
          h' = maximum [w2,h2,0] - minimum [w2,h2,0]
      in (Object (Rectangle x' y' w' h') id name)
    transform matrix (Object (Ellipsis x y rx ry) id name) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
          [rx1,ry1,rx2,ry2,_,_] = toList $ matrix * (fromList 3 2 [rx,0,0,ry,0,0])
          rx' = maximum [rx1,ry1,0] - minimum [rx1,ry1,0]
          ry' = maximum [rx2,ry2,0] - minimum [rx2,ry2,0]
      in (Object (Ellipsis x' y' rx' ry') id name)
    transform matrix (Line x0 y0 x1 y1 a) =
      let [x0', y0',_, x1', y1', _] = toList $ matrix * (fromList 3 2 [x0,y0,1, x1,y1,1])
      in Line x0' y0' x1' y1' a
    transform matrix (Text x y str) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
      in (Text x' y' str)
    parseG matrix attr
      | "transform" == (X.qName $ X.attrKey attr) = matrix * (parseTransform $ X.attrVal attr)
      | otherwise = matrix
    parseRectangle (Rectangle x y w h, id, name, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Rectangle x y w h, (X.attrVal attr), name, matrix)
      | "x"  == (X.qName $ X.attrKey attr) = (Rectangle (read $ X.attrVal attr :: Float) y w h, id, name, matrix)
      | "y"  == (X.qName $ X.attrKey attr) = (Rectangle x (read $ X.attrVal attr :: Float) w h, id, name, matrix)
      | "height" == (X.qName $ X.attrKey attr) = (Rectangle x y w (read $ X.attrVal attr :: Float), id, name, matrix)
      | "width"  == (X.qName $ X.attrKey attr) = (Rectangle x y (read $ X.attrVal attr :: Float) h, id, name, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Rectangle x y (read $ X.attrVal attr :: Float) h, id, name, matrix * (parseTransform $ X.attrVal attr))
      | otherwise = (Rectangle x y w h, id, name, matrix)
    parseEllipsis (Ellipsis x y rx ry, id, name, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Ellipsis x y rx ry, (X.attrVal attr), name, matrix)
      | "cx" == (X.qName $ X.attrKey attr) = (Ellipsis (read $ X.attrVal attr :: Float) y rx ry, id, name, matrix)
      | "cy" == (X.qName $ X.attrKey attr) = (Ellipsis x (read $ X.attrVal attr :: Float) rx ry, id, name, matrix)
      | "rx" == (X.qName $ X.attrKey attr) = (Ellipsis x y (read $ X.attrVal attr :: Float)  ry, id, name, matrix)
      | "ry" == (X.qName $ X.attrKey attr) = (Ellipsis x y rx (read $ X.attrVal attr :: Float) , id, name, matrix)
      | "r"  == (X.qName $ X.attrKey attr) = (Ellipsis x y (read $ X.attrVal attr :: Float) (read $ X.attrVal attr :: Float), id, name, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Ellipsis x y rx ry, id, name, matrix * (parseTransform $ X.attrVal attr))
      | otherwise = (Ellipsis x y rx ry, id, name, matrix)
    toNode (Object (Rectangle x y w h) id name) = G.Node (x + w/2) (y + h/2) id name [G.Rectangle]
    toNode (Object (Ellipsis x y _ _)  id name) = G.Node x y id name []
    parseName [] = defaultText
    parseName ((X.Elem element):xs)
      | (X.qName $ X.elName element) == "tspan" =
        let (x,y, matrix) = foldl parseCoordinates (0,0, identity 3) $ X.elAttribs element
            getName (X.Text cdata) = Left $ X.cdData cdata
            getName _ = Right ""
            name  = foldl (>>) (Right "") $ map getName $ X.elContent element
        in case name of
          Left  n -> Text x y n
          Right _ -> defaultText
      | otherwise = parseName xs
    parseEdge (Line xa ya xb yb a) attr
      | "d" == (X.qName $ X.attrKey attr) =
        let path = parsePath $ X.attrVal attr
            ((x0,y0), (x1,y1)) = ((head path :: (Float, Float)), (last path :: (Float, Float)))
        in Line x0 y0 x1 y1 a
      | "style"        == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style = edgeStyle fields
        in Line xa ya xb yb style
      | otherwise = Line xa ya xb yb a
    edgeStyle :: [(String, String)] -> [(G.EdgeStyle, Float)]
    edgeStyle [] = []
    edgeStyle (x:xs) = case x of
      ("marker-end", "none")        -> (G.Arrow G.ArrowNone, undefined) : edgeStyle xs
      ("marker-end", _)             -> (G.Arrow G.ArrowTo,   undefined) : edgeStyle xs
      ("marker-start", "none")      -> (G.Arrow G.ArrowNone, undefined) : edgeStyle xs
      ("marker-start", _)           -> (G.Arrow G.ArrowFrom, undefined) : edgeStyle xs
      ("stroke-width", len)         -> (G.Thick, (readLength len)) : edgeStyle xs
      ("stroke-dasharray", "none")  -> edgeStyle xs
      ("stroke-dasharray", dashes)  -> let dash = takeWhile (/=',') dashes in (G.Dashed, (read dash :: Float)) : edgeStyle xs
      _ -> edgeStyle xs
    fixEdgeStyle ls =
      let strokeWs = map snd $ filter (\(s,v) -> s == G.Thick) $ concatMap lineStyle ls
          minStroke = minimum strokeWs
          maxStroke = maximum strokeWs
          midStroke = minStroke + (maxStroke - minStroke) / 2
          fixLine arrow [] = [G.Arrow arrow]
          fixLine arrow (s:ss) = case s of
            (G.Thick, v)   -> if v > midStroke && v > minStroke * 1.4 then G.Thick : fixLine arrow ss else (fixLine arrow ss)
            (G.Dashed, v)  -> (if v > 2*minStroke then G.Dashed else G.Dotted) : (fixLine arrow ss)
            (G.Arrow a, _) -> fixLine (G.joinArrow a arrow) ss
      in map (\(Line x0 y0 x1 y1 s) -> Line x0 y0 x1 y1 (fixLine G.ArrowNone s)) ls
    parseCoordinates (x,y, matrix) attr
      | "x" == (X.qName $ X.attrKey attr) = ((read $ X.attrVal attr :: Float), y, matrix)
      | "y" == (X.qName $ X.attrKey attr) = (x,(read $ X.attrVal attr :: Float), matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (x,y, matrix * (parseTransform $ X.attrVal attr))
      | otherwise = (x,y, matrix)
    assignNames [] _ = []
    assignNames gnodes [] = gnodes
    assignNames gnodes (t:ts) =
      let (Object s iD n) = assignName gnodes t
      in (Object s iD n) : assignNames (filter (\(Object _ iD1 _) -> iD1 /= iD) gnodes) ts
    assignName gnodes (Text x0 y0 n) =
      let dist s = squareDistance (x0,y0) s
          (Object s iD _ ) = minimumBy (\(Object s0 _ _) (Object s1 _ _) -> compare (dist s0) (dist s1) ) gnodes
      in (Object s iD n )
    closest vertices (Line x0 y0 x1 y1 a) =
      let p0 = (x0, y0)
          p1 = (x1, y1)
          (n0,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _) -> (id, squareDistance p0 shape)) vertices
          (n1,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _) -> (id, squareDistance p1 shape)) vertices
      in G.Edge n0 n1 a
