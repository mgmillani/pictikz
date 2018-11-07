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


module Pictikz.Graph where

import Pictikz.Elements
import qualified Pictikz.Geometry as G
import qualified Pictikz.Text as T
import qualified Pictikz.Output.Tikz as Tikz
import Pictikz.Parser
import Data.List

data Node a  = Node a a String [T.Text] [GraphStyle] (Int, Int) deriving (Show, Read, Eq, Ord)
data Edge    = Edge String String [GraphStyle] (Int, Int)       deriving (Show, Read, Eq, Ord)
data Graph a = Graph [Node a] [Edge]                            deriving (Show, Read, Eq, Ord)

instance Positionable Node where
  getPos (Node x y _ _ _ _)     = (x,y)
  fPos f (Node x y id name style time) = let (x1,y1) = f (x,y) in Node x1 y1 id name style time

instance Temporal (Node a) where
  getTime (Node _ _ _ _ _ time) = time
  fTime f (Node x y id name style time) = (Node x y id name style (f time))

instance Temporal Edge where
  getTime (Edge _ _ _ time) = time
  fTime f (Edge n1 n2 style time) = (Edge n1 n2 style (f time))

instance Tikz.Drawable GraphStyle where
  draw Dotted    = ", pictikz-dotted"
  draw Dashed    = ", pictikz-dashed"
  draw Thick     = ", pictikz-thick"
  draw Rectangle = ", pictikz-rectangle"
  draw Circle    = ", pictikz-node"
  draw (Fill c)  = ", fill=" ++ c
  draw (Stroke c)  = ", draw=" ++ c
  draw (Arrow ArrowNone) = ""
  draw (Arrow t) = ", " ++ show t
  draw LeftAligned  = ", left"
  draw RightAligned = ", right"
  draw Centered     = ", center"

instance (Num a, Show a) => Tikz.Drawable (Node a) where
  draw (Node x y id name style (t0, t1)) = concat
    [ if t1 > 0 then "\\uncover<" ++ show t0 ++ "-" ++ show t1 ++ ">{ " else ""
    , "\\node["
    , drop 2 $ concatMap Tikz.draw $ filter (\s -> not $ s `elem` [LeftAligned, Centered, RightAligned]) style
    , "] ("
    , id
    , ") at ("
    , show x
    , ", "
    , show y
    , ") [align=" ++ (drop 2 $ Tikz.draw alignment) ++ "]{"
    , escapeLines $ concatMap Tikz.draw name
    , "};"
    , if t1 > 0 then " }\n" else "\n"
    ]
    where
      escapeLines "\n" = []
      escapeLines ('\n':r) = "\\\\ " ++ escapeLines r
      escapeLines (a:r) = a : escapeLines r
      escapeLines [] = []
      alignment = head $ filter (\f -> f `elem` [LeftAligned, RightAligned, Centered]) (style ++ [LeftAligned])

instance Tikz.Drawable Edge where
  draw (Edge n1 n2 style (t0, t1)) = concat
    [ if t1 > 0 then "\\uncover<" ++ show t0 ++ "-" ++ show t1 ++">{ " else ""
    , "\\draw["
    , drop 2 $ concatMap Tikz.draw style
    , "] ("
    , n1
    , ") edge ("
    , n2
    , ");"
    , if t1 > 0 then " }\n" else "\n"
    ]

instance (Num a, Show a) => Tikz.Drawable (Graph a) where
  draw (Graph nodes edges) = concat $ map Tikz.draw nodes ++ map Tikz.draw edges

makeGraph layers colors  = map (makeGraph' colors) $ zip layers [0..]

makeGraph' colors ((Layer elements), t) = 
  let gnames = filter isText elements
      gnodes = assignNames (fixGraphStyle colors (fixIDs $ filter isObject elements)) gnames
      gedges = map (closest t gnodes) $ fixGraphStyle colors $ filter isLine elements
  in Graph (map (fPos (\(x,y) -> (x,-y))) (map (toNode t) gnodes)) (gedges) :: Graph Double

assignNames [] _ = []
assignNames gnodes [] = gnodes
assignNames gnodes (t:ts) =
  let (Object s iD n style) = assignName gnodes t
  in (Object s iD n style) : assignNames (filter (\(Object _ iD1 _ _) -> iD1 /= iD) gnodes) ts
assignName gnodes (Paragraph x0 y0 text format) =
  let dist s = G.squareDistance (x0,y0) s
      (Object s iD _ style) = minimumBy (\(Object s0 _ _ _) (Object s1 _ _ _) -> compare (dist s0) (dist s1) ) gnodes
      style' = format ++ style
  in (Object s iD text style')

fixGraphStyle :: (Ord a, Floating a) => [(Color, String)] -> [Element a] -> [Element a]
fixGraphStyle colors ls =
  let strokeWs = map (\(RawGraphStyle s v) -> v) $ filter (\(RawGraphStyle s v) -> s == Thick) $ concatMap getStyle ls
      minStroke = minimum strokeWs
      maxStroke = maximum strokeWs
      midStroke = minStroke + (maxStroke - minStroke) / 2
      fixLine arrow [] = [Parsed $ Arrow arrow]
      fixLine arrow (s:ss) = case s of
        RawGraphStyle Thick v   -> if v > midStroke && v > minStroke * 1.4 then Parsed Thick : fixLine arrow ss else (fixLine arrow ss)
        RawGraphStyle  Dashed    v -> (Parsed $ if v > 2*minStroke then Dashed else Dotted) : (fixLine arrow ss)
        RawGraphStyle (Arrow  a) _ -> fixLine (joinArrow a arrow) ss
        RawGraphStyle (Fill   c) _ ->
          let color = readColor c
              dists = map (\(c', n) -> (rgbDist c' color, n)) colors
              (_, cname) = minimumBy (\(c0, _) (c1, _) -> compare c0 c1) dists
          in Parsed (Fill cname) : fixLine arrow ss
        RawGraphStyle (Stroke c) _ ->
          let color = readColor c
              dists = map (\(c', n) -> (rgbDist c' color, n)) colors
              (_, cname) = minimumBy (\(c0, _) (c1, _) -> compare c0 c1) dists
          in Parsed (Stroke cname) : fixLine arrow ss
        RawGraphStyle s  _ -> Parsed s : (fixLine arrow ss)
  in map (fStyle (fixLine ArrowNone)) ls

fixIDs objs = zipWith (\(Object s iD name style) i -> if null iD then (Object s (show i) name style) else (Object s iD name style)) objs [1..]
closest t vertices (Line x0 y0 x1 y1 a) =
  let p0 = (x0, y0)
      p1 = (x1, y1)
      (n0,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _ _) -> (id, G.squareDistance p0 shape)) vertices
      (n1,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _ _) -> (id, G.squareDistance p1 shape)) vertices
  in Edge n0 n1 (map graphStyle a) (t,t)

toNode t (Object (G.Rectangle x y w h) id name style) = Node (x + w/2) (y + h/2) id name (map graphStyle style) (t,t)
toNode t (Object (G.Ellipsis x y _ _)  id name style) = Node x y id name (map graphStyle style) (t,t)
