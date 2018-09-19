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

module Pictikz.Loader (loadGraph, loadColors, renameNodes, textAsNodes, Element) where

--  import Prelude hiding (read)
--  import qualified Prelude as P (read)
import Data.Matrix hiding (trace)
import Pictikz.Geometry
import Pictikz.Drawing
import Pictikz.Parser
import Pictikz.XML
import qualified Pictikz.Graph as G
import qualified Pictikz.Text  as T
import Data.List
import Data.Char
import Data.Maybe
import qualified Debug.Trace as D (trace)

import qualified Text.XML.Light as X

data Style = RawTextStyle T.Format | RawGraphStyle G.Style Double  | Parsed G.Style deriving (Show, Eq, Read)

data Element a =
  Object (Shape a) String [T.Text] [Style]
  | Line a a a a [Style]
  | Paragraph a a [T.Text] [Style]
  | Layer [Element a] deriving (Eq)

instance Show a => Show (Element a) where
  show (Object shape iD name style) = concat [iD, ": ", show shape]
  show (Line x0 y0 x1 y1 _) = concat [show (x0,y0), " -- ", show (x1, y1)]
  show (Layer els) = concatMap (\x -> show x ++ "\n") els
  show (Paragraph x y text style) = "Par:" ++ show (x,y)

--  read x = (D.trace ("read:" ++ show x) $ P.read x)

isObject (Object _ _ _ _) = True
isObject _                = False
isLine (Line _ _ _ _ _) = True
isLine _                = False
isText (Paragraph _ _ _ _) = True
isText _                   = False
isLayer (Layer _) = True
isLayer _         = False

getStyle (Line _ _ _ _ style) = style
getStyle (Object _ _ _ style) = style

fStyle f (Line x0 y0 x1 y1 style) = Line x0 y0 x1 y1 (f style)
fStyle f (Object s iD name style) = Object s iD name (f style)

defaultText = T.Text "" []
defaultPar  = Paragraph 0 0 [] []
defaultRectangle = (Rectangle 0 0 0 0, "", [], [RawGraphStyle G.Rectangle 0], identity 3)
defaultEllipsis  = (Ellipsis  0 0 0 0, "", [], [RawGraphStyle G.Circle    0], identity 3)
defaultGNode = G.Node 0 0 "" []

textAsNodes elements = fst $ textAsNodes' elements 1
  where
    textAsNodes' [] i = ([], i)
    textAsNodes' (e:es) i =
      case e of
        Paragraph x y ts format ->
          let (rs, n) = textAsNodes' es (i+1)
          in ((Object (Rectangle x y 1 1) ("text-" ++ show i) ts format) : rs, n)
        Layer ls ->
          let (rs, i')  = textAsNodes' es i
              (ls', n) = textAsNodes' ls i'
          in ((Layer ls') : rs, n)
        x ->
          let (rs, n) = textAsNodes' es i
          in (x : rs, n)

renameNodes elements = fst $ renameNodes' elements 1
  where
    renameNodes' [] i = ([], i)
    renameNodes' (e:es) i =
      case e of
        Object shape iD name style ->
          let (rs, n) = renameNodes' es (i+1)
          in (Object shape ("v" ++ show i) name style : rs, n)
        Layer ls ->
          let (rs, i')  = renameNodes' es i
              (ls', n) = renameNodes' ls i'
          in ((Layer ls') : rs, n)
        x ->
          let (rs, n) = renameNodes' es i
          in (x : rs, n)

graphStyle s = case s of
  Parsed s' -> s'
  RawTextStyle T.Centered     -> G.Centered
  RawTextStyle T.LeftAligned  -> G.LeftAligned
  RawTextStyle T.RightAligned -> G.RightAligned
  RawGraphStyle s' _ -> s'

loadGraph svg colors preprocess =
  let contents = X.parseXML svg
      elements = preprocess $ concatMap (parseElements (identity 3)) contents
      layers = filter isLayer elements
      layer0 = filter (not . isLayer) elements
      gr = map makeGraph $ zip ((Layer layer0) : layers) [0..]
  in gr
  where
    makeGraph ((Layer elements), t) =
      let gnames = filter isText elements
          gnodes = assignNames (fixGraphStyle colors (fixIDs $ filter isObject elements)) gnames
          gedges = map (closest t gnodes) $ fixGraphStyle colors $ filter isLine elements
      in G.Graph (map (fPos (\(x,y) -> (x,-y))) (map (toNode t) gnodes)) (gedges) :: G.Graph Double
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
            rest = concatMap (parseElements (matrix') ) $ X.elContent element
        in if satisfyAttrib element "id" (\v -> "layer" `isPrefixOf` (map toLower v))
           then [Layer rest]
           else if satisfyAttrib element "groupmode" (=="layer")
           then [Layer rest]
           else rest
      -- Edges
      | (X.qName $ X.elName element) == "path" =  [transform matrix $ foldl parseEdge (Line 0 0 0 0 []) $ X.elAttribs element]
      -- Text
      | (X.qName $ X.elName element) == "text" = [transform matrix $ parseParagraph defaultPar (X.Elem element)]
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
    transform matrix (Paragraph x y text format) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
      in (Paragraph x' y' text format)
    parseG matrix attr
      | "transform" == (X.qName $ X.attrKey attr) = matrix * (parseTransform $ X.attrVal attr)
      | otherwise = matrix
    parseRectangle (Rectangle x y w h, id, name, style, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Rectangle x y w h, (X.attrVal attr), name, style,  matrix)
      | "x"  == (X.qName $ X.attrKey attr) = (Rectangle (read $ X.attrVal attr ) y w h, id, name, style, matrix)
      | "y"  == (X.qName $ X.attrKey attr) = (Rectangle x (read $ X.attrVal attr ) w h, id, name, style, matrix)
      | "height" == (X.qName $ X.attrKey attr) = (Rectangle x y w (read $ X.attrVal attr ), id, name, style, matrix)
      | "width"  == (X.qName $ X.attrKey attr) = (Rectangle x y (read $ X.attrVal attr ) h, id, name, style, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Rectangle x y w h, id, name, style, matrix * (parseTransform $ X.attrVal attr))
      | "style"      == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style' = rawGraphStyle fields
        in (Rectangle x y w h, id, name, style ++ style', matrix)
      | otherwise = (Rectangle x y w h, id, name, style, matrix)
    parseEllipsis (Ellipsis x y rx ry, id, name, style, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Ellipsis x y rx ry, (X.attrVal attr), name, style, matrix)
      | "cx" == (X.qName $ X.attrKey attr) = (Ellipsis (read $ X.attrVal attr ) y rx ry, id, name, style, matrix)
      | "cy" == (X.qName $ X.attrKey attr) = (Ellipsis x (read $ X.attrVal attr ) rx ry, id, name, style, matrix)
      | "rx" == (X.qName $ X.attrKey attr) = (Ellipsis x y (read $ X.attrVal attr )  ry, id, name, style, matrix)
      | "ry" == (X.qName $ X.attrKey attr) = (Ellipsis x y rx (read $ X.attrVal attr ) , id, name, style, matrix)
      | "r"  == (X.qName $ X.attrKey attr) = (Ellipsis x y (read $ X.attrVal attr ) (read $ X.attrVal attr ), id, name, style, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Ellipsis x y rx ry, id, name, style, matrix * (parseTransform $ X.attrVal attr))
      | "style"      == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style' = rawGraphStyle fields
        in (Ellipsis x y rx ry, id, name, style ++ style', matrix)
      | otherwise = (Ellipsis x y rx ry, id, name, style, matrix)
    toNode t (Object (Rectangle x y w h) id name style) = G.Node (x + w/2) (y + h/2) id name (map graphStyle style) (t,t)
    toNode t (Object (Ellipsis x y _ _)  id name style) = G.Node x y id name (map graphStyle style) (t,t)
    -- Paragraph
    parseParagraph (Paragraph _ _ text format) (X.Elem element) =
      let (x,y, matrix) = foldl parseCoordinates (0,0, identity 3) $ X.elAttribs element
          style = fromMaybe [] $ X.findAttr (X.blank_name{X.qName = "style"}) element
          format' = map RawTextStyle $ parseFormat $ parseStyle style
          text'   = concatMap (parseName defaultText y) $ X.elContent element
      in transform matrix $ Paragraph x y (text ++ text') (format' ++ format)
    parseName (T.Text str format) y (X.Text text) = [T.Text (str ++ X.cdData text) format]
    parseName (T.Text str format) y (X.Elem element) =
      let style = fromMaybe [] $ X.findAttr (X.blank_name{X.qName = "style"}) element
          y' = fromMaybe y $ fmap read $ X.findAttr (X.blank_name{X.qName = "y"}) element
          newline = if y' == y then "" else "\n"
          format' = parseFormat $ parseStyle $ style
          rs = X.elContent element
          str' = dropWhile isSpace str
          text' = if null str' then [] else [T.Text (newline ++ str) (format' ++ format)]
      in if null rs then text'
         else
          text' ++ (map (\(T.Text s fm) -> T.Text (newline ++ s) fm) $ concatMap (parseName (T.Text "" (format' ++ format)) y') rs)
    parseFormat [] = []
    parseFormat ((var, val):as)
      | var == "text-align" =
        case val of
          "center" -> T.Centered
          "end"    -> T.RightAligned
          "start"  -> T.LeftAligned
        : parseFormat as
      | var == "font-weight" =
        case val of
          "bold"   -> T.Bold : parseFormat as
          "italic" -> T.Italics : parseFormat as
          "normal" -> parseFormat as        
      | var == "baseline-shift" =
        case val of
          "sub"   -> T.Subscript : parseFormat as
          "super" -> T.Superscript : parseFormat as
          "baseline" -> parseFormat as
      | otherwise = parseFormat as
    -- Edge
    parseEdge (Line xa ya xb yb a) attr
      | "d" == (X.qName $ X.attrKey attr) =
        let path = parsePath $ X.attrVal attr
            ((x0,y0), (x1,y1)) = ((head path), (last path))
        in Line x0 y0 x1 y1 a
      | "style"        == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style = rawGraphStyle fields
        in Line xa ya xb yb style
      | otherwise = Line xa ya xb yb a
    rawGraphStyle :: [(String, String)] -> [Style]
    rawGraphStyle [] = []
    rawGraphStyle (x:xs) = case x of
      ("marker-end", "none")        -> RawGraphStyle (G.Arrow G.ArrowNone) undefined : rawGraphStyle xs
      ("marker-end", _)             -> RawGraphStyle (G.Arrow G.ArrowTo)   undefined : rawGraphStyle xs
      ("marker-start", "none")      -> RawGraphStyle (G.Arrow G.ArrowNone) undefined : rawGraphStyle xs
      ("marker-start", _)           -> RawGraphStyle (G.Arrow G.ArrowFrom) undefined : rawGraphStyle xs
      ("fill", "none")              -> rawGraphStyle xs
      ("fill", color)               -> RawGraphStyle (G.Fill color)        undefined : rawGraphStyle xs
      ("stroke", "none")            -> rawGraphStyle xs
      ("stroke", color)             -> RawGraphStyle (G.Stroke color)      undefined : rawGraphStyle xs
      ("stroke-width", len)         -> RawGraphStyle G.Thick        (readLength len) : rawGraphStyle xs
      ("stroke-dasharray", "none")  -> rawGraphStyle xs
      ("stroke-dasharray", dashes)  -> let dash = takeWhile (/=',') dashes in RawGraphStyle G.Dashed (read dash) : rawGraphStyle xs
      _ -> rawGraphStyle xs
    fixGraphStyle :: (Ord a, Floating a) => [(Color, String)] -> [Element a] -> [Element a]
    fixGraphStyle colors ls =
      let strokeWs = map (\(RawGraphStyle s v) -> v) $ filter (\(RawGraphStyle s v) -> s == G.Thick) $ concatMap getStyle ls
          minStroke = minimum strokeWs
          maxStroke = maximum strokeWs
          midStroke = minStroke + (maxStroke - minStroke) / 2
          fixLine arrow [] = [Parsed $ G.Arrow arrow]
          fixLine arrow (s:ss) = case s of
            RawGraphStyle G.Thick v   -> if v > midStroke && v > minStroke * 1.4 then Parsed G.Thick : fixLine arrow ss else (fixLine arrow ss)
            RawGraphStyle  G.Dashed    v -> (Parsed $ if v > 2*minStroke then G.Dashed else G.Dotted) : (fixLine arrow ss)
            RawGraphStyle (G.Arrow  a) _ -> fixLine (G.joinArrow a arrow) ss
            RawGraphStyle (G.Fill   c) _ ->
              let color = readColor c
                  dists = map (\(c', n) -> (rgbDist c' color, n)) colors
                  (_, cname) = minimumBy (\(c0, _) (c1, _) -> compare c0 c1) dists
              in Parsed (G.Fill cname) : fixLine arrow ss
            RawGraphStyle (G.Stroke c) _ ->
              let color = readColor c
                  dists = map (\(c', n) -> (rgbDist c' color, n)) colors
                  (_, cname) = minimumBy (\(c0, _) (c1, _) -> compare c0 c1) dists
              in Parsed (G.Stroke cname) : fixLine arrow ss
            RawGraphStyle s  _ -> Parsed s : (fixLine arrow ss)
      in map (fStyle (fixLine G.ArrowNone)) ls
    parseCoordinates (x,y, matrix) attr
      | "x" == (X.qName $ X.attrKey attr) = ((read $ X.attrVal attr), y, matrix)
      | "y" == (X.qName $ X.attrKey attr) = (x,(read $ X.attrVal attr), matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (x,y, matrix * (parseTransform $ X.attrVal attr))
      | otherwise = (x,y, matrix)
    fixIDs objs = zipWith (\(Object s iD name style) i -> if null iD then (Object s (show i) name style) else (Object s iD name style)) objs [1..]
    assignNames [] _ = []
    assignNames gnodes [] = gnodes
    assignNames gnodes (t:ts) =
      let (Object s iD n style) = assignName gnodes t
      in (Object s iD n style) : assignNames (filter (\(Object _ iD1 _ _) -> iD1 /= iD) gnodes) ts
    assignName gnodes (Paragraph x0 y0 text format) =
      let dist s = squareDistance (x0,y0) s
          (Object s iD _ style) = minimumBy (\(Object s0 _ _ _) (Object s1 _ _ _) -> compare (dist s0) (dist s1) ) gnodes
          style' = format ++ style
      in (Object s iD text style')
    closest t vertices (Line x0 y0 x1 y1 a) =
      let p0 = (x0, y0)
          p1 = (x1, y1)
          (n0,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _ _) -> (id, squareDistance p0 shape)) vertices
          (n1,_) = minimumBy (\p q -> compare (snd p) (snd q)) $ map (\(Object shape id _ _) -> (id, squareDistance p1 shape)) vertices
      in G.Edge n0 n1 (map graphStyle a) (t,t)

loadColors str = map (getColor . words) $ lines str
  where
    getColor (name:space:value)
      | (map toLower space) == "rgb" =
        let (r,g,b) = parseValue value 255 255 255
        in (RGB r g b, name)
      | (map toLower space) == "hsl" =
        let (h,s,l) = parseValue value 359 100 100
        in (fromHSL (fromIntegral h) (fromIntegral s / 100) (fromIntegral l / 100), name)
    parseValue (('#':hex):_) _ _ _ = readHexa hex
    parseValue (x:y:z:_) mx my mz =
      let [x1,y1,z1] = zipWith parseNumber [x, y, z] [mx, my, mz] in (x1, y1, z1)
    parseNumber x mx
      | '.' `elem` x = round $ (read x) * mx
      | otherwise = read x :: Int
