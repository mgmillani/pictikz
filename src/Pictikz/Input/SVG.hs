module Pictikz.Input.SVG where

import           Pictikz.Elements
import           Pictikz.Parser
import           Pictikz.XML
import qualified Pictikz.Geometry as Ge
import qualified Pictikz.Graph as G
import qualified Pictikz.Text  as T

import Data.Char
import Data.List
import Data.Matrix hiding (trace)
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State

import qualified Debug.Trace as D (trace)

import qualified Text.XML.Light as X

defaultGNode = G.Node 0 0 "" []
defaultText = T.Text "" []
defaultPar  = Paragraph 0 0 [] []
defaultRectangle = (Ge.Rectangle 0 0 0 0, "", [], [RawGraphStyle Rectangle 0], identity 3)
defaultEllipsis  = (Ge.Ellipsis  0 0 0 0, "", [], [RawGraphStyle Circle    0], identity 3)

loadElements svg colors preprocess =
  let contents = X.parseXML svg
      elements = preprocess $ concatMap (parseElements (identity 3)) contents
      layers = filter isLayer elements
      layer0 = filter (not . isLayer) elements
  in ((Layer layer0) : layers)
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
    transform matrix ( Object (Ge.Rectangle x y w h) id name style) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
          [w1,h1,w2,h2,_,_] = toList $ matrix * (fromList 3 2 [w,0,0,h,0,0])
          w' = maximum [w1,h1,0] - minimum [w1,h1,0]
          h' = maximum [w2,h2,0] - minimum [w2,h2,0]
      in (Object (Ge.Rectangle x' y' w' h') id name style)
    transform matrix (Object (Ge.Ellipsis x y rx ry) id name style) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
          [rx1,ry1,rx2,ry2,_,_] = toList $ matrix * (fromList 3 2 [rx,0,0,ry,0,0])
          rx' = maximum [rx1,ry1,0] - minimum [rx1,ry1,0]
          ry' = maximum [rx2,ry2,0] - minimum [rx2,ry2,0]
      in (Object (Ge.Ellipsis x' y' rx' ry') id name style)
    transform matrix (Line x0 y0 x1 y1 a) =
      let [x0', x1', y0',y1',_, _] = toList $ matrix * (fromList 3 2 [x0,x1,y0,y1,1,1])
      in Line x0' y0' x1' y1' a
    transform matrix (Paragraph x y text format) =
      let [x', y',_] = toList $ matrix * (fromList 3 1 [x,y,1])
      in (Paragraph x' y' text format)
    parseG matrix attr
      | "transform" == (X.qName $ X.attrKey attr) = matrix * (parseTransform $ X.attrVal attr)
      | otherwise = matrix
    parseRectangle (Ge.Rectangle x y w h, id, name, style, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Ge.Rectangle x y w h, (X.attrVal attr), name, style,  matrix)
      | "x"  == (X.qName $ X.attrKey attr) = (Ge.Rectangle (read $ X.attrVal attr ) y w h, id, name, style, matrix)
      | "y"  == (X.qName $ X.attrKey attr) = (Ge.Rectangle x (read $ X.attrVal attr) w h, id, name, style, matrix)
      | "height" == (X.qName $ X.attrKey attr) = (Ge.Rectangle x y w (read $ X.attrVal attr ), id, name, style, matrix)
      | "width"  == (X.qName $ X.attrKey attr) = (Ge.Rectangle x y (read $ X.attrVal attr ) h, id, name, style, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Ge.Rectangle x y w h, id, name, style, matrix * (parseTransform $ X.attrVal attr))
      | "style"      == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style' = rawGraphStyle fields
        in (Ge.Rectangle x y w h, id, name, style ++ style', matrix)
      | otherwise = (Ge.Rectangle x y w h, id, name, style, matrix)
    parseEllipsis (Ge.Ellipsis x y rx ry, id, name, style, matrix) attr
      | "id" == (X.qName $ X.attrKey attr) = (Ge.Ellipsis x y rx ry, (X.attrVal attr), name, style, matrix)
      | "cx" == (X.qName $ X.attrKey attr) = (Ge.Ellipsis (read $ X.attrVal attr ) y rx ry, id, name, style, matrix)
      | "cy" == (X.qName $ X.attrKey attr) = (Ge.Ellipsis x (read $ X.attrVal attr ) rx ry, id, name, style, matrix)
      | "rx" == (X.qName $ X.attrKey attr) = (Ge.Ellipsis x y (read $ X.attrVal attr )  ry, id, name, style, matrix)
      | "ry" == (X.qName $ X.attrKey attr) = (Ge.Ellipsis x y rx (read $ X.attrVal attr ) , id, name, style, matrix)
      | "r"  == (X.qName $ X.attrKey attr) = (Ge.Ellipsis x y (read $ X.attrVal attr ) (read $ X.attrVal attr ), id, name, style, matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (Ge.Ellipsis x y rx ry, id, name, style, matrix * (parseTransform $ X.attrVal attr))
      | "style"      == (X.qName $ X.attrKey attr) =
        let fields = parseStyle $ X.attrVal attr
            style' = rawGraphStyle fields
        in (Ge.Ellipsis x y rx ry, id, name, style ++ style', matrix)
      | otherwise = (Ge.Ellipsis x y rx ry, id, name, style, matrix)
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
      ("marker-end", "none")        -> RawGraphStyle (Arrow ArrowNone) undefined : rawGraphStyle xs
      ("marker-end", _)             -> RawGraphStyle (Arrow ArrowTo)   undefined : rawGraphStyle xs
      ("marker-start", "none")      -> RawGraphStyle (Arrow ArrowNone) undefined : rawGraphStyle xs
      ("marker-start", _)           -> RawGraphStyle (Arrow ArrowFrom) undefined : rawGraphStyle xs
      ("fill", "none")              -> rawGraphStyle xs
      ("fill", color)               -> RawGraphStyle (Fill color)        undefined : rawGraphStyle xs
      ("stroke", "none")            -> rawGraphStyle xs
      ("stroke", color)             -> RawGraphStyle (Stroke color)      undefined : rawGraphStyle xs
      ("stroke-width", len)         -> RawGraphStyle Thick        (readLength len) : rawGraphStyle xs
      ("stroke-dasharray", "none")  -> rawGraphStyle xs
      ("stroke-dasharray", dashes)  -> let dash = takeWhile (/=',') dashes in RawGraphStyle Dashed (read dash) : rawGraphStyle xs
      _ -> rawGraphStyle xs
    parseCoordinates (x,y, matrix) attr
      | "x" == (X.qName $ X.attrKey attr) = ((read $ X.attrVal attr), y, matrix)
      | "y" == (X.qName $ X.attrKey attr) = (x,(read $ X.attrVal attr), matrix)
      | "transform"  == (X.qName $ X.attrKey attr) = (x,y, matrix * (parseTransform $ X.attrVal attr))
      | otherwise = (x,y, matrix)

mmFactor "in" = 25.4
mmFactor "cm" = 10
mmFactor "pt" = 2.834646
mmFactor "pc" = 0.2362205
mmFactor _    = 1

readLength len =
  let (n,u) = span (\x -> isNumber x || x == '.') len in (read n) * (mmFactor u)

parseStyle style = map (\f -> let (k,v) = span (/=':') f in (k,tail v) ) $ splitBy (==';') style

parseTransform transform = evalState parseTransform' transform
  where
    --  parseTransform' :: Floating a => State String (Matrix a)
    parseTransform' = do
      f <- function
      ps <- parameters
      let m = buildTransform  f ps
      rest <- whenNotEmpty (identity 3) $ parseTransform'
      return $ m * rest
    function :: State String String
    function = do
      consumeWhile (\x -> x `elem` [' ','\t'])
      f <- consumeWhile isAlpha
      return $ map toLower f
    --  parameters :: Floating a => State String [a]
    parameters = do
      consumeWhile  (\x -> x `elem` [' ', '(', '\t'])
      ps <- consumeWhile (/= ')')
      consumeWhile  (\x -> x `elem` [' ', ')', '\t'])
      let params = (splitBy (\x -> x `elem` ", ") ps)
      return $ map read params
    buildTransform :: Floating a => String -> [a] -> (Matrix a)
    buildTransform "scale" [sx]     = Ge.scale sx sx
    buildTransform "scale" [sx,sy]  = Ge.scale sx sy
    buildTransform "translate" [tx]     = Ge.translate tx 0
    buildTransform "translate" [tx, ty] = Ge.translate tx ty
    buildTransform "rotate" [a]        = Ge.rotate (pi * a / 180)
    buildTransform "rotate" [a,cx ,cy] = (Ge.translate cx cy) * (Ge.rotate (pi * a / 180)) * (Ge.translate (-cx) (-cy))
    buildTransform "matrix" [a,b,c,d,e,f] = fromList 3 3 [a,c,e,b,d,f,0,0,1]
    buildTransform "skewx" [a] = Ge.skewx a
    buildTransform "skewy" [a] = Ge.skewy a
    buildTransform _ _ = identity 3

parsePath path = evalState parsePath' path
  where
    --  parsePath' :: State String [(Float, Float)]
    parsePath' = do
      c <- command
      processCommand (0,0) (Right (0,0)) c
    processCommand (cx,cy) first cmd
      | cmd == "m" || cmd == "l" || cmd == "t" = do
        (x,y) <- coordPair
        c <- command
        let this = (x + cx, y + cy)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "M" || cmd == "L" || cmd == "T" = do
        (x,y) <- coordPair
        c <- command
        let this = (x, y)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "H" = do
        x <- value
        c <- command
        let this = (x, cy)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "h" = do
        x <- value
        c <- command
        let this = (cx + x, cy)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "V" = do
        y <- value
        c <- command
        let this = (cx, y)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "v" = do
        y <- value
        c <- command
        let this = (cx, cy + y)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "c" = do
        coordPair -- first and second control points are irrelevant
        coordPair
        (x,y) <- coordPair
        c <- command
        let this = (x + cx, y + cy)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "C" = do
        coordPair -- first and second control points are irrelevant
        coordPair
        (x,y) <- coordPair
        c <- command
        let this = (x, y)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "s" || cmd == "q" = do
        coordPair -- first control point is irrelevant
        (x,y) <- coordPair
        c <- command
        let this = ((x + cx), (y + cy))
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "S" || cmd == "Q"= do
        coordPair -- first control point is irrelevant
        (x,y) <- coordPair
        c <- command
        let this = (x, y)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "A" = do
        skipNValue 5 -- rx ry x-rotation larg-arc-flag sweep-flag are irrelevant
        (x,y) <- coordPair
        c <- command
        let this = (x, y)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "a"  = do
        skipNValue 5 -- rx ry x-rotation larg-arc-flag sweep-flag are irrelevant
        (x,y) <- coordPair
        c <- command
        let this = (x + cx, y + cy)
        rest <- whenNotEmpty [] $ processCommand this (first >> (Left this)) (if c == "" then cmd else c)
        return $ this : rest
      | cmd == "z" || cmd == "Z" =
        case first of
          Left p  -> return [p]
          Right p -> return [p]
      | otherwise = error $ "Unknown command '" ++ cmd ++ "' found when parsing line."
    --  coordPair :: Floating a => State String (a, a)
    coordPair = do
      consumeWhile (\x -> x `elem` " \t,")
      x <- consumeWhile ((\x -> x == 'e' || (not $ x `elem` " \t," || isAlpha x)))
      consumeWhile (\x -> x `elem` " \t,")
      y <- consumeWhile ((\x -> x == 'e' || (not $ x `elem` " \t," || isAlpha x)))
      return $ (read x, read y)
    command :: State String String
    command = do
      consumeWhile (\x -> x `elem` " \t,")
      c <- consumeWhile isAlpha
      return $ c
    --  value :: Floating a => State String a
    value = do
      consumeWhile (\x -> x `elem` " \t,")
      x <- consumeWhile ((\x -> not $ x `elem` " \t," || isAlpha x))
      return $ read x
    skipNValue :: Int -> State String ()
    skipNValue n = replicateM_ n value
