module Pictikz.Parser where

import Prelude hiding (splitAt)
import Data.Matrix
import qualified Pictikz.Geometry as G
import Control.Monad.Trans.State
import Control.Monad
import Data.List hiding (splitAt)
import Data.Char
import qualified Debug.Trace as D (trace)

splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy p xs =
  let (w, r) = span (not . p) (dropWhile p xs)
  in w : (splitBy p r)

consumeWhile :: (Char -> Bool) -> State String String
consumeWhile f = do
  str <- get
  let (x,r) = span f str
  put r
  return x

whenNotEmpty e x = do
  s <- get
  if s == "" then return e else x

mmFactor "in" = 25.4
mmFactor "cm" = 10
mmFactor "pt" = 2.834646
mmFactor "pc" = 0.2362205
mmFactor _    = 1

readLength len =
  let (n,u) = span (\x -> isNumber x || x == '.') len in (read n :: Float) * (mmFactor u)

parseStyle style = map (\f -> let (k,v) = span (/=':') f in (k,tail v) ) $ splitBy (==';') style

parseTransform transform = evalState parseTransform' transform
  where
    parseTransform' :: State String (Matrix Float)
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
    parameters :: State String [Float]
    parameters = do
      consumeWhile  (\x -> x `elem` [' ', '(', '\t'])
      ps <- consumeWhile (/= ')')
      consumeWhile  (\x -> x `elem` [' ', ')', '\t'])
      let params = (splitBy (==',') ps)
      return $ map read params
    buildTransform :: String -> [Float] -> (Matrix Float)
    buildTransform "scale" [sx]     = G.scale sx sx
    buildTransform "scale" [sx,sy]  = G.scale sx sy
    buildTransform "translate" [tx]     = G.translate tx 0
    buildTransform "translate" [tx, ty] = G.translate tx ty
    buildTransform "rotate" [a]        = G.rotate a
    buildTransform "rotate" [a,cx ,cy] = (G.translate cx cy) * (G.rotate a) * (G.translate (-cx) (-cy))
    buildTransform "matrix" [a,b,c,d,e,f] = fromList 3 3 [a,c,e,b,d,f,0,0,1]
    buildTransform "skewx" [a] = G.skewx a
    buildTransform "skewy" [a] = G.skewy a
    buildTransform _ _ = identity 3

parsePath path = evalState parsePath' path
  where
    parsePath' :: State String [(Float, Float)]
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
    coordPair :: State String (Float, Float)
    coordPair = do
      consumeWhile (\x -> x `elem` [' ','\t',','])
      x <- consumeWhile ((\x -> not $ x `elem` [' ','\t',',']))
      consumeWhile (\x -> x `elem` [' ','\t',','])
      y <- consumeWhile ((\x -> not $ x `elem` [' ','\t',',']))
      return (read x, read y)
    command :: State String String
    command = do
      consumeWhile (\x -> x `elem` [' ','\t',','])
      c <- consumeWhile isAlpha
      return c
    value :: State String Float
    value = do
      consumeWhile (\x -> x `elem` [' ','\t',','])
      x <- consumeWhile ((\x -> not $ x `elem` [' ','\t',',']))
      return $ read x
    skipNValue :: Int -> State String ()
    skipNValue n = replicateM_ n value
