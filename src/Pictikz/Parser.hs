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

module Pictikz.Parser (parseTransform, parseStyle, parsePath, readLength, readColor, readHexa) where

import Prelude hiding (splitAt)
--  import qualified Prelude as P (read)
import Data.Matrix
import qualified Pictikz.Geometry as G
import qualified Pictikz.Drawing  as D
import Control.Monad.Trans.State
import Control.Monad
import Data.List hiding (splitAt)
import Data.Char
import qualified Debug.Trace as Db (trace)

--  read x = (D.trace ("read:" ++ show x) $ P.read x)

splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy p xs =
  let (w, r) = span (not . p) (dropWhile p xs)
  in w : (splitBy p r)

consumeWhile :: (Char -> Bool) -> State String String
consumeWhile f = do
  str <- get
  let (x,r) = span f str
  put $ r
  return x

whenNotEmpty e x = do
  s <- get
  if s == "" then return e else x

mmFactor "in" = 25.4
mmFactor "cm" = 10
mmFactor "pt" = 2.834646
mmFactor "pc" = 0.2362205
mmFactor _    = 1

readHexa xyz =
  let hexa = map digitToInt xyz
  in case hexa of
    (xh:xl:yh:yl:zh:zl:[]) -> ((xh*16 + xl), (yh*16 + yl), (zh*16 + zl))
    (x:y:z:[]) -> ((x*17), (y*17), (z*17))
readColor ('#':color) =
  let (r,g,b) = readHexa color
  in D.RGB r g b

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
    buildTransform "scale" [sx]     = G.scale sx sx
    buildTransform "scale" [sx,sy]  = G.scale sx sy
    buildTransform "translate" [tx]     = G.translate tx 0
    buildTransform "translate" [tx, ty] = G.translate tx ty
    buildTransform "rotate" [a]        = G.rotate (pi * a / 180)
    buildTransform "rotate" [a,cx ,cy] = (G.translate cx cy) * (G.rotate (pi * a / 180)) * (G.translate (-cx) (-cy))
    buildTransform "matrix" [a,b,c,d,e,f] = fromList 3 3 [a,c,e,b,d,f,0,0,1]
    buildTransform "skewx" [a] = G.skewx a
    buildTransform "skewy" [a] = G.skewy a
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
