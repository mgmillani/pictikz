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

module Pictikz.Parser where

import Control.Monad.Trans.State
import Control.Monad
import Data.Char
import Pictikz.Elements

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

readHexa xyz =
  let hexa = map digitToInt xyz
  in case hexa of
    (xh:xl:yh:yl:zh:zl:[]) -> ((xh*16 + xl), (yh*16 + yl), (zh*16 + zl))
    (x:y:z:[]) -> ((x*17), (y*17), (z*17))
readColor ('#':color) =
  let (r,g,b) = readHexa color
  in RGB r g b
