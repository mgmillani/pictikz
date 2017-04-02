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

module Pictikz.Geometry where

import Data.Matrix

data Shape a = Rectangle a a a a | Ellipsis a a a a deriving (Show, Eq)

rotate a =
  fromList 3 3 [cos a, - sin a, 0
               ,sin a,   cos a, 0
               ,    0,       0, 1]
translate x y =
  fromList 3 3 [1,0,x
               ,0,1,y
               ,0,0,1]
skewx a =
  fromList 3 3 [1, tan a, 0
               ,0,     1, 0
               ,0,     0, 1]
skewy a =
  fromList 3 3 [1    , 0, 0
               ,tan a, 1, 0
               ,0    , 0, 1]
scale x y =
  fromList 3 3 [x,0,0
               ,0,y,0
               ,0,0,1]

squareDistance (x0,y0) (Rectangle x y w h) =
  let dx
        | x0 >= x && x0 <= x + w = 0
        | x0 >= x + w = x0 - (x+w)
        | x0 < x = x - x0
      dy
        | y0 >= y && y0 <= y + h = 0
        | y0 >= y + h = y0 - (y+h)
        | y0 < y = y - y0
  in dx*dx + dy*dy
squareDistance (x0,y0) (Ellipsis x1 y1 rx ry) =
  let dx = (x0 - x1) / rx
      dy = (y0 - y1) / ry
      sd = dx*dx + dy*dy
  in if sd < 1 then 0 else sd
distance p0 p1 = sqrt $ squareDistance p0 p1

