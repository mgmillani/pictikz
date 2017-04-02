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

