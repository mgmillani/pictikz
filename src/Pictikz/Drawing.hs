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

module Pictikz.Drawing where

import qualified Debug.Trace as D (trace)

-- RGB color with one byte per channel (0 to 255).
data Color = RGB Int Int Int

class Drawable a where
  draw :: a -> String

class Positionable t where
  getPos :: t a -> (a,a)
  fPos :: ((a,a) -> (a,a)) -> t a  -> t a

class Temporal a where
  getTime :: a -> (Int, Int)
  fTime   :: ((Int, Int) -> (Int, Int)) -> a -> a

instance Drawable Color where
  draw (RGB r g b) = "{RGB}{" ++ show r ++ ", "  ++ show g ++ ", " ++ show b ++ "}"

rgbDist (RGB r0 g0 b0) (RGB r1 g1 b1) = sqrt $ dr*dr + dg*dg + db*db
  where
    dr = fromIntegral $ r1 - r0
    dg = fromIntegral $ g1 - g0
    db = fromIntegral $ b1 - b0

-- | Convert HSL to RGB. Use formula from Wikipedia.
--  fromHSL :: Float -> Float -> Float -> Color
fromHSL h s l =
  case h' of
    0 -> RGB c x m
    1 -> RGB x c m
    2 -> RGB m c x
    3 -> RGB m x c
    4 -> RGB x m c
    5 -> RGB c m x
  where
    c' = (1 - (abs $ 2*l - 1)) * s
    (h', h'') = properFraction $ h / 60
    m = round $ 255 * (l - 0.5*c')
    x = (round $ 255 * c' * (1 - (abs $ h'' - 1))) + m
    c = (round $ 255 * c') + m

tikzpicture d = concat
  [ "\\begin{tikzpicture}\n"
  , draw d
  , "\\end{tikzpicture}\n"]
