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

module Pictikz.Organizer where

import Pictikz.Drawing
import Pictikz.Graph
import Data.List

import qualified Debug.Trace as D (trace)

fitToBox w h objects =
  let positions = map getPos objects
      xs = map fst positions
      ys = map snd positions
      shiftx = - minimum xs
      shifty = - minimum ys
      scalex = maximum xs
      scaley = maximum ys
      scale = min (w / (scalex + shiftx)) (h / (scaley + shifty))
  in map (fPos (\(x,y) -> ((x + shiftx) * scale, (y + shifty) * scale)))  objects

scaleToBox w h objects =
  let positions = map getPos objects
      xs = map fst positions
      ys = map snd positions
      shiftx = - minimum xs
      shifty = - minimum ys
      scalex = shiftx + maximum xs
      scaley = shifty + maximum ys
  in map (fPos (\(x,y) -> ((x + shiftx) * w/scalex, (y + shifty) * h/scaley)))  objects

average xs = realToFrac (sum xs) / genericLength xs

-- | Organizes nodes in a way that the amount of x and y coordinates used is decreased.
-- | The given parameter `d` the maximum difference in order to merge two coordinates.
-- | The function `groupf` should convert coordinates into their grouped form
uniformCoordinatesBy groupf d ns =
  let nsx = sortBy (\n m -> compare (fst $ getPos n) (fst $ getPos m)) ns
      xs = map (fst . getPos) nsx
      dx = d * ((maximum xs) - (minimum xs))
      ux = concat $ groupf dx xs
      -- update x coordinates
      ns1 = zipWith (\n x1 -> fPos (\(x,y) -> (x1,y)) n) nsx ux
      nsy = sortBy (\n m -> compare (snd $ getPos n) (snd $ getPos m)) ns1
      ys = map (snd . getPos) nsy
      dy = d * ((maximum ys) - (minimum ys))
      uy = concat $ groupf dy ys
  in zipWith (\n y1 -> fPos (\(x,y) -> (x,y1)) n) nsy uy

genGroup f d [] = []
genGroup f d as =
  let (bs, r1) = f d as in bs : genGroup f d r1

distanceGroup d0 [] = ([], [])
distanceGroup d0 (a:as) = group' d0 d0 0 a (a:as)
  where
    group' d d0 l a0 as =
      let (g, rest) = span (\x -> x - a0 < d0) as
          a1 = average g
          l1 = genericLength g
          d1 = 0.55 * d
      in if g == [] then (take l $ repeat a0, rest) else (group' d d1 (l + l1) a1 rest)

isometricGroup d0 as =
  let gs = genGroup distanceGroup d0 as in zipWith (\g i -> map (\x -> fromIntegral i) g) gs [0,1..]
