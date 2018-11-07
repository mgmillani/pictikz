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

import qualified Data.Set as S
import qualified Data.Map as M
import Pictikz.Graph
import Pictikz.Elements
import Data.List
import Data.Maybe

import qualified Debug.Trace as D (trace)

boundingBox (Graph nodes edges) =
  let coords = map getPos nodes
      x0 = minimum $ map fst $ coords
      x1 = maximum $ map fst $ coords
      y0 = minimum $ map snd $ coords
      y1 = maximum $ map snd $ coords
  in (x0,y0,x1,y1)

-- | Join equal vertices which stand on the same position in subsequent layers
mergeLayers epsilon (Graph n0 e0) (Graph n1 e1) = Graph gnodes gedges
  -- TODO: only nodes of adjacent layers should be compared. Right now, n1 contains all nodes.
  where
    epsilon2 = epsilon * epsilon
    mergeNode n [] = ([], Nothing)

    mergeNode n (m:ms) =
      let (Node x0 y0 uid0 name0 style0 (t0, t1)) = n
          (Node x1 y1 uid1 name1 style1 (t2, t3)) = m
          d = (x1 - x0)^2 + (y1 - y0)^2
      in if d < epsilon2 && t1 == (t2-1) && name0 == name1 && style0 == style1 then
          ((Node x1 y1 uid1 name1 style1 (t0,t3)) : ms, Just uid1)
         else
           let (r, rn) = mergeNode n ms
           in (m : r, rn)
    mergeNodes [] ms = (M.empty, ms)
    mergeNodes (n:ns) ms =
      let (ms', n') = mergeNode n ms
          (t, nodes') = mergeNodes ns ms'
      in case n' of
        Nothing    -> (t, n : nodes')
        Just uid1  ->
          let (Node _ _ uid0 _ _ _) = n
          in (M.insert uid0 uid1 t, nodes')
    (table, gnodes) = mergeNodes n0 n1
    edgeIda   (Edge v u style (t0,t1)) = (v, u, style, t1)
    edgeIdb   (Edge v u style (t0,t1)) = (v, u, style, t0-1)
    edgeInfo (Edge v u style time) = time
    gedges =  map (\((v, u, style, _), time) -> Edge v u style time) $ M.toList $
      M.unionWith (\(t0, _) (_,t1) -> (t0,t1))
        (M.fromList $ map (\e -> (edgeIda e, edgeInfo e)) $ map updateEdge e0)
        (M.fromList $ map (\e -> (edgeIdb e, edgeInfo e)) $ map updateEdge e1)
    updateEdge (Edge v u style time) = (Edge (f v) (f u) style time)
      where
        f x = fromMaybe x $ table M.!? x

fitToBox w h objects =
  let positions = map getPos objects
      xs = map fst positions
      ys = map snd positions
      shiftx = - minimum xs
      shifty = - minimum ys
      scalex = maximum xs
      scaley = maximum ys
      scale = max ((scalex + shiftx) / w) ((scaley + shifty) / h)
  in if scale == 0 then objects else map (fPos (\(x,y) -> ((x + shiftx) / scale, (y + shifty) / scale)))  objects

scaleToBox w h objects =
  let positions = map getPos objects
      xs = map fst positions
      ys = map snd positions
      shiftx = - minimum xs
      shifty = - minimum ys
      scalex' = shiftx + maximum xs
      scaley' = shifty + maximum ys
      scalex = if scalex' == 0 then 1 else scalex'
      scaley = if scaley' == 0 then 1 else scaley'
  in map (fPos (\(x,y) -> ((x + shiftx) * w/scalex, (y + shifty) * h/scaley)))  objects

-- | Reposition objects such that the minimum distance in x-axis equals x (and the analog for the y-axis).
minDist xd yd objects =
  let positions = map getPos objects
      xs = sort $ map fst positions
      ys = sort $ map snd positions
      shiftx = - minimum xs
      shifty = - minimum ys
      minDistXs = filter (/= 0.0) $ zipWith (\x0 x1 -> abs $ x0 - x1) xs (tail xs)
      minDistYs = filter (/= 0.0) $ zipWith (\y0 y1 -> abs $ y0 - y1) ys (tail ys)
      mdX = if null minDistXs then xd else minimum minDistXs
      mdY = if null minDistYs then yd else minimum minDistYs
  in map (fPos (\(x,y) -> (xd * (x + shiftx) / mdX, yd * (y + shifty) / mdY))) objects

average xs = realToFrac (sum xs) / genericLength xs

-- | Organizes nodes in a way that the amount of x and y coordinates used is decreased.
-- | The given parameter `d` the maximum difference in order to merge two coordinates.
-- | The function `groupf` should convert coordinates into their grouped form
uniformCoordinatesBy groupf d ns =
  let nsx = sortBy (\n m -> compare (fst $ getPos n) (fst $ getPos m)) ns
      xs = map (fst . getPos) nsx
      dx = max 1e-5 $ d * ((maximum xs) - (minimum xs))
      ux = concat $ groupf dx xs
      -- update x coordinates
      ns1 = zipWith (\n x1 -> fPos (\(x,y) -> (x1,y)) n) nsx ux
      nsy = sortBy (\n m -> compare (snd $ getPos n) (snd $ getPos m)) ns1
      ys = map (snd . getPos) nsy
      dy = max 1e-5 $ d * ((maximum ys) - (minimum ys))
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
