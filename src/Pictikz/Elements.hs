--  Copyright 2017-2018 Marcelo Garlet Millani
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

module Pictikz.Elements where

import Data.Matrix
import qualified Pictikz.Geometry as G
import qualified Pictikz.Text  as T
import qualified Pictikz.Output.Tikz as Tikz

-- RGB color with one byte per channel (0 to 255).
data Color = RGB Int Int Int

rgbDist (RGB r0 g0 b0) (RGB r1 g1 b1) = sqrt $ dr*dr + dg*dg + db*db
  where
    dr = fromIntegral $ r1 - r0
    dg = fromIntegral $ g1 - g0
    db = fromIntegral $ b1 - b0

-- | Convert HSL to RGB. Use formula from Wikipedia.
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

instance Tikz.Drawable Color where
  draw (RGB r g b) = "{RGB}{" ++ show r ++ ", "  ++ show g ++ ", " ++ show b ++ "}"

data ArrowType = ArrowTo | ArrowFrom | ArrowNone | ArrowBoth deriving (Read, Eq, Ord)
joinArrow a ArrowNone = a
joinArrow ArrowBoth b = ArrowBoth
joinArrow ArrowTo ArrowFrom = ArrowBoth
joinArrow ArrowFrom ArrowTo = ArrowBoth
joinArrow a b = b

instance Show ArrowType where
  show ArrowTo   = "pictikz-edgeto"
  show ArrowFrom = "pictikz-edgefrom"
  show ArrowBoth = "pictikz-edgeboth"
  show ArrowNone = ""

data GraphStyle =
    Dashed
  | Dotted
  | Thick
  | Rectangle
  | Circle
  | Fill String
  | Stroke String
  | LeftAligned
  | RightAligned
  | Centered
  | Arrow ArrowType                                        deriving (Show, Read, Eq, Ord)

data Style = RawTextStyle T.Format | RawGraphStyle GraphStyle Double  | Parsed GraphStyle deriving (Show, Eq, Read)

data Element a =
  Object (G.Shape a) String [T.Text] [Style]
  | Line a a a a [Style]
  | Paragraph a a [T.Text] [Style]
  | Layer [Element a] deriving (Eq)

class Positionable t where
  getPos :: t a -> (a,a)
  fPos :: ((a,a) -> (a,a)) -> t a  -> t a

class Temporal a where
  getTime :: a -> (Int, Int)
  fTime   :: ((Int, Int) -> (Int, Int)) -> a -> a

instance Show a => Show (Element a) where
  show (Object shape iD name style) = concat [iD, ": ", show shape]
  show (Line x0 y0 x1 y1 _) = concat [show (x0,y0), " -- ", show (x1, y1)]
  show (Layer els) = concatMap (\x -> show x ++ "\n") els
  show (Paragraph x y text style) = "Par:" ++ show (x,y)

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

textAsNodes elements = fst $ textAsNodes' elements 1
  where
    textAsNodes' [] i = ([], i)
    textAsNodes' (e:es) i =
      case e of
        Paragraph x y ts format ->
          let (rs, n) = textAsNodes' es (i+1)
          in ((Object (G.Rectangle x y 1 1) ("text-" ++ show i) ts format) : rs, n)
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
  RawTextStyle T.Centered     -> Centered
  RawTextStyle T.LeftAligned  -> LeftAligned
  RawTextStyle T.RightAligned -> RightAligned
  RawGraphStyle s' _ -> s'
