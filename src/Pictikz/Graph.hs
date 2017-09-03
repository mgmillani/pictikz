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


module Pictikz.Graph where

import Pictikz.Drawing

data ArrowType = ArrowTo | ArrowFrom | ArrowNone | ArrowBoth deriving (Read, Eq, Ord)
joinArrow a ArrowNone = a
joinArrow ArrowBoth b = ArrowBoth
joinArrow ArrowTo ArrowFrom = ArrowBoth
joinArrow ArrowFrom ArrowTo = ArrowBoth
joinArrow a b = b

data Style =
    Dashed
  | Dotted
  | Thick
  | Rectangle
  | Circle
  | Fill String
  | Stroke String
  | Arrow ArrowType                               deriving (Show, Read, Eq, Ord)
data Node a  = Node a a String String [Style]     deriving (Show, Read, Eq, Ord)
data Edge    = Edge String String [Style]         deriving (Show, Read, Eq, Ord)
data Graph a = Graph [Node a] [Edge]              deriving (Show, Read, Eq, Ord)

instance Show ArrowType where
  show ArrowTo   = "pictikz-edgeto"
  show ArrowFrom = "pictikz-edgefrom"
  show ArrowBoth = "pictikz-edgeboth"
  show ArrowNone = ""

instance Positionable Node where
  getPos (Node x y _ _ _)     = (x,y)
  fPos f (Node x y id name style) = let (x1,y1) = f (x,y) in Node x1 y1 id name style

instance Drawable Style where
  draw Dotted    = ", pictikz-dotted"
  draw Dashed    = ", pictikz-dashed"
  draw Thick     = ", pictikz-thick"
  draw Rectangle = ", pictikz-rectangle"
  draw Circle    = ", pictikz-node"
  draw (Fill c)  = ", fill=" ++ c
  draw (Stroke c)  = ", draw=" ++ c
  draw (Arrow ArrowNone) = ""
  draw (Arrow t) = ", " ++ show t

instance (Num a, Show a) => Drawable (Node a) where
  draw (Node x y id name style) = concat
    [ "\\node["
    , drop 2 $ concatMap draw style
    , "] ("
    , id
    , ") at ("
    , show x
    , ", "
    , show y
    , ") {"
    , name
    , "};\n"
    ]

instance Drawable Edge where
  draw (Edge n1 n2 style) = concat
    [ "\\draw["
    , drop 2 $ concatMap draw style
    , "] ("
    , n1
    , ") edge ("
    , n2
    , ");\n"
    ]

instance (Num a, Show a) => Drawable (Graph a) where
  draw (Graph nodes edges) = concat $ map draw nodes ++ map draw edges
