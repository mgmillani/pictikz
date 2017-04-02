module Pictikz.Graph where

import Pictikz.Drawing

data ArrowType = ArrowTo | ArrowFrom | ArrowNone | ArrowBoth deriving (Read, Eq, Ord)
joinArrow a ArrowNone = a
joinArrow ArrowBoth b = ArrowBoth
joinArrow ArrowTo ArrowFrom = ArrowBoth
joinArrow ArrowFrom ArrowTo = ArrowBoth
joinArrow a b = b

data NodeStyle = Rectangle                         deriving (Show, Read, Eq, Ord)
data EdgeStyle =
    Dashed
  | Dotted
  | Thick
  | Arrow ArrowType                               deriving (Show, Read, Eq, Ord)
data Node a = Node a a String String [NodeStyle]  deriving (Show, Read, Eq, Ord)
data Edge = Edge String String [EdgeStyle]        deriving (Show, Read, Eq, Ord)
data Graph a = Graph [Node a] [Edge]              deriving (Show, Read, Eq, Ord)

instance Show ArrowType where
  show ArrowTo   = "edgeto"
  show ArrowFrom = "edgefrom"
  show ArrowBoth = "edgeboth"
  show ArrowNone = ""

instance Positionable Node where
  getPos (Node x y _ _ _)     = (x,y)
  fPos f (Node x y id name style) = let (x1,y1) = f (x,y) in Node x1 y1 id name style

instance Drawable NodeStyle where
  draw Rectangle = ", rectangle"

instance Drawable EdgeStyle where
  draw Dotted = ", dotted"
  draw Dashed = ", dashed"
  draw Thick  = ", very thick"
  draw (Arrow t) = ", " ++ show t

instance (Num a, Show a) => Drawable (Node a) where
  draw (Node x y id name style) = concat
    [ "\\node[gnode"
    , concatMap draw style
    , "] ("
    , id,
    ") at ("
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
    , concatMap draw style
    , "] ("
    , n1
    , ") edge ("
    , n2
    , ");\n"
    ]

instance (Num a, Show a) => Drawable (Graph a) where
  draw (Graph nodes edges) = concat $ map draw nodes ++ map draw edges
