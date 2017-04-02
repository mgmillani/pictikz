module Pictikz.Drawing where

class Drawable a where
  draw :: a -> String

class Positionable t where
  getPos :: t a -> (a,a)
  fPos :: ((a,a) -> (a,a)) -> t a  -> t a

tikzpicture d = concat
  [ "\\begin{tikzpicture}\n"
  , draw d
  , "\\end{tikzpicture}\n"]
