module Pictikz.Output.Tikz where

import Data.Maybe

class Drawable a where
  draw :: a -> String

node style (x,y) name alignment label = concat $
  [ "\\node"
  , "[" ++ csl style ++ "]"
  , "(" ++ name ++ ")"
  , " at "
  , "(" ++ show x ++ ", " ++ show y ++ ") "
  , "[ align=" ++ alignment ++"]"
  , "{" ++ label ++ "};"
  ]
  
edge style from to = concat $
  [ "\\draw"
  , "[" ++ csl style ++ "]"
  , "(" ++ from ++ ")"
  , " edge "
  , "(" ++ to ++ ");"
  ]



uncover begin end element = concat $
  [ "\\uncover"
  , "<" ++ fromMaybe [] begin ++ "-" ++ fromMaybe [] end ++ ">"
  , "{" ++ element ++ "}"
  ]

csl [] = []
csl [x] = x
csl (x:xs) = x ++ ", " ++ concatMap (\l -> ',':' ': l ) xs

tikzpicture d = concat
  [ "\\begin{tikzpicture}\n"
  , draw d
  , "\\end{tikzpicture}\n"]
