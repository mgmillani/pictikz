--  Copyright 2018 Marcelo Garlet Millani
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

module Pictikz.Text where

import qualified Pictikz.Output.Tikz as Tikz

data Format =
    Bold
  | Italics
  | Subscript
  | Superscript
  | LeftAligned
  | RightAligned
  | Centered
  deriving (Eq, Show, Read, Ord)

data Text = Text String [Format] deriving (Eq, Show, Read, Ord)

instance Tikz.Drawable Format where
  draw f = case f of
    Bold         -> "\\bfseries{}"
    Italics      -> "\\itseries{}"
    Subscript    -> "\\textsubscript"
    Superscript  -> "\\textsuperscript"
    LeftAligned  -> "\\raggedright{}"
    RightAligned -> "\\raggedleft{}"
    Centered     -> "\\centering{}"

instance Tikz.Drawable Text where
  draw (Text str format) =
    concat $ 
      [ filter (=='\n') str
      , concatMap Tikz.draw $ (filter (\f -> f `elem` [Subscript, Superscript])) format
      , "{"
      , concatMap Tikz.draw $ (filter (\f -> not $ f `elem` [Subscript, Superscript])) format
      , ""
      , filter (/='\n') str
      , "}"      
      ]
