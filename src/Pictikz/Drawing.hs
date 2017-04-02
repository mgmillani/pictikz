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

class Drawable a where
  draw :: a -> String

class Positionable t where
  getPos :: t a -> (a,a)
  fPos :: ((a,a) -> (a,a)) -> t a  -> t a

tikzpicture d = concat
  [ "\\begin{tikzpicture}\n"
  , draw d
  , "\\end{tikzpicture}\n"]
