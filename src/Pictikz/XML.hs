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

module Pictikz.XML (satisfyAttrib) where

import qualified Text.XML.Light as X

-- | Checks if the value of `atName` satisfies the predicate `p`
satisfyAttrib el atName p =
  or $ map (\a -> p $ X.attrVal a) $ filter (\a -> (X.qName $ X.attrKey a) == atName) $ X.elAttribs el
