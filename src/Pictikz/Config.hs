module Pictikz.Config where

import qualified Data.DescriLo as D
import           Data.Char
import           Pictikz.Elements
import           System.Directory
import           System.Environment
import           Pictikz.Parser

-- | Searches for the file in the following directories:
--   1. Current directory.
--   2. $XDG_CONFIG_HOME/pictikz
--   3. $HOME/.pictikz
--   4. /usr/share/pictikz
-- | If the file is not found in any of these directories, the same path is returned.
findFileConfig fl = do
  xdg <- getXdgDirectory XdgConfig "pictikz"
  app <- getAppUserDataDirectory "pictikz"
  exists <- doesFileExist fl
  if exists then return fl
	else findFileDirs fl [xdg, app, "/usr/share/paphragen"]

findFileDirs fl dirs = do
  let dirsFl = map ( ++ ('/':fl)) dirs
  dirsE <- mapM doesFileExist $ dirsFl
  let results = dropWhile (\(fl,e) -> not e) $ zip dirsFl dirsE
  return $ if null results then fl else fst $ head results

loadConfig fl = do
	fl' <- findFileConfig fl
	D.loadDescriptionFile fl' "heuristics"

loadColors str = map (getColor . words) $ lines str
  where
    getColor (name:space:value)
      | (map toLower space) == "rgb" =
        let (r,g,b) = parseValue value 255 255 255
        in (RGB r g b, name)
      | (map toLower space) == "hsl" =
        let (h,s,l) = parseValue value 359 100 100
        in (fromHSL (fromIntegral h) (fromIntegral s / 100) (fromIntegral l / 100), name)
    parseValue (('#':hex):_) _ _ _ = readHexa hex
    parseValue (x:y:z:_) mx my mz =
      let [x1,y1,z1] = zipWith parseNumber [x, y, z] [mx, my, mz] in (x1, y1, z1)
    parseNumber x mx
      | '.' `elem` x = round $ (read x) * mx
      | otherwise = read x :: Int
