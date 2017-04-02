module Main where

import Pictikz.Drawing
import Pictikz.Graph
import Pictikz.Loader
import Pictikz.Parser
import Pictikz.Organizer

testGraph inF outF = do
  svg <- readFile inF
  let (Graph nodes edges) = loadGraph svg
  writeFile outF (draw $ Graph (scaleToBox 1 1 $ uniformCoordinatesBy isometricGroup 0.2 nodes) edges)
main = do
  let g1 = Graph
            [Node x y ("v" ++ show x ++ show y) ("$v_{" ++ show x ++ show y ++"}$") []  | x <- [0,1,2], y <- [0,1,2]]
            [Edge ("v" ++ show x ++ show y) ("v" ++ (show $ x + 1) ++ (show $ y + 1)) [Arrow ArrowNone] | x <- [0,1] , y <- [0,1]]
  writeFile "g1.tex" (tikzpicture g1)
  testGraph "../examples/C5.svg" "C5.tex"
  testGraph "../examples/akt-standard-graph.svg" "akt-standard-graph.tex"
  testGraph "../examples/pairs.svg" "pairs.tex"
  testGraph "../examples/speech-recognition.svg" "speech-recognition.tex"
  testGraph "../examples/twin-cross.svg" "twin-cross.tex"
  testGraph "../examples/9x9-grid.svg" "9x9-grid.tex"
  testGraph "../examples/tree.svg" "tree.tex"
