#!/bin/sh
cp ../pictikzgraph.sty .

# Grid example
pictikz           --fit 3 3 grid.svg --output grid-plain.tex
pictikz --uniform --fit 3 3 grid.svg --output grid-uniform.tex
pictikz --grid    --fit 3 3 grid.svg --output grid-grid.tex

# Tree example
pictikz           --fit 2.5 2.5 tree.svg --output tree-plain.tex
pictikz --uniform --fit 2.5 2.5 tree.svg --output tree-uniform.tex
pictikz --grid    --fit 2.5 2.5 tree.svg --output tree-grid.tex

# Star example
pictikz           --scale 2.5 2.5 star.svg --output star-plain.tex
pictikz --uniform --scale 2.5 2.5 star.svg --output star-uniform.tex
pictikz --grid    --scale 2.5 2.5 star.svg --output star-grid.tex

# Stickman example
pictikz              --fit 3 3 running-man.svg --output running-man-plain.tex
pictikz --uniform 10 --fit 3 3 running-man.svg --output running-man-uniform.tex
pictikz --grid    10 --fit 3 3 running-man.svg --output running-man-grid.tex

# Box example
pictikz           --scale 2 2 box.svg --output box-plain.tex
pictikz --uniform --scale 2 2 box.svg --output box-uniform.tex
pictikz --grid    --scale 2 2 box.svg --output box-grid.tex

pdflatex examples.tex
rm pictikzgraph.sty
