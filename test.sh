#!/bin/sh

BIN=dist/build/pictikz/pictikz

echo "" > tests/compact.tex
cp examples/pictikzgraph.sty tests

for s in examples/*.svg
do
	OUT=$(basename $s .svg)
	$BIN --colours colours --grid 15    --scale 3 2 "$s" -o "tests/grid-scale-$OUT.tex"
	$BIN --colours colours --grid 15    --fit 3 2 "$s" -o "tests/grid-fit-$OUT.tex"
	$BIN --colours colours --uniform 15 --fit 3 2 "$s" -o "tests/uniform-$OUT.tex"
	$BIN --colours colours              --fit 3 2 "$s" -o "tests/plain-$OUT.tex"
	echo "\\begin{figure}\
	\\centering\
	\\begin{tikzpicture}\
	\\input{grid-scale-$OUT.tex}\
	\\end{tikzpicture}~\
	\\begin{tikzpicture}\
	\\input{grid-fit-$OUT.tex}\
	\\end{tikzpicture}~\
	\\begin{tikzpicture}\
	\\input{uniform-$OUT.tex}\
	\\end{tikzpicture}~\
	\\begin{tikzpicture}\
	\\input{plain-$OUT.tex}\
	\\end{tikzpicture}\
	\\end{figure}" >> tests/compact.tex
done

cd tests
pdflatex test.tex
