SVG = ${wildcard *.svg}
TEX = $(SVG:svg=tex)

all: $(TEX)

%.tex: %.svg
	pictikz --colours colours --temporal --grid 10 --scale 7 5 $< > $@
