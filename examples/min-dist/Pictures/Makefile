SVG = ${wildcard *.svg}
TEX = $(SVG:svg=tex)

all: $(TEX)

%.tex: %.svg
	pictikz --colours colours --grid --fit 0.5 0.5 --min-dist 0.5 0.5 $< > $@
