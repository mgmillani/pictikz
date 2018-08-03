SVG = ${wildcard *.svg}
TEX = $(SVG:.svg=-grid.tex) $(SVG:.svg=-uniform.tex) $(SVG:.svg=-plain.tex)
PDF = ${SVG:svg=pdf}

all: $(TEX) $(PDF)

%-grid.tex: %.svg
	pictikz --colours colours --grid 15 --scale 3 2 $< > $@
%-uniform.tex: %.svg
	pictikz --colours colours --uniform 15 --scale 3 2 $< > $@
%-plain.tex: %.svg
	pictikz --colours colours --fit 3 2 $< > $@
%.pdf: %.svg
	inkscape --without-gui --export-pdf $@ $<

clean:
	rm $(TEX) $(PDF)
