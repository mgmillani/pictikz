SVG = ${wildcard *.svg}
TEX = $(SVG:.svg=-grid.tex) $(SVG:.svg=-uniform.tex) $(SVG:.svg=-plain.tex)
PDF = ${SVG:svg=pdf}

all: $(TEX) $(PDF)

text-circle-grid.tex: text-circle.svg
	pictikz --text-as-nodes --colours colours --grid 15 --scale 3 2 $< > $@
text-circle-uniform.tex: text-circle.svg
	pictikz --text-as-nodes --colours colours --uniform 15 --scale 3 2 $< > $@
text-circle-plain.tex: text-circle.svg
	pictikz --text-as-nodes --colours colours --fit 3 2 $< > $@

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
