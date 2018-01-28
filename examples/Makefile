BUILD=tex-files

ATEMPORAL_SVG = $(wildcard atemporal/*.svg)
ATEMPORAL_PDF = $(addprefix $(BUILD)/,$(ATEMPORAL_SVG:.svg=.pdf))
ATEMPORAL_PDF_FIGURE = $(addprefix $(BUILD)/,$(ATEMPORAL_SVG:.svg=-pdf-figure.tex))
ATEMPORAL_TEX = $(addprefix $(BUILD)/,$(ATEMPORAL_SVG:.svg=-grid.tex) $(ATEMPORAL_SVG:.svg=-uniform.tex) $(ATEMPORAL_SVG:.svg=-plain.tex))
ATEMPORAL_FIGURE =  $(addprefix $(BUILD)/, $(ATEMPORAL_SVG:.svg=-figure.tex))
TEMPORAL_SVG  = $(wildcard temporal/*.svg)
TEMPORAL_FIGURE  = $(addprefix $(BUILD)/,$(TEMPORAL_SVG:.svg=-figure.tex))
TEMPORAL_TEX  = $(addprefix $(BUILD)/,$(TEMPORAL_SVG:.svg=.tex))

PICTIKZ=pictikz

all: atemporal-compact.pdf temporal.pdf atemporal-comparison.pdf

temporal.pdf: $(BUILD)/temporal $(BUILD)/temporal.tex
	cd $(BUILD) && pdflatex temporal.tex && mv temporal.pdf ../

atemporal-compact.pdf: $(BUILD)/atemporal ${ATEMPORAL_FIGURE} $(BUILD)/atemporal-compact.tex
	cd $(BUILD) && pdflatex atemporal-compact.tex && mv atemporal-compact.pdf ../

atemporal-comparison.pdf: $(BUILD)/atemporal ${ATEMPORAL_PDF_FIGURE} $(BUILD)/atemporal-comparison.tex
	cd $(BUILD) && pdflatex atemporal-comparison.tex && mv atemporal-comparison.pdf ../

$(BUILD)/temporal:
	mkdir -p $@

$(BUILD)/atemporal:
	mkdir -p $@

$(BUILD)/temporal.tex: ${TEMPORAL_FIGURE} tex-files/beamer.tex
	cat tex-files/beamer.tex ${TEMPORAL_FIGURE} > $@
	echo "\\end{document}" >> $@

$(BUILD)/atemporal-compact.tex: ${ATEMPORAL_FIGURE} tex-files/atemporal.tex
	cat tex-files/atemporal.tex ${ATEMPORAL_FIGURE} > $@
	echo "\\end{document}" >> $@

$(BUILD)/atemporal-comparison.tex: ${ATEMPORAL_PDF_FIGURE} tex-files/atemporal.tex
	cat tex-files/atemporal.tex ${ATEMPORAL_PDF_FIGURE} > $@
	echo "\\end{document}" >> $@

$(BUILD)/atemporal/%.pdf: atemporal/%.svg
	inkscape --without-gui --export-pdf $@ $<

$(BUILD)/atemporal/%-grid.tex: atemporal/%.svg
	${PICTIKZ} --colours colours --grid 15 --scale 2 2 $< > $@

$(BUILD)/atemporal/%-uniform.tex: atemporal/%.svg
	${PICTIKZ} --colours colours --uniform 15 --scale 2 2 $< > $@

$(BUILD)/atemporal/%-plain.tex: atemporal/%.svg
	${PICTIKZ} --colours colours --fit 3 2 $< > $@

$(BUILD)/atemporal/%-pdf-figure.tex: $(BUILD)/atemporal/%.pdf $(BUILD)/atemporal/%-grid.tex $(BUILD)/atemporal/%-uniform.tex $(BUILD)/atemporal/%-plain.tex
	echo -e '\\begin{figure}[H]\n'\
	  '\\centering\n'\
	  '\\includegraphics[width=0.5\\textwidth]{atemporal/$*.pdf}\n'\
	  '\\caption{Input image (\\texttt{atemporal/$*.svg}).}\n'\
	  '\\end{figure}\n\n'\
	  '\\begin{figure}[H]\n'\
	  '\\centering\n'\
	  '\\begin{tikzpicture}[scale=1.5]\n'\
	  '\\input{atemporal/$*-grid.tex}\n'\
	  '\\end{tikzpicture}~\n'\
	  '\\begin{tikzpicture}[scale=1.5]\n'\
	  '\\input{atemporal/$*-uniform.tex}\n'\
	  '\\end{tikzpicture}~\n'\
	  '\\begin{tikzpicture}[scale=1.5]\n'\
	  '\\input{atemporal/$*-plain.tex}\n'\
	  '\\end{tikzpicture}\n'\
	  '\\caption{Output of pictikz (grid, uniform, plain)}\n'\
	  '\\end{figure}\\clearpage\n' > $@

$(BUILD)/atemporal/%-figure.tex: $(BUILD)/atemporal/%-grid.tex $(BUILD)/atemporal/%-uniform.tex $(BUILD)/atemporal/%-plain.tex
	echo "\\begin{figure}\
	\\centering\
	\\begin{tikzpicture}\
	  \\input{atemporal/$*-grid.tex}\
	\\end{tikzpicture}~\
	\\begin{tikzpicture}\
	  \\input{atemporal/$*-uniform.tex}\
	\\end{tikzpicture}~\
	\\begin{tikzpicture}\
	  \\input{atemporal/$*-plain.tex}\
	\\end{tikzpicture}\
	\\end{figure}\\pagebreak" > $@

$(BUILD)/temporal/%-figure.tex: $(BUILD)/temporal/%.tex
	echo -e '\\begin{frame}{$*}\n'\
	'\\begin{minipage}{\\textwidth}\n'\
	'\\centering\n'\
	'\\begin{tikzpicture}\n'\
	  '\\input{temporal/$*.tex}\n'\
	'\\end{tikzpicture}\n'\
	'\\end{minipage}\n'\
	'\\end{frame}\n' > $@

$(BUILD)/temporal/%.tex: temporal/%.svg
	${PICTIKZ} --colours colours --temporal --grid 15 --scale 5 4 $< > $@

clean:
	rm $(BUILD)/temporal/*
	rm $(BUILD)/atemporal/*
	rm atemporal-compact.pdf atemporal-comparison.pdf temporal.pdf
