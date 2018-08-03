PICTURE_DIR    := Pictures
PICTURES_SVG   := ${wildcard ${PICTURE_DIR}/*.svg}
PICTURES   := ${PICTURES_SVG:.svg=-grid.tex} ${PICTURES_SVG:.svg=-plain.tex} ${PICTURES_SVG:.svg=-uniform.tex} ${PICTURES_SVG:svg=pdf}

all: atemporal-compact.pdf atemporal-comparison.pdf

atemporal-compact.pdf: atemporal-compact.tex ${PICTURES}
	pdflatex $<

atemporal-comparison.pdf: atemporal-comparison.tex ${PICTURES}
	pdflatex $<

${PICTURE_DIR}/%-grid.tex: ${PICTURE_DIR}/%.svg
	cd ${PICTURE_DIR} ; make $*-grid.tex
${PICTURE_DIR}/%-uniform.tex: ${PICTURE_DIR}/%.svg
	cd ${PICTURE_DIR} ; make $*-uniform.tex
${PICTURE_DIR}/%-plain.tex: ${PICTURE_DIR}/%.svg
	cd ${PICTURE_DIR} ; make $*-plain.tex
${PICTURE_DIR}/%.pdf: ${PICTURE_DIR}/%.svg
	cd ${PICTURE_DIR} ; make $*.pdf
