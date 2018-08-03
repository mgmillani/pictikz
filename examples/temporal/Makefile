PICTURE_DIR    := Pictures
PICTURES_SVG   := ${wildcard ${PICTURE_DIR}/*.svg}
PICTURES       := ${PICTURES_SVG:svg=tex}

all: temporal.pdf

temporal.pdf: temporal.tex ${PICTURES}
	pdflatex $<

${PICTURE_DIR}/%:
	cd ${PICTURE_DIR} ; make $*
