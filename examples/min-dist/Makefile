PICTURE_DIR    := Pictures
PICTURES_SVG   := ${wildcard ${PICTURE_DIR}/*.svg}
PICTURES_SVG   := ${PICTURES_SVG:svg=tex}
PICTURES       := ${wildcard ${PICTURE_DIR}/*.tex}

all: min-dist.pdf

min-dist.pdf: min-dist.tex ${PICTURES}
	pdflatex $<

${PICTURE_DIR}/%:
	cd ${PICTURE_DIR} ; make $*
