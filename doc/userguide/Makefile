

TARGETS = pdf

all: ${TARGETS}

nraotexfiles = \
	difx_setup.tex \
	difx_vlba.tex \
	difx_database.tex \
	difx_installation.tex 

texfiles = \
	difx_versions.tex \
	difx_sources.tex \
	difx_formats.tex \
	difx_pulsars.tex \
	difx_conventions.tex \
	difx_programs.tex \
	difx_files.tex \
	difx_xml.tex \
	difx_messages.tex \
	difx_endmatter.tex 

figfiles = \
	swcorr_block_diagram.pdf \
	difxdependency.pdf \
	tree.pdf

pdf: UserGuide.pdf 

swcorr_block_diagram.pdf: swcorr_block_diagram.eps
	ps2pdf swcorr_block_diagram.eps

UserGuide.pdf: ${texfiles} ${nraotexfiles} ${figfiles} UserGuide.tex
	pdflatex UserGuide.tex
	pdflatex UserGuide.tex

DiFXUserGuide.pdf: ${texfiles} ${figfiles} DiFXUserGuide.tex 
	pdflatex DiFXUserGuide.tex
	pdflatex DiFXUserGuide.tex


web: ${texfiles} ${figfiles.pdf}
	rm -rf UserGuide
	latex2html -image_type png -split 5 UserGuide.tex

clean:
	rm -f *.log *.dvi *.ps *.aux UserGuide.pdf
	rm -rf UserGuide/
