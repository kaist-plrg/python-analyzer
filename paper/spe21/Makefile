PACKAGE=WileyNJD-v2

all: WileyNJD-v2.cls spe.pdf

%.pdf: %.tex macro.tex intro.tex background.tex trans.tex impl.tex overview.tex parse.tex cha.tex pattern.tex eval.tex discussion.tex related.tex conclusion.tex ref.bib $(PACKAGE).cls 
	pdflatex $<
	- bibtex $*
	pdflatex $<
	- makeindex -s gind.ist -o $*.ind $*.idx
	- makeindex -s gglo.ist -o $*.gls $*.glo
	pdflatex $<
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $*.log) \
	do pdflatex $<; done

clean:
	$(RM)  *.log *.aux \
	*.cfg *.glo *.idx *.toc \
	*.ilg *.ind *.out *.lof \
	*.lot *.bbl *.blg *.gls *.cut *.hd \
	*.dvi *.ps *.thm *.tgz *.zip *.rpi \
	*.fdb_latexmk *.fls *.synctex.gz spe.pdf *.pag

.PHONY: all clean
