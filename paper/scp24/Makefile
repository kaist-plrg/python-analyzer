PACKAGE=sn-jnl
SRC=$(wildcard *.tex)

all: sn-jnl.cls ase.pdf

%.pdf: %.tex macro.tex intro.tex background.tex background_tf1.tex trans.tex trans_patterns.tex impl.tex overview.tex parse.tex cha.tex pattern.tex eval.tex discussion.tex related.tex conclusion.tex ref.bib $(PACKAGE).cls 
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
	*.fdb_latexmk *.fls *.synctex.gz ase.pdf *.pag

.PHONY: all clean
