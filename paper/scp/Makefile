PAPER = scp
TEX = $(wildcard *.tex)
BIB = ref.bib

.PHONY: clean

$(PAPER).pdf: $(TEX) $(BIB)
	pdflatex $(PAPER)
	bibtex $(PAPER)
	pdflatex $(PAPER)
	pdflatex $(PAPER)

cover_letter.pdf: cover_letter.tex
	pdflatex cover_letter

clean:
	rm -f *.aux *.bbl *.blg *.log *.out *.pag *.pdf *.spl
