PDFLATEX = pdflatex
# BIBTEX = bibtex
# OTT = ott
# OTT_FLAGS := -tex_wrap false -tex_show_meta true -picky_multiple_parses false
SKIM := skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)
PDF := main.pdf

all: pdf clean
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH
  ifdef SKIMRevinPath
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
  endif

pdf : main.pdf ref.bib

# quick : main.tex ref.bib Makefile
quick : main.tex Makefile
	$(PDFLATEX) -jobname=main main.tex
        # This is for my private machine.  It forces my PDF reader to reload.
        # It should not run unless "skim_revert.sh" is in your PATH.
	ifdef SKIMRevinPath
		@$(SKIM) $(PDF) &>/dev/null
		@$(SKIM) $(PDF) &>/dev/null
		@$(SKIM) $(PDF) &>/dev/null
	endif

# Now this takes the full LaTex translation and compiles it using
# pdflatex.
main.pdf : main.tex Makefile categorical-models.tex ref.bib
	$(PDFLATEX) -jobname=main main.tex
	$(BIBTEX) main
	$(PDFLATEX) -jobname=main main.tex
	$(PDFLATEX) -jobname=main main.tex

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *~ *.vtc *.out *.spl *-inc.tex
