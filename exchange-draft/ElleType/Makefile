PDFLATEX = pdflatex
BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_show_meta false -tex_wrap false
SKIM := skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

all: pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath	
	@$(SKIM) ElleType.pdf &>/dev/null
	@$(SKIM) ElleType.pdf &>/dev/null
	@$(SKIM) ElleType.pdf &>/dev/null
  endif

pdf : ElleType.pdf

ElleType-output.tex : ElleType.ott ElleType.tex
	$(OTT) $(OTT_FLAGS) -i ElleType.ott -o ElleType-ott.tex -tex_filter ElleType.tex ElleType-output.tex

ElleType.pdf : ElleType.tex Makefile ElleType-output.tex
	$(PDFLATEX) -jobname=ElleType ElleType-output.tex

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
