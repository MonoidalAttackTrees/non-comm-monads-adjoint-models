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
	@$(SKIM) Elle-ND.pdf &>/dev/null
	@$(SKIM) Elle-ND.pdf &>/dev/null
	@$(SKIM) Elle-ND.pdf &>/dev/null
  endif

pdf : Elle-ND.pdf

Elle-ND-output.tex : Elle-ND.ott Elle-ND.tex
	$(OTT) $(OTT_FLAGS) -i Elle-ND.ott -o Elle-ND-ott.tex -tex_filter Elle-ND.tex Elle-ND-output.tex

Elle-ND.pdf : Elle-ND.tex Makefile Elle-ND-output.tex
	$(PDFLATEX) -jobname=Elle-ND Elle-ND-output.tex

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
