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
	@$(SKIM) Elle.pdf &>/dev/null
	@$(SKIM) Elle.pdf &>/dev/null
	@$(SKIM) Elle.pdf &>/dev/null
  endif

pdf : Elle.pdf

Elle-output.tex : Elle.ott Elle.tex
	$(OTT) $(OTT_FLAGS) -i Elle.ott -o Elle-ott.tex -tex_filter Elle.tex Elle-output.tex

Elle.pdf : Elle.tex Makefile Elle-output.tex
	$(PDFLATEX) -jobname=Elle Elle-output.tex

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
