PDFLATEX = pdflatex
BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_show_meta false -tex_wrap true
SKIM := skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

all: pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath	
	@$(SKIM) LAM.pdf &>/dev/null
	@$(SKIM) LAM.pdf &>/dev/null
	@$(SKIM) LAM.pdf &>/dev/null
  endif

pdf : LAM.pdf

LAM.tex : LAM.ott
	$(OTT) $(OTT_FLAGS) -i LAM.ott -o LAM.tex

LAM.pdf : LAM.tex Makefile
	$(PDFLATEX) -jobname=LAM LAM.tex

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
