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
	@$(SKIM) LNL.pdf &>/dev/null
	@$(SKIM) LNL.pdf &>/dev/null
	@$(SKIM) LNL.pdf &>/dev/null
  endif

pdf : LNL.pdf

LNL-output.tex : LNL.ott LNL.tex
	$(OTT) $(OTT_FLAGS) -i LNL.ott -o LNL-ott.tex -tex_filter LNL.tex LNL-output.tex

LNL.pdf : LNL.tex Makefile LNL-output.tex
	$(PDFLATEX) -jobname=LNL LNL-output.tex

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
