PDFLATEX = pdflatex
# BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_wrap false -tex_show_meta true -picky_multiple_parses false
SKIM := skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

TexFileName := main
OTTOutputFile := main.tex
OTTPrefix := Ott

Main := $(TexFileName).tex
References := references.bib

PDF := $(TexFileName).pdf

all: pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH
  ifdef SKIMRevinPath
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
  endif

quick : quick-pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
  endif

pdf : $(PDF)

natural-deduction-ott.tex : natural-deduction.tex Elle-ND/Elle-ND.ott
	@$(OTT) $(OTT_FLAGS) -i Elle-ND/Elle-ND.ott -o Elle-ND-inc.tex -tex_name_prefix ND \
		-tex_filter natural-deduction.tex natural-deduction-ott.tex

sequent-calculus-ott.tex : sequent-calculus.tex Elle/Elle.ott
	@$(OTT) $(OTT_FLAGS) -i Elle/Elle.ott -o Elle-inc.tex -tex_name_prefix Elle \
		-tex_filter sequent-calculus.tex sequent-calculus-ott.tex

ott : main.text natural-deduction-ott.tex sequent-calculus-ott.tex Makefile

# Now this takes the full LaTex translation and compiles it using
# pdflatex.
# main.pdf : main.tex Makefile adjoint-model.tex ref.bib
$(PDF) : main.tex Makefile adjoint-model.tex ref.bib natural-deduction-ott.tex sequent-calculus-ott.tex Makefile
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

quick-pdf : $(OTTOutputFile) Makefile
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *~ *.vtc *.out *.spl *-inc.tex
