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

logic-ott.tex : logic.tex Elle-ND/Elle-ND.ott Elle/Elle.ott Elle-to-LNL-ott.tex
	@$(OTT) $(OTT_FLAGS) -i Elle-ND/Elle-ND.ott -o Elle-ND-inc.tex -tex_name_prefix ND \
		-tex_filter logic.tex logic-ott1.tex
	scripts/prepr_double_ott.sh logic-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i Elle/Elle.ott -o Elle-inc.tex -tex_name_prefix Elle \
		-tex_filter logic-ott1.tex logic-ott.tex

LAM-ott.tex : LAM/LAM.ott LAM.tex
	@$(OTT) $(OTT_FLAGS) -i LAM/LAM.ott -o LAM-inc.tex -tex_name_prefix LAM \
		-tex_filter LAM.tex LAM-ott.tex

ott : main.text logic-ott.tex Makefile

# Now this takes the full LaTex translation and compiles it using
# pdflatex.
# main.pdf : main.tex Makefile adjoint-model.tex ref.bib
$(PDF) : main.tex Makefile ref.bib Makefile introduction.tex adjoint-models.tex LAM-ott.tex
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	bibtex $(TexFileName)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

quick-pdf : $(OTTOutputFile) Makefile introduction.tex
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *~ *.vtc *.out *.spl *-inc.tex