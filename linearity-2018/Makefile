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

logic-output.tex : logic.tex Elle-ND/Elle-ND.ott Elle/Elle.ott Elle-to-LNL-inc.tex
	@$(OTT) $(OTT_FLAGS) -i Elle-ND/Elle-ND.ott -o Elle-ND-inc.tex -tex_name_prefix ND \
		-tex_filter logic.tex logic-output1.tex
	scripts/prepr_double_ott.sh logic-output1.tex
	@$(OTT) $(OTT_FLAGS) -i Elle/Elle.ott -o Elle-output.tex -tex_name_prefix Elle \
		-tex_filter logic-output1.tex logic-output.tex

LAM-output.tex : Elle-ND/Elle-ND.ott LAM.tex
	@$(OTT) $(OTT_FLAGS) -i Elle-ND/Elle-ND.ott -o Elle-ND-inc.tex -tex_name_prefix ND \
		-tex_filter LAM.tex LAM-output.tex	

# Now this takes the full LaTex translation and compiles it using
# pdflatex.
# main.pdf : main.tex Makefile adjoint-model.tex ref.bib
$(PDF) : main.tex Makefile ref.bib Makefile introduction.tex adjoint-models.tex LAM-output.tex dialectica-spaces.tex fwork.tex
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	bibtex $(TexFileName)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

quick-pdf : $(OTTOutputFile) Makefile introduction.tex
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

clean :
	rm -f *.aux *.dvi *.ps *.log *-output.tex *.bbl *.blg *.rel *~ *.vtc *.out *.spl *-inc.tex *.pag
