-include Makefile.local

# Variables for packagers.
PREFIX=/usr
DISTRIB_DIR=gw2l_dist
ENV_DIR=gw2l_env
BUILD_DIR=_build/
BASES=$(GW2L_BASES)
#OS_TYPE=$(shell `uname -r`) ???
# [BEGIN] Generated files section

CPPO_D=$(GWDB_D) $(OS_D) $(SYSLOG_D) $(SOSA_D)

COMMIT_ID := $(shell git rev-parse --short HEAD)

bin/gwrepl/.depend:
	@echo -n "Generating $@…"
	@pwd > $@
	@dune top bin/gwrepl >> $@
	@echo " Done."

.PHONY:hd/etc/version.txt

# [End] Generated files section

GENERATED_FILES_DEP = \
	dune-workspace

generated: $(GENERATED_FILES_DEP)

install uninstall build distrib: $(GENERATED_FILES_DEP)

fmt:
	$(RM) -r $(DISTRIB_DIR)
	dune build @fmt --auto-promote

# [BEGIN] Installation / Distribution section

build: ## Build the geneweb package (libraries and binaries)
	-dune build @fmt --auto-promote
	dune build -p gwtolatex 

BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/

distrib: build ## Build the project and copy what is necessary for distribution
	$(RM) -r $(DISTRIB_DIR)
	dune build -p gwtolatex
	mkdir $(DISTRIB_DIR)
	mkdir $(DISTRIB_DIR)/tmp
	cp $(BUILD_DIR)default/bin/mkTex/mkTex.exe $(DISTRIB_DIR)/mkTex$(EXT)
	cp $(BUILD_DIR)default/bin/mkTweekInd/mkTweekInd.exe $(DISTRIB_DIR)/mkTweekInd$(EXT)
	cp $(BUILD_DIR)default/bin/mkNewGw/mkNewGw.exe $(DISTRIB_DIR)/mkNewGw$(EXT)
	cp $(BUILD_DIR)default/bin/mkBook/mkBook.exe $(DISTRIB_DIR)/mkBook$(EXT)
	cp -R $(ENV_DIR)/tex $(BASES)/etc
	cp -R $(ENV_DIR)/tex $(DISTRIB_DIR)
	cp -R $(ENV_DIR)/append-annex.sh $(DISTRIB_DIR)
	cp $(ENV_DIR)/Blank.pdf $(DISTRIB_DIR)
	# Apple extended attributes
	# xattr -d com.apple.quarantine $(DISTRIB_DIR)/make-*.sh;
	cp $(ENV_DIR)/Gw2LaTeX-env.tex $(DISTRIB_DIR)
	cp ./version.txt ./tmp
	echo ", commit: " >> ./tmp
	echo $(COMMIT_ID) >> ./tmp
	cp ./tmp $(DISTRIB_DIR)/version.txt
	$(RM) ./tmp

install: distrib
	$(RM) -r $(BASES)/$(DISTRIB_DIR)
	cp -R $(DISTRIB_DIR) $(BASES)
	
clean:
	@echo -n "Cleaning…"
	@$(RM) $(GENERATED_FILES_DEP)
	@$(RM) -r $(DISTRIB_DIR)
	@$(RM) tmp/*
	@dune clean
	@echo " Done."
.PHONY: clean

