-include Makefile.local

# Variables for packagers.
PREFIX=/usr
DISTRIB_DIR=gw2l_dist
BUILD_DIR=_build/

# [BEGIN] Generated files section

CPPO_D=$(GWDB_D) $(OS_D) $(SYSLOG_D) $(SOSA_D)

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
	mkdir $(DISTRIB_DIR)/etc
	mkdir $(DISTRIB_DIR)/tmp
	cp $(BUILD_DIR)default/bin/mkTex/mkTex.exe $(DISTRIB_DIR)/mkTex$(EXT)
	cp $(BUILD_DIR)default/bin/mkTweekIndSort/mkTweekIndSort.exe $(DISTRIB_DIR)/mkTweekIndSort$(EXT)
	cp $(BUILD_DIR)default/bin/mkTweekIndMerge/mkTweekIndMerge.exe $(DISTRIB_DIR)/mkTweekIndMerge$(EXT)
	cp $(BUILD_DIR)default/bin/mkImgDict/mkImgDict.exe $(DISTRIB_DIR)/mkImgDict$(EXT)
	cp $(BUILD_DIR)default/bin/mkNewGw/mkNewGw.exe $(DISTRIB_DIR)/mkNewGw$(EXT)
	cp $(BUILD_DIR)default/bin/mkBook/mkBook.exe $(DISTRIB_DIR)/mkBook$(EXT)
	cp -R ./tex $(DISTRIB_DIR)
	cp ./gwl.sh $(DISTRIB_DIR)
	cp ./chausey.sh $(DISTRIB_DIR)
	cp ./Gw2LaTeX-env.tex $(DISTRIB_DIR)
	cp ../geneweb/hd/etc/version.txt $(DISTRIB_DIR)/gw_version.txt
	cp ./version.txt $(DISTRIB_DIR)

clean:
	@echo -n "Cleaning…"
	@$(RM) $(GENERATED_FILES_DEP)
	@$(RM) -r $(DISTRIB_DIR)
	@$(RM) tmp/*
	@dune clean
	@echo " Done."
.PHONY: clean

