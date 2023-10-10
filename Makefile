-include Makefile.local

# Variables for packagers.
PREFIX=/usr
DISTRIB_DIR=distribution
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
	dune-workspace \

generated: $(GENERATED_FILES_DEP)

install uninstall build distrib: $(GENERATED_FILES_DEP)

fmt:
	$(RM) -r $(DISTRIB_DIR)
	dune build @fmt --auto-promote

# [BEGIN] Installation / Distribution section

build: ## Build the geneweb package (libraries and binaries)
build:
	-dune build @fmt --auto-promote
	dune build -p gwtolatex 

BUILD_DISTRIB_DIR=$(BUILD_DIR)/bin/

distrib: ## Build the project and copy what is necessary for distribution
distrib:
	$(RM) -r $(DISTRIB_DIR)
	dune build -p gwtolatex
	mkdir $(DISTRIB_DIR)
	mkdir $(DISTRIB_DIR)/etc
	mkdir $(DISTRIB_DIR)/livres
	cp $(BUILD_DIR)default/bin/gwtolatex.exe $(DISTRIB_DIR)/gwl$(EXT);
	cp -R ./tex $(DISTRIB_DIR)/etc;

clean:
	@echo -n "Cleaning…"
	@$(RM) $(GENERATED_FILES_DEP)
	@$(RM) -r $(DISTRIB_DIR)
	@$(RM) livres/*
	@dune clean
	@echo " Done."
.PHONY: clean

