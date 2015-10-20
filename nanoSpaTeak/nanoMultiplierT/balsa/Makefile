### Balsa Makefile
### Created: Fri Oct 31 16:13:02 GMT 2008
### By: tarazonl@royaloak.cs.man.ac.uk (Linux)
### With balsa-make-makefile version: 3.5.1
### Command: balsa-make-makefile -b -p /mnt/scratch/tarazonl/designs/nanoMultiplier/balsa

### Must use a Bourne shell
SHELL = /bin/sh

### Programs
BALSAC = balsa-c
BALSAMAKETEST = balsa-make-test -b
BALSAMAKEIMPLTEST = balsa-make-impl-test -b
BALSAMAKEBUILTIN = balsa-make-builtin-lib
BALSASIMIMPL = balsa-sim-impl -b
BALSANETLIST = balsa-netlist
BREEZESIM = breeze-sim
BREEZESIMCTRL = breeze-sim-ctrl
GTKWAVE = gtkwave
MAKE = make
RM = rm -f
RM_R = rm -rf
LN_S = ln -s

### Options
# -- BALSAIMPORTPATH defined by user
BALSACOPTS_COMPLETE = -b $(BALSAIMPORTPATH) $(BALSACOPTS)
BALSANETLISTOPTS = $(BALSAIMPORTPATH)

### Implementation technology

### User defined variables
BALSAIMPORTPATH = -I . 

### Suffix rules
.SUFFIXES: .balsa .breeze .ps

.breeze.ps:
	breeze2ps $*
.balsa.breeze:
	$(BALSAC) $(BALSACOPTS_COMPLETE) $*

all: nanoMultiplier.breeze nanoMultSupport.breeze nanoMulTypes.breeze nanoMBoothR3rolled.breeze 
### Balsa rules
nanoMBoothR3rolled.breeze: nanoMulTypes.breeze
nanoMultSupport.breeze: nanoMulTypes.breeze
nanoMultiplier.breeze: nanoMulTypes.breeze nanoMultSupport.breeze nanoMBoothR3rolled.breeze

### Test harness rules
### Implementation rules
### Implementation simulations
### Miscellaneous rules
.PHONY: depend clean very-clean ps cost help

depend:
	balsa-make-makefile -b -p /mnt/scratch/tarazonl/designs/nanoMultiplier/balsa

clean:
	$(RM)
	$(RM) nanoMBoothR3rolled.breeze nanoMulTypes.breeze nanoMultSupport.breeze nanoMultiplier.breeze
	$(RM) nanoMBoothR3rolled.ps nanoMulTypes.ps nanoMultSupport.ps nanoMultiplier.ps

very-clean: clean
	$(RM)
	$(RM_R) .libs

ps: nanoMultiplier.ps nanoMultSupport.ps nanoMulTypes.ps nanoMBoothR3rolled.ps

cost: nanoMultiplier.breeze nanoMultSupport.breeze nanoMulTypes.breeze nanoMBoothR3rolled.breeze
	breeze-cost nanoMultiplier.breeze
	breeze-cost nanoMultSupport.breeze
	breeze-cost nanoMulTypes.breeze
	breeze-cost nanoMBoothR3rolled.breeze

help:
	@echo 'Target              Meaning'
	@echo '------------------- ---------------------------------------------------'
	@echo 'clean               Remove auto-generated files (except test harnesses)'
	@echo 'cost                Estimate Breeze block costs'
	@echo 'ps                  Generate PostScript graphs for a Breeze files'
	@echo 'suffix rule .breeze Compile Balsa to Breeze'
	@echo 'suffix rule .ps     Generate a PostScript graph of a Breeze file'
	@echo 'very-clean          Remove all auto-generated files'
	@echo

