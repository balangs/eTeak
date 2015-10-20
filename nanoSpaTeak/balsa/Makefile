### Balsa Makefile
### Created: Thu Sep 30 02:32:11 CST 2010
### By: rhg_001@rhg-desktop (Linux)
### With balsa-make-makefile version: 4.0
### Command: balsa-make-makefile -b -p /home/rhg_001/teak/examples/nanoSpaTeak/balsa

### Must use a Bourne shell
SHELL = /bin/sh

### Programs
BALSAC = balsa-c
BALSAMAKETEST = balsa-make-test -b
BALSAMAKEIMPLTEST = balsa-make-impl-test -b
BALSAMAKEBUILTIN = balsa-make-builtin-lib
BALSASIMIMPL = balsa-sim-impl -b
BALSANETLIST = balsa-netlist
BREEZE2PS = breeze2ps
BREEZESIM = breeze-sim
BREEZESIMCTRL = breeze-sim-ctrl
BALSAWAVE = balsa-wave
MAKE = make
RM = rm -f
RM_R = rm -rf

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
	$(BREEZE2PS) $(BALSAIMPORTPATH) $*
.balsa.breeze:
	$(BALSAC) $(BALSACOPTS_COMPLETE) $*

all: sim.breeze pipelineRegister.breeze nanoSpaTypes.breeze nanoSpaHarvard_deparameterised.breeze nanoSpaHarvard.breeze nanoSpa.breeze nanoShifter.breeze nanoRegBank.breeze nanoMultiplier.breeze nanoMultSupport.breeze nanoMBoothR3unrolled.breeze nanoMBoothR3rolled.breeze nanoLSMcontrol.breeze nanoFetch.breeze nanoExecuteSupport.breeze nanoExecuteControl.breeze nanoExecute.breeze nanoDecode.breeze nanoAlu.breeze 
### Balsa rules
nanoAlu.breeze: nanoSpaTypes.breeze
nanoDecode.breeze: nanoSpaTypes.breeze nanoLSMcontrol.breeze nanoExecuteSupport.breeze
nanoExecuteControl.breeze: nanoSpaTypes.breeze
nanoExecuteSupport.breeze: nanoSpaTypes.breeze pipelineRegister.breeze
nanoExecute.breeze: nanoSpaTypes.breeze nanoExecuteControl.breeze nanoExecuteSupport.breeze nanoAlu.breeze nanoShifter.breeze nanoMultiplier.breeze
nanoFetch.breeze: nanoSpaTypes.breeze
nanoLSMcontrol.breeze: nanoSpaTypes.breeze
nanoMBoothR3rolled.breeze: nanoSpaTypes.breeze
nanoMBoothR3unrolled.breeze: nanoSpaTypes.breeze
nanoMultSupport.breeze: nanoSpaTypes.breeze
nanoMultiplier.breeze: nanoSpaTypes.breeze nanoMultSupport.breeze nanoMBoothR3rolled.breeze nanoMBoothR3unrolled.breeze nanoExecuteSupport.breeze
nanoRegBank.breeze: nanoSpaTypes.breeze
nanoShifter.breeze: nanoSpaTypes.breeze
nanoSpaHarvard.breeze: nanoSpaTypes.breeze nanoFetch.breeze nanoDecode.breeze nanoExecute.breeze nanoRegBank.breeze pipelineRegister.breeze
nanoSpaHarvard_deparameterised.breeze: nanoSpaTypes.breeze nanoSpaHarvard.breeze
nanoSpa.breeze: nanoSpaTypes.breeze nanoSpaHarvard.breeze
pipelineRegister.breeze: nanoSpaTypes.breeze
sim.breeze: nanoSpaHarvard.breeze

### Test harness rules
### Implementation rules
### Implementation simulations
### Miscellaneous rules
.PHONY: depend clean very-clean ps cost help

depend:
	balsa-make-makefile -b -p /home/rhg_001/teak/examples/nanoSpaTeak/balsa

clean:
	$(RM) nanoAlu.breeze nanoDecode.breeze nanoExecuteControl.breeze nanoExecuteSupport.breeze nanoExecute.breeze nanoFetch.breeze nanoLSMcontrol.breeze nanoMBoothR3rolled.breeze nanoMBoothR3unrolled.breeze nanoMultSupport.breeze nanoMultiplier.breeze nanoRegBank.breeze nanoShifter.breeze nanoSpaHarvard.breeze nanoSpaHarvard_deparameterised.breeze nanoSpaTypes.breeze nanoSpa.breeze pipelineRegister.breeze sim.breeze
	$(RM) nanoAlu.ps nanoDecode.ps nanoExecuteControl.ps nanoExecuteSupport.ps nanoExecute.ps nanoFetch.ps nanoLSMcontrol.ps nanoMBoothR3rolled.ps nanoMBoothR3unrolled.ps nanoMultSupport.ps nanoMultiplier.ps nanoRegBank.ps nanoShifter.ps nanoSpaHarvard.ps nanoSpaHarvard_deparameterised.ps nanoSpaTypes.ps nanoSpa.ps pipelineRegister.ps sim.ps

very-clean: clean
	$(RM_R) .libs

ps: sim.ps pipelineRegister.ps nanoSpaTypes.ps nanoSpaHarvard_deparameterised.ps nanoSpaHarvard.ps nanoSpa.ps nanoShifter.ps nanoRegBank.ps nanoMultiplier.ps nanoMultSupport.ps nanoMBoothR3unrolled.ps nanoMBoothR3rolled.ps nanoLSMcontrol.ps nanoFetch.ps nanoExecuteSupport.ps nanoExecuteControl.ps nanoExecute.ps nanoDecode.ps nanoAlu.ps

cost: sim.breeze pipelineRegister.breeze nanoSpaTypes.breeze nanoSpaHarvard_deparameterised.breeze nanoSpaHarvard.breeze nanoSpa.breeze nanoShifter.breeze nanoRegBank.breeze nanoMultiplier.breeze nanoMultSupport.breeze nanoMBoothR3unrolled.breeze nanoMBoothR3rolled.breeze nanoLSMcontrol.breeze nanoFetch.breeze nanoExecuteSupport.breeze nanoExecuteControl.breeze nanoExecute.breeze nanoDecode.breeze nanoAlu.breeze
	breeze-cost sim.breeze
	breeze-cost pipelineRegister.breeze
	breeze-cost nanoSpaTypes.breeze
	breeze-cost nanoSpaHarvard_deparameterised.breeze
	breeze-cost nanoSpaHarvard.breeze
	breeze-cost nanoSpa.breeze
	breeze-cost nanoShifter.breeze
	breeze-cost nanoRegBank.breeze
	breeze-cost nanoMultiplier.breeze
	breeze-cost nanoMultSupport.breeze
	breeze-cost nanoMBoothR3unrolled.breeze
	breeze-cost nanoMBoothR3rolled.breeze
	breeze-cost nanoLSMcontrol.breeze
	breeze-cost nanoFetch.breeze
	breeze-cost nanoExecuteSupport.breeze
	breeze-cost nanoExecuteControl.breeze
	breeze-cost nanoExecute.breeze
	breeze-cost nanoDecode.breeze
	breeze-cost nanoAlu.breeze

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

