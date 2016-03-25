## Process this file with automake to produce Makefile.in

all: manual.pdf

EXTRA_DIST = \
	manual.lout \
	book \
	lout.sh \
	macros \
	flow.dot \
	flow-optim.dot \
	gui.png \
	$(LOUT_FILES) \
	$(CMD_BALSA) \
	$(ASCHEMS) \
	traverse.svg \
	files

ASCHEM = stack exec aschem --
TEAK = stack exec eTeak --
PS2PDF = ps2pdf -sPAPERSIZE=a4
PS2EPSI = ps2epsi
DOT = dot
CONVERT = convert

OPTIM_RULES = ../../optim/default.rules

optim.lout: $(OPTIM_RULES)
	$(TEAK) -r $(OPTIM_RULES) --rules-test-card optim.lout SubAppendix

manual.pdf: manual.ps
	$(PS2PDF) manual.ps manual.pdf

SMALL_PLOT_OPTS = size=50x50:paper=50x50:no-box:no-title

LOUT_FILES = compilation.lout parsetree.lout grammar.lout

CMD_BALSA = NoCmd.balsa SeqCmd.balsa ParCmd.balsa InputCmd.balsa OutputCmd.balsa AssignCmd.balsa SinkCmd.balsa \
	CaseCmd.balsa PrintCmd.balsa WhileCmd1.balsa WhileCmd2.balsa WhileCmd3.balsa LoopCmd.balsa \
	BinExpr.balsa CaseExpr.balsa UnExpr.balsa BuiltinCallExpr.balsa ConsExpr.balsa AppendExpr.balsa \
	BitfieldExpr.balsa ExtendExpr.balsa ValueExpr.balsa VarRead.balsa OpenChanRead.balsa

CMD_PS = NoCmd.cps SeqCmd.cps ParCmd.cps InputCmd.cops OutputCmd.cops AssignCmd.cops SinkCmd.cps CaseCmd.cps \
	PrintCmd.cps WhileCmd1.cps WhileCmd2.cps WhileCmd3.cps LoopCmd.cps \
	BinExpr.ceps CaseExpr.ceps UnExpr.ceps BuiltinCallExpr.ceps ConsExpr.ceps AppendExpr.ceps \
	BitfieldExpr.ceps ExtendExpr.ceps ValueExpr.ceps VarRead.ceps OpenChanRead.ceps

ASCHEMS = m.aschem m0.aschem

ASCHEM_PS = m.eps m0.eps

SUFFIXES = .cps .cops .ceps .eps .svg

.aschem.eps:
	$(ASCHEM) $< $*.ps
	$(PS2EPSI) $*.ps $@
	rm -f $*.ps

.balsa.cps:
	$(TEAK) --plot -p parts=$*:$(SMALL_PLOT_OPTS) -o $* $< && mv $*.ps $@

.balsa.cops:
	$(TEAK) --plot -p parts=$*:$(SMALL_PLOT_OPTS) -O -t $* -o $* $< && mv $*.ps $@

.balsa.ceps:
	$(TEAK) --plot -p parts=$*:$(SMALL_PLOT_OPTS):labels=$*.show -o $* $< && mv $*.ps $@

.svg.eps:
	convert $< $@

manual.ps: manual.lout book macros flow.eps flow-optim.eps gui.eps optim.lout $(CMD_PS) $(ASCHEM_PS) traverse.eps $(LOUT_FILES)
	./lout.sh manual.lout

flow.eps: flow.dot
	$(DOT) -Teps $< > $@

flow-optim.eps: flow-optim.dot
	$(DOT) -Teps $< > $@

gui.eps: gui.png
	$(CONVERT) gui.png gui.eps

clean:
	rm -rf *.eps
	rm -rf *.ps *.pdf
	rm -rf *.cps *.cops *.ceps
	rm -rf *.ld *.li *.ldx *.lix
	rm -rf log oldlog
	rm -rf optim-*.dot optim-*.dot1
	rm -rf optim.lout
	rm -rf *.teak
