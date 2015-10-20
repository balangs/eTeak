HLDFLAGS="-package mtl @PACKAGES@"
HFLAGS="-O2 -Wall -fno-warn-name-shadowing @GHC_FLAGS@ -DHAS_GTK2HS=@HAS_GTK2HS@"
export HLDFLAGS
export HFLAGS

../bin/makemake teak \
	Balsa.hs BalsaLexer.hs BalsaParser.hs Bind.hs Bits.hs \
	Call.hs Chan.hs Config.hs Context.hs \
	Dot.hs \
	Eval.hs Expr.hs \
	Finish.hs \
	Gates.hs Gen.hs Graph.hs Gui.hs GuiSupport.hs \
	Latch.hs Layout.hs Lexer.hs \
	Main.hs Misc.hs Monitor.hs \
	Network.hs NetParts.hs \
	Optim.hs Options.hs \
	Parser.hs ParseTree.hs Plot.hs Print.hs \
	Report.hs Rule.hs \
	Show.hs SimBuiltin.hs Sim.hs SimPN.hs SimTypes.hs State.hs \
	Scp.hs DelayCal.hs \
	Teak.hs ToolOptions.hs TeakScript.hs Traverse.hs Type.hs > Makefile.am

cat >> Makefile.am <<EOF
EXTRA_DIST = makemake.sh
EOF
