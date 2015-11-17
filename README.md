#This is the eTeak system. A GALS back end for the Balsa language.

#NEWS
	Description of language change information for Balsa from the Balsa system verison 3.5's view of the
	Balsa language.
#TODO
	A to-do/wish list for changes to this package.
#aschem/
	ASCII art schematic interpreter
#bin/
	Scripts to install in ${prefix}/bin
#doc/
	Documentation
#examples/
	Balsa examples (some including Teak run scripts) used to test `teak'
#library/
	Balsa language libraries e.g. [teak.builtin].  Gradualy redefining the unused/simulation libraries
	to be newer/re-thought-out for Teak
#runtime/
	Runtime library (runtime/verilog/: in Verilog) supporting behavioural Balsa statements in simulation
#src/
	Source of `teak'

#Building
Install ghc 7.8 and cabal. The instructions below assume its installed as /usr/bin/ghc-7.8.

	cabal configure -w /usr/bin/ghc-7.8
	cabal build
	cabal install

To run (Assumes $HOME/.cabal/bin is on your PATH):
	eTeak --gui
