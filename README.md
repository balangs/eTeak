NOTE: eTeak's GALS/synchronous backend is available for teaching and academic research. Please don't hesitate to contact us for more information. The current version is the asynchronous (dual-rail 4-phase RTZ) beackend and is available to public. Moreover eTeak's multiple-clock syntehsis feature is a WIP by the APT group of the University of Manchetser, UK.  

#eTeak

This is the eTeak system. A synchronous/asycnhronous synthesis backend for the Balsa language.

#Building and running
Install [stack](https://github.com/commercialhaskell/stack)

If you're building from git, first you need to

	install automake autoconf libgtk2.0-dev

If you've done that or you're building from tarball, proceed to:

	stack setup --upgrade-cabal
	
	stack build 

	stack exec -- eTeak --gui

# Top level layout
- NEWS  
  Description of language change information for Balsa from the Balsa system verison 3.5's view of the
  Balsa language.
- TODO  
  A to-do/wish list for changes to this package.
- aschem/  
  ASCII art schematic interpreter
- bin/  
  Scripts to install in ${prefix}/bin
- doc/  
  Documentation
- examples/  
  Balsa examples (some including Teak run scripts) used to test `teak'
- library/  
  Balsa language libraries e.g. [teak.builtin].  Gradualy redefining the unused/simulation libraries
  to be newer/re-thought-out for Teak
- runtime/  
  Runtime library (runtime/verilog/: in Verilog) supporting behavioural Balsa statements in simulation
- src/  
  Source of eTeak

