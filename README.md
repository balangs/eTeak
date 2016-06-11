NOTE: eTeak's GALS/synchronous backend is available for teaching and academic research. Please don't hesitate to contact us for more information. The current version is the asynchronous (dual-rail 4-phase RTZ) beackend and is available to public. Moreover eTeak's multiple-clock syntehsis feature is a WIP by the APT group of the University of Manchetser, UK.  

#The eTeak Synthesis Frameowrk

This is eTeak. A synchronous/asycnhronous synthesis backend for the CSP-based language of Balsa. eTeak inherits the following from its predecessor system, [Teak](http://apt.cs.manchester.ac.uk/projects/teak/):   

- A synthesiser from Balsa to Teak component networks
- A mechanism to plot those networks
- A language-level simulator for Balsa
- A programmable peephole optimiser for component networks
- A GUI to drive and visualise optimisation choices
- A prototype `back end' to generate Verilog gate-level implementations of Teak components

What's new about eTeak:

- A Synchronous Elastic Dataflow backend for Balsa language
- Adopts Synchronous Elastic Protocol (SELF)
- Supports Synchronous, Asynchronous and Elastic protocols towards GALS synthesis
- Inherits a powerful visualisation engine from Teak to visualise synchronous and mixed signal interactions
- Supports De-Elastisation (From Asynchrony to Synchrony) and De-Synchronisation (From Synchrony to Asynchrony)
- Fast growing collaboration between Academia and Industry

Despite the fact that there are many High-Level Synthesis tools developed in Academia and Industry, just a handful of them provide the source code for the researchers including LegUP and Chisel (which has been rebranded from a HLS tool to a HDL flow suitable for constructing large-scale hardware). eTeak is the first open-source framework that exploits asynchronous synthesis techniques to realise fine-grained synchronous circuits capable of running at different clock frequencies.


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

