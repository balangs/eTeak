NOTE: eTeak's GALS/synchronous backend is available for teaching and academic research. Please don't hesitate to contact us for more information. The current version is the asynchronous (dual-rail 4-phase RTZ) beackend and is available to public. Moreover eTeak's multiple-clock syntehsis feature is a WIP by the APT group of the University of Manchetser, UK.  

#The eTeak Synthesis Framework

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
<<<<<<< HEAD
=======

### Download eTeak on a [Virtual Machine](https://github.com/balangs/eTeak-vmware)

Please download our provided VirtualBox machine image to easily run eTeak on a Linux, Windows or Apple machine. The virtual machine is running Ubuntu 15.04 with the required packages for installing eTeak. Therefore you could either use the installed version by running `stack exec -- eTeak --gui` in the $HOME/Desktop/eTeak-master directory or cloning the git repository using `git clone git@github.com:balangs/eTeak.git`. You'd need to go through the steps above to build the stack.  
>>>>>>> d81ebbd58b125bbc5d741b199eff99b074a4f734

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

## Acknowledgement

eTeak's synchronous (Elastic) and GALS extensions have been sponsored by EPSRC Full Research Scholarship under research grant EP/I038306/1 from 2012 to 2015 and EPSRC Doctoral Prize Fellowship 2015/15.


## References [Publications](https://github.com/balangs/Documentation/tree/master/Peer-reviewed_Publications)

- Mahdi Jelodari Mamaghani, Milos Krtic, Jim Garside, "Automatic Clock (AutoCLK): A Promising Approach Towards GALSification," In Proc. of 22nd IEEE International Symposium on Asynchronous Circuits and Systems (ASYNC), May 2016.
- Mahdi Jelodari Mamaghani, Danil Sokolov, Jim Garside, "Asynchronous Dataflow De-Elastisation for Efficient Heterogeneous Synthesis," In Proc. of the 16th International Conference on Application of Concurrency to System Design (ACSD), Poland, June 2016.
- Mahdi Jelodari Mamaghani, Jim Garside, Doug Edwards, "De-Elastisation: From Asynchronous Dataflows to Synchronous Circuits," (to appear) IEEE/ACM conference on Design Automation and Test in Europe (DATE), March 2015, Gernoble, France. [DATE Best IP Award Winner].
- Mahdi Jelodari Mamaghani, Jim Garside, Will Toms, Doug Edwards, "Optimised Synthesis of Asynchronous Elastic Dataflows by Leveraging Clocked EDA," The Euromicro Conference on Digital System Design (DSD), August 2014, Verona, Italy.
- Mahdi Jelodari Mamaghani, Will Toms, Andrew Bardsley, Jim Garside, "Exploiting Synchrony for Area and Performance Improvement in the Asynchronous Domain," Intl. Symposium on Asynchronous Circuits and Systems (ASYNC), May 2014, Potsdam, Germany.
- Mahdi Jelodari Mamaghani, Jim Garside, "High-level Synthesis of GALS Systems," Workshop on Designing with Uncertainty - Opportunities and Challenges (PAnDA), March 2014, York ,UK.
- Mahdi Jelodari Mamaghani, Will Toms, Jim Garside, "eTeak: A Data-Driven Synchronous Elastic Synthesiser," Intl. Conference on Application of Concurrency to System Design (ACSD), July 2013, Barcelona, Spain.

