`timescale 1ns/1ps

`define balsa_simulate true
`define balsa_init_time 500
//`define balsa_switchable true

`ifdef TECHFILE
    `include `TECHFILE
`else
    `include "technology.amust018.v"
`endif

`ifdef NLSTFILE
    `include `NLSTFILE
`else
    `include "spaHarvard.v"
`endif

`ifdef OOF
`define balsa_initialise true

`include "memory_oof.v"

module test();
  reg  activate_0r;
  wire activate_0a;
  wire [18:0] d__Access_0r0d;
  wire [18:0] d__Access_0r1d;
  wire [18:0] d__Access_0r2d;
  wire [18:0] d__Access_0r3d;
  wire d__Access_0a;
  wire [18:0] i__Access_0r0d;
  wire [18:0] i__Access_0r1d;
  wire [18:0] i__Access_0r2d;
  wire [18:0] i__Access_0r3d;
  wire i__Access_0a;
  wire d__Mode_0r0d;
  wire d__Mode_0r1d;
  wire d__Mode_0a;
  wire i__Mode_0r0d;
  wire i__Mode_0r1d;
  wire i__Mode_0a;

  wire [15:0] d__Di_0a0d;
  wire [15:0] d__Di_0a1d;
  wire [15:0] d__Di_0a2d;
  wire [15:0] d__Di_0a3d;
  wire d__Di_0r;
  wire [15:0] i__Di_0a0d;
  wire [15:0] i__Di_0a1d;
	wire [15:0] i__Di_0a2d;
  wire [15:0] i__Di_0a3d;
  wire i__Di_0r;

  wire [15:0] d__Do_0r0d;
  wire [15:0] d__Do_0r1d;
  wire [15:0] d__Do_0r2d;
  wire [15:0] d__Do_0r3d;
  wire d__Do_0a;

  wire d__Abort_0a0d;
  wire d__Abort_0a1d;
  wire d__Abort_0r;
  wire i__Abort_0a0d;
  wire i__Abort_0a1d;
  wire i__Abort_0r;

  wire [1:0] cpId_0r0d;
  wire [1:0] cpId_0r1d;
  wire [1:0] cpId_0r2d;
  wire [1:0] cpId_0r3d;
  reg  cpId_0a;
  wire [16:0] cpInst_0r0d;
  wire [16:0] cpInst_0r1d;
	wire [15:0] cpInst_0r2d;
  wire [15:0] cpInst_0r3d;
  reg  cpInst_0a;
  wire cpCapableP_0r;
  reg  [1:0] cpCapableP_0a0d;
  reg  [1:0] cpCapableP_0a1d;
  reg  [1:0] cpCapableP_0a2d;
  reg  [1:0] cpCapableP_0a3d;
  wire [1:0] cpId2_0r0d;
  wire [1:0] cpId2_0r1d; 
	wire [1:0] cpId2_0r2d;
  wire [1:0] cpId2_0r3d;
  reg  cpId2_0a;
  wire [2:0] cpExec_0r0d;
  wire [2:0] cpExec_0r1d;
	wire [2:0] cpExec_0r2d;
  wire [2:0] cpExec_0r3d;
  reg  cpExec_0a;
  wire [15:0] cpRegisterSpa2cp_0r0d;
  wire [15:0] cpRegisterSpa2cp_0r1d;
	wire [15:0] cpRegisterSpa2cp_0r2d;
  wire [15:0] cpRegisterSpa2cp_0r3d;
  reg  cpRegisterSpa2cp_0a;
  wire cpRegisterCp2spa_0r;
  reg  [15:0] cpRegisterCp2spa_0a0d;
  reg  [15:0] cpRegisterCp2spa_0a1d;
	reg  [15:0] cpRegisterCp2spa_0a2d;
  reg  [15:0] cpRegisterCp2spa_0a3d;
  wire cpMemAccessLastP_0r;
  reg  cpMemAccessLastP_0a0d;
  reg  cpMemAccessLastP_0a1d;

  reg  irq_0r0d;
  reg  irq_0r1d;
  wire irq_0a;
  reg  fiq_0r0d;
  reg  fiq_0r1d;
  wire fiq_0a;

`ifdef balsa_switchable
	reg spacer;
`endif

`ifdef balsa_initialise
	reg initialise;
`endif

  integer outFile;

	memorySpecial mem (
       i__Access_0r0d, i__Access_0r1d,
			 i__Access_0r2d, i__Access_0r3d, i__Access_0a, 
       i__Mode_0r0d, i__Mode_0r1d, i__Mode_0a, i__Di_0r,
			 i__Di_0a0d, i__Di_0a1d, i__Di_0a2d, i__Di_0a3d, 
       i__Abort_0r, i__Abort_0a0d, i__Abort_0a1d, 
       d__Access_0r0d, d__Access_0r1d,d__Access_0r2d,
			 d__Access_0r3d, d__Access_0a,
	   	 d__Mode_0r0d, d__Mode_0r1d, d__Mode_0a, 
       d__Di_0r, d__Di_0a0d, d__Di_0a1d, d__Di_0a2d,
			 d__Di_0a3d, d__Do_0r0d, d__Do_0r1d,
       d__Do_0r2d, d__Do_0r3d, d__Do_0a, 
       d__Abort_0r, d__Abort_0a0d, d__Abort_0a1d
	);

	Balsa___spaHarvard__V5T spa (
  activate_0r, activate_0a,
  d__Access_0r0d, d__Access_0r1d, d__Access_0r2d, d__Access_0r3d, d__Access_0a,
  i__Access_0r0d, i__Access_0r1d, i__Access_0r2d, i__Access_0r3d, i__Access_0a,
  d__Mode_0r0d, d__Mode_0r1d, d__Mode_0a,
  i__Mode_0r0d, i__Mode_0r1d, i__Mode_0a,
  d__Di_0r, d__Di_0a0d, d__Di_0a1d, d__Di_0a2d, d__Di_0a3d,
  i__Di_0r, i__Di_0a0d, i__Di_0a1d, i__Di_0a2d, i__Di_0a3d,
  d__Do_0r0d, d__Do_0r1d, d__Do_0r2d, d__Do_0r3d, d__Do_0a,
  d__Abort_0r, d__Abort_0a0d, d__Abort_0a1d,
  i__Abort_0r, i__Abort_0a0d, i__Abort_0a1d,
  cpId_0r0d, cpId_0r1d, cpId_0r2d, cpId_0r3d, cpId_0a,
  cpInst_0r0d, cpInst_0r1d, cpInst_0r2d, cpInst_0r3d, cpInst_0a,
  cpCapableP_0r, cpCapableP_0a0d, cpCapableP_0a1d, cpCapableP_0a2d, cpCapableP_0a3d,
  cpId2_0r0d, cpId2_0r1d, cpId2_0r2d, cpId2_0r3d, cpId2_0a,
  cpExec_0r0d, cpExec_0r1d, cpExec_0r2d, cpExec_0r3d, cpExec_0a,
  cpRegisterSpa2cp_0r0d, cpRegisterSpa2cp_0r1d, cpRegisterSpa2cp_0r2d, cpRegisterSpa2cp_0r3d, cpRegisterSpa2cp_0a,
  cpRegisterCp2spa_0r, cpRegisterCp2spa_0a0d, cpRegisterCp2spa_0a1d, cpRegisterCp2spa_0a2d, cpRegisterCp2spa_0a3d,
  cpMemAccessLastP_0r, cpMemAccessLastP_0a0d, cpMemAccessLastP_0a1d,
  irq_0r0d, irq_0r1d, irq_0a,
  fiq_0r0d, fiq_0r1d, fiq_0a
	`ifdef balsa_initialise
	, initialise
	`endif
);

	always @(&(cpId_0r0d | cpId_0r1d | cpId_0r2d | cpId_0r3d)) begin
		#2
		cpId_0a = 1;
		wait (&(!cpId_0r0d & !cpId_0r1d & !cpId_0r2d & !cpId_0r3d))
		#2
		cpId_0a = 0;
	end

	always @(&(cpInst_0r0d | cpInst_0r1d | cpInst_0r2d | cpInst_0r3d)) begin
		#2
		cpInst_0a = 1;
		wait (&(!cpInst_0r0d & !cpInst_0r1d & !cpInst_0r2d & !cpInst_0r3d))
		#2
		cpInst_0a = 0;
	end

	always @(posedge cpCapableP_0r) begin
		#2
		cpCapableP_0a0d = 2'b11;
		cpCapableP_0a1d = 2'b00;
		cpCapableP_0a2d = 2'b00;
		cpCapableP_0a3d = 2'b00;
		wait (!cpCapableP_0r)
		#2
		cpCapableP_0a0d = 2'b00;
		cpCapableP_0a1d = 2'b00;
		cpCapableP_0a2d = 2'b00;
		cpCapableP_0a3d = 2'b00;
	end

	always @(posedge cpMemAccessLastP_0r) begin
		#2
		cpMemAccessLastP_0a1d = 1'b1;
		cpMemAccessLastP_0a0d = ~cpMemAccessLastP_0a1d;
		wait (!cpMemAccessLastP_0r)
		#2
		cpMemAccessLastP_0a1d = 1'b0;
		cpMemAccessLastP_0a0d = 1'b0;
	end

    initial begin

        `ifdef TUBEFILE
            outFile = $fopen(`TUBEFILE);
        `else
			outFile = $fopen("out.txt");
        `endif

		if (outFile == 0) begin
		    $display ("memorySpecial: Cannot open TUBE output file");
			$finish;
		end

        `ifdef DUMPFILE
            $dumpfile ( `DUMPFILE );
            $display("Using dumpfile: \"%s\"", `DUMPFILE );
        `else
			$dumpfile ("/tmp/test-spaHarvard.vcd");
            $display("Using dumpfile: \"%s\"", "/tmp/test-spaHarvard.vcd");
        `endif

		$dumpvars(1,test);

        `ifdef DUMPVARS
		    $dumpvars (`DUMPVARS,spa);
	    `endif

//		$sdf_annotate ("spaHarvard.sdf", spa);

		cpId_0a = 1'b0;
		cpInst_0a = 1'b0;
		cpCapableP_0a1d = 2'b00;
		cpCapableP_0a0d = 2'b00;
		cpCapableP_0a2d = 2'b00;
		cpCapableP_0a3d = 2'b00;

		cpId2_0a = 1'b0;
		cpExec_0a = 1'b0;
		cpRegisterSpa2cp_0a = 1'b0;
		cpRegisterCp2spa_0a1d = 16'h0;
		cpRegisterCp2spa_0a0d = 16'h0;
		cpRegisterCp2spa_0a2d = 16'h0;
		cpRegisterCp2spa_0a3d = 16'h0;
		cpMemAccessLastP_0a1d = 1'b0;
		cpMemAccessLastP_0a0d = 1'b0;

		fiq_0r0d = 1'b0;
		fiq_0r1d = 1'b0;
		irq_0r0d = 1'b0;
		irq_0r1d = 1'b0;

`ifdef balsa_switchable
		spacer   = 1'b0;
`endif
`ifdef balsa_initialise
		initialise   = 1'b0;
`endif
	    activate_0r = 0;
		$display ("test-spaHarvard: variables initialised.");
		
		#1000
		`ifdef balsa_initialise
			initialise   = 1;
		`endif
		#1000
		`ifdef balsa_initialise
			initialise   = 0;
		`endif
		#200
		activate_0r = 1;

		$display ("test-spaHarvard: activate <- 1.");
//		#1 $dumpoff;

	end
endmodule
`else

`include "memoryDualPort.v"
`include "dummyCoproIrqFiq.v"

module test();
  reg  activate_0r;
  wire activate_0a;

`ifdef SR
  wire d__Access_0r;
  wire d__Access_0a;
  wire [37:0] d__Access_0d;
  wire i__Access_0r;
  wire i__Access_0a;
  wire [37:0] i__Access_0d;
  wire d__Mode_0r;
  wire d__Mode_0a;
  wire d__Mode_0d;
  wire i__Mode_0r;
  wire i__Mode_0a;
  wire i__Mode_0d;

  wire d__Di_0r;
  wire d__Di_0a;
  wire [31:0] d__Di_0d;
  wire i__Di_0r;
  wire i__Di_0a;
  wire [31:0] i__Di_0d;

  wire d__Do_0r;
  wire d__Do_0a;
  wire [31:0] d__Do_0d;

  wire d__Abort_0r;
  wire d__Abort_0a;
  wire d__Abort_0d;
  wire i__Abort_0r;
  wire i__Abort_0a;
  wire i__Abort_0d;

  wire cpId_0r;
  wire cpId_0a;
  wire [3:0] cpId_0d;
  wire cpInst_0r;
  wire cpInst_0a;
  wire [32:0] cpInst_0d;
  wire cpCapableP_0r;
  wire cpCapableP_0a;
  wire [3:0] cpCapableP_0d;
  wire cpId2_0r;
  wire cpId2_0a;
  wire [3:0] cpId2_0d;
  wire cpExec_0r;
  wire cpExec_0a;
  wire [5:0] cpExec_0d;
  wire cpRegisterSpa2cp_0r;
  wire cpRegisterSpa2cp_0a;
  wire [31:0] cpRegisterSpa2cp_0d;
  wire cpRegisterCp2spa_0r;
  wire cpRegisterCp2spa_0a;
  wire [31:0] cpRegisterCp2spa_0d;
  wire cpMemAccessLastP_0r;
  wire cpMemAccessLastP_0a;
  wire cpMemAccessLastP_0d;

  wire irq_0r;
  wire irq_0a;
  wire irq_0d;
  wire fiq_0r;
  wire fiq_0a;
  wire fiq_0d;

`ifdef NRESET
   reg 	    nreset;
`endif

`ifdef RESET
   reg 	    reset;
`endif

`else
  wire [37:0] d__Access_0r0d;
  wire [37:0] d__Access_0r1d;
  wire d__Access_0a;
  wire [37:0] i__Access_0r0d;
  wire [37:0] i__Access_0r1d;
  wire i__Access_0a;
  wire d__Mode_0r0d;
  wire d__Mode_0r1d;
  wire d__Mode_0a;
  wire i__Mode_0r0d;
  wire i__Mode_0r1d;
  wire i__Mode_0a;

  wire [31:0] d__Di_0a0d;
  wire [31:0] d__Di_0a1d;
  wire d__Di_0r;
  wire [31:0] i__Di_0a0d;
  wire [31:0] i__Di_0a1d;
  wire i__Di_0r;

  wire [31:0] d__Do_0r0d;
  wire [31:0] d__Do_0r1d;
  wire d__Do_0a;

  wire d__Abort_0a0d;
  wire d__Abort_0a1d;
  wire d__Abort_0r;
  wire i__Abort_0a0d;
  wire i__Abort_0a1d;
  wire i__Abort_0r;

  wire [3:0] cpId_0r0d;
  wire [3:0] cpId_0r1d;
  wire cpId_0a;
  wire [32:0] cpInst_0r0d;
  wire [32:0] cpInst_0r1d;
  wire cpInst_0a;
  wire cpCapableP_0r;
  wire [3:0] cpCapableP_0a0d;
  wire [3:0] cpCapableP_0a1d;
  wire [3:0] cpId2_0r0d;
  wire [3:0] cpId2_0r1d;
  wire cpId2_0a;
  wire [5:0] cpExec_0r0d;
  wire [5:0] cpExec_0r1d;
  wire cpExec_0a;
  wire [31:0] cpRegisterSpa2cp_0r0d;
  wire [31:0] cpRegisterSpa2cp_0r1d;
  wire cpRegisterSpa2cp_0a;
  wire cpRegisterCp2spa_0r;
  wire [31:0] cpRegisterCp2spa_0a0d;
  wire [31:0] cpRegisterCp2spa_0a1d;
  wire cpMemAccessLastP_0r;
  wire cpMemAccessLastP_0a0d;
  wire cpMemAccessLastP_0a1d;

  wire irq_0r0d;
  wire irq_0r1d;
  wire irq_0a;
  wire fiq_0r0d;
  wire fiq_0r1d;
  wire fiq_0a;

`ifdef balsa_switchable
	reg spacer;
`endif

`ifdef NRESET
   reg 	    nreset;
`endif

`ifdef RESET
   reg 	    reset;
`endif

`endif

  integer outFile;

`ifdef SR
	memoryDualPort mem (
       i__Access_0r, i__Access_0a, i__Access_0d,
       i__Mode_0r, i__Mode_0a, i__Mode_0d, 
       i__Di_0r, i__Di_0a, i__Di_0d, 
       i__Abort_0r, i__Abort_0a, i__Abort_0d,
       d__Access_0r, d__Access_0a, d__Access_0d,
       d__Mode_0r, d__Mode_0a, d__Mode_0d, 
       d__Di_0r, d__Di_0a, d__Di_0d, 
       d__Do_0r, d__Do_0a, d__Do_0d, 
       d__Abort_0r, d__Abort_0a, d__Abort_0d
	);

	Balsa___spaHarvard__V5T spa (
       activate_0r, activate_0a,
       d__Access_0r, d__Access_0a, d__Access_0d,
       i__Access_0r, i__Access_0a, i__Access_0d,
       d__Mode_0r, d__Mode_0a, d__Mode_0d,
       i__Mode_0r, i__Mode_0a, i__Mode_0d,
       d__Di_0r, d__Di_0a, d__Di_0d, 
       i__Di_0r, i__Di_0a, i__Di_0d, 
       d__Do_0r, d__Do_0a, d__Do_0d, 
       d__Abort_0r, d__Abort_0a, d__Abort_0d, 
       i__Abort_0r, i__Abort_0a, i__Abort_0d, 
       cpId_0r, cpId_0a, cpId_0d, 
       cpInst_0r, cpInst_0a, cpInst_0d, 
       cpCapableP_0r, cpCapableP_0a, cpCapableP_0d,
       cpId2_0r, cpId2_0a, cpId2_0d, 
       cpExec_0r, cpExec_0a, cpExec_0d,
       cpRegisterSpa2cp_0r, cpRegisterSpa2cp_0a, cpRegisterSpa2cp_0d,
       cpRegisterCp2spa_0r, cpRegisterCp2spa_0a, cpRegisterCp2spa_0d,
       cpMemAccessLastP_0r, cpMemAccessLastP_0a, cpMemAccessLastP_0d,
       irq_0r, irq_0a, irq_0d, 
       fiq_0r, fiq_0a, fiq_0d

`ifdef NRESET
           , nreset
`endif

`ifdef RESET
           , reset
`endif

	);

       dummyCoproIrqFiq coproIrqFiq (
       cpId_0r, cpId_0a, cpId_0d, 
       cpInst_0r, cpInst_0a, cpInst_0d, 
       cpCapableP_0r, cpCapableP_0a, cpCapableP_0d,
       cpId2_0r, cpId2_0a, cpId2_0d, 
       cpExec_0r, cpExec_0a, cpExec_0d,
       cpRegisterSpa2cp_0r, cpRegisterSpa2cp_0a, cpRegisterSpa2cp_0d,
       cpRegisterCp2spa_0r, cpRegisterCp2spa_0a, cpRegisterCp2spa_0d,
       cpMemAccessLastP_0r, cpMemAccessLastP_0a, cpMemAccessLastP_0d,
       irq_0r, irq_0a, irq_0d, 
       fiq_0r, fiq_0a, fiq_0d
	);
`else
	memoryDualPort mem (
       i__Access_0r0d, i__Access_0r1d, i__Access_0a, 
       i__Mode_0r0d, i__Mode_0r1d, i__Mode_0a,
	   i__Di_0r, i__Di_0a0d, i__Di_0a1d, 
       i__Abort_0r, i__Abort_0a0d, i__Abort_0a1d, 
       d__Access_0r0d, d__Access_0r1d, d__Access_0a,
	   d__Mode_0r0d, d__Mode_0r1d, d__Mode_0a, 
       d__Di_0r, d__Di_0a0d, d__Di_0a1d, 
       d__Do_0r0d, d__Do_0r1d, d__Do_0a, 
       d__Abort_0r, d__Abort_0a0d, d__Abort_0a1d
	);

	Balsa___spaHarvard__V5T spa (
       activate_0r, activate_0a,
	   d__Access_0r0d, d__Access_0r1d, d__Access_0a, 
       i__Access_0r0d, i__Access_0r1d, i__Access_0a, 
       d__Mode_0r0d, d__Mode_0r1d, d__Mode_0a,
	   i__Mode_0r0d, i__Mode_0r1d, i__Mode_0a, 
       d__Di_0r, d__Di_0a0d, d__Di_0a1d, 
       i__Di_0r, i__Di_0a0d, i__Di_0a1d, 
       d__Do_0r0d, d__Do_0r1d, d__Do_0a, 
       d__Abort_0r, d__Abort_0a0d, d__Abort_0a1d, 
       i__Abort_0r, i__Abort_0a0d, i__Abort_0a1d, 
       cpId_0r0d, cpId_0r1d, cpId_0a, 
       cpInst_0r0d, cpInst_0r1d, cpInst_0a, 
       cpCapableP_0r, cpCapableP_0a0d, cpCapableP_0a1d,
	   cpId2_0r0d, cpId2_0r1d, cpId2_0a, 
       cpExec_0r0d, cpExec_0r1d, cpExec_0a,
	   cpRegisterSpa2cp_0r0d, cpRegisterSpa2cp_0r1d, cpRegisterSpa2cp_0a,
	   cpRegisterCp2spa_0r, cpRegisterCp2spa_0a0d, cpRegisterCp2spa_0a1d,
	   cpMemAccessLastP_0r, cpMemAccessLastP_0a0d, cpMemAccessLastP_0a1d,
	   irq_0r0d, irq_0r1d, irq_0a, 
       fiq_0r0d, fiq_0r1d, fiq_0a

`ifdef balsa_switchable
	   , spacer
`endif

`ifdef NRESET
           , nreset
`endif

`ifdef RESET
           , reset
`endif

	);

       dummyCoproIrqFiq coproIrqFiq (
       cpId_0r0d, cpId_0r1d, cpId_0a, 
       cpInst_0r0d, cpInst_0r1d, cpInst_0a, 
       cpCapableP_0r, cpCapableP_0a0d, cpCapableP_0a1d,
       cpId2_0r0d, cpId2_0r1d, cpId2_0a, 
       cpExec_0r0d, cpExec_0r1d, cpExec_0a,
       cpRegisterSpa2cp_0r0d, cpRegisterSpa2cp_0r1d, cpRegisterSpa2cp_0a,
       cpRegisterCp2spa_0r, cpRegisterCp2spa_0a0d, cpRegisterCp2spa_0a1d,
       cpMemAccessLastP_0r, cpMemAccessLastP_0a0d, cpMemAccessLastP_0a1d,
       irq_0r0d, irq_0r1d, irq_0a, 
       fiq_0r0d, fiq_0r1d, fiq_0a
	);

`endif
   
    initial begin

        `ifdef TUBEFILE
            outFile = $fopen(`TUBEFILE);
        `else
			outFile = $fopen("out.txt");
        `endif

		if (outFile == 0) begin
		    $display ("test-spaHarvard: Cannot open TUBE output file");
			$finish;
		end

        `ifdef DUMPFILE
            $dumpfile ( `DUMPFILE );
            $display("Using dumpfile: \"%s\"", `DUMPFILE );
        `else
			$dumpfile ("/tmp/test-spaHarvard.vcd");
            $display("Using dumpfile: \"%s\"", "/tmp/test-spa.vcd");
        `endif

		$dumpvars (1,test);

        `ifdef DUMPVARS
		    $dumpvars (`DUMPVARS,spa);
	    `endif

//		$sdf_annotate ("g3cardSpa.sdf", spa);

`ifdef balsa_switchable
		spacer   = 1'b0;
`endif

	        activate_0r = 0;
`ifdef NRESET
                nreset = 0;
`endif
`ifdef RESET
                reset = 1;
`endif
		$display ("test-spaHarvard: variables initialised.");


		#1000
		activate_0r = 1;
`ifdef NRESET
	        nreset = 1;
`endif
`ifdef RESET
	        reset = 0;
`endif
		$display ("test-spaHarvard: activate <- 1 @ t=%t", $time);
//		#1 $dumpoff;

		#100000000
		$display ("test-spaHarvard: activate still 1 @ t=%t", $time);
	end
endmodule
`endif
