 `timescale 1ns/1ps

`define balsa_simulate true
`define balsa_init_time 400
`define balsa_switchable true

`ifdef TECHFILE
    `include `TECHFILE
`else
    `include "technology.amust018.v"
`endif

`ifdef NLSTFILE
    `include `NLSTFILE
`else
    `include "multiplier.v"
`endif

`define DataDelay 0.1
`define bmax 31
`define obmax 63
`define all_ones 32'hFFFFFFFF
`define all_zeroes 32'h0
`define all_5 32'h55555555
`define all_A 32'hFFFFFFFF
`define OpA  32'h00000005
`define OpB  32'h00000002
`define AccL 32'h00000001
`define AccH 32'h00000000
//`define OpA 32'h29B13FA3
//`define OpB 32'h00113942
//`define AccL 32'h8F4F4CFA
//`define AccH 32'h406570DD
//`define AccL 32'h0
//`define AccH 32'h0
//`define CSADDER 0
//`define TESTPATTERN 0
//`define SR 0

`define MUL 0   //multiply (32-bit result)
`define MLA 1   //multiply-accumulate (32-bit result)
`define MUND2 2 //undefined code
`define MUND3 3 //undefined code
`define UMULL 4 //unsigned multiply long
`define UMLAL 5 //unsigned multiply-accumulate long
`define SMULL 6 //signed multiply long
`define SMLAL 7 //signed multiply-accumulate long

`define TOTAL 50

module test();

  reg  activate_0r;
  wire activate_0ar;

  wire bypass_0a;
  reg  bypass_0r0d;
  reg  bypass_0r1d;

  wire bypassH_0a;
  reg  bypassH_0r0d;
  reg  bypassH_0r1d;

  wire mType_0a;
  reg [2:0] mType_0r0d;
  reg [2:0] mType_0r1d;

  wire a_0a;
  reg [`bmax:0] a_0r0d;
  reg [`bmax:0] a_0r1d;

  wire b_0a;
  reg [`bmax:0] b_0r0d;
  reg [`bmax:0] b_0r1d;

  wire c_0a;
  reg [`bmax:0] c_0r0d;
  reg [`bmax:0] c_0r1d;

  wire [`bmax:0] mpH_0r0d;
  wire [`bmax:0] mpH_0r1d;
  reg mpH_0a;

  wire [`bmax:0] mpL_0r0d;
  wire [`bmax:0] mpL_0r1d;
  reg mpL_0a;

  wire mZ_0r0d;
  wire mZ_0r1d;
  reg mZ_0a;

  wire mN_0r0d;
  wire mN_0r1d;
  reg mN_0a;


  reg bypass_0r;
  reg bypass_0d;

  reg bypassH_0r;
  reg bypassH_0d;

  reg mType_0r;
  reg [`bmax:0] mType_0d;

  reg a_0r;
  reg [`bmax:0] a_0d;

  reg b_0r;
  reg [`bmax:0] b_0d;

  reg c_0r;
  reg [`bmax:0] c_0d;

  reg mpH_0r;
  wire [`bmax:0] mpH_0d;

  reg mpL_0r;
  wire [`bmax:0] mpL_0d;

  reg mZ_0r;
  wire mZ_0d;

  reg mN_0r;
  wire mN_0d;

  reg initialise;

Balsa_nmult mult (
  activate_0r, activate_0a,
  bypass_0r0d, bypass_0r1d, bypass_0a,
  bypassH_0r0d, bypassH_0r1d, bypassH_0a,
  mType_0r0d, mType_0r1d, mType_0a,
  a_0r0d, a_0r1d, a_0a,
  b_0r0d, b_0r1d, b_0a,
  c_0r0d, c_0r1d, c_0a,
  mpH_0r0d, mpH_0r1d, mpH_0a,
  mpL_0r0d, mpL_0r1d, mpL_0a,
  mZ_0r0d, mZ_0r1d, mZ_0a,
  mN_0r0d, mN_0r1d, mN_0a
`ifdef RESET
  ,initialise
`endif
);

  integer outFile;
  reg[`bmax:0] da,db,dc,ph,pl,sda,sdb,sdc;
  reg[2:0] dmType,tmpType;
  reg[63:0] result64;
  reg dira,dirb,dirc,carry,init;
  time prevtime,inittime;
  integer times;
  reg[7:0] ustatus,sstatus;
  reg[`obmax:0] pt,pr,spt;
  integer sa,sb,sc,up,sp,count;
  real sresult;

  // dual-rail wrapper for input signals
  // requests
  always @(posedge bypass_0r) begin
    bypass_0r1d = bypass_0d;
    bypass_0r0d = ~bypass_0d;
  end

  always @(posedge bypassH_0r) begin
    bypassH_0r1d = bypassH_0d;
    bypassH_0r0d = ~bypassH_0d;
  end

  always @(posedge mType_0r) begin
    mType_0r1d = mType_0d;
    mType_0r0d = ~mType_0d;
  end

  always @(posedge a_0r) begin
    a_0r1d = a_0d;
    a_0r0d = ~a_0d;
  end

  always @(posedge b_0r) begin
    b_0r1d = b_0d;
    b_0r0d = ~b_0d;
  end

  always @(posedge c_0r) begin
    c_0r1d = c_0d;
    c_0r0d = ~c_0d;
  end

  // dual-rail wrapper for output signals
  always @(posedge &(mpH_0r0d | mpH_0r1d))   mpH_0r = 1'b1;
  always @(posedge &(~mpH_0r0d & ~mpH_0r1d)) mpH_0r = 1'b0;
  assign mpH_0d = mpH_0r1d;

  always @(posedge &(mpL_0r0d | mpL_0r1d))   mpL_0r = 1'b1;
  always @(posedge &(~mpL_0r0d & ~mpL_0r1d)) mpL_0r = 1'b0;
  assign mpL_0d = mpL_0r1d;

  always @(posedge &(mZ_0r0d | mZ_0r1d))   mZ_0r = 1'b1;
  always @(posedge &(~mZ_0r0d & ~mZ_0r1d)) mZ_0r = 1'b0;
  assign mZ_0d = mZ_0r1d;

  always @(posedge &(mN_0r0d | mN_0r1d))   mN_0r = 1'b1;
  always @(posedge &(~mN_0r0d & ~mN_0r1d)) mN_0r = 1'b0;
  assign mN_0d = mN_0r1d;

  // return to zero phases @ inputs
  //acks
  always @(posedge bypass_0a) begin
    #`DataDelay
    bypass_0r1d = 0;
    bypass_0r0d = 0;
    bypass_0r = 0;
    wait(~bypass_0a)
    #`DataDelay
    bypass_0r = 1;
  end

  always @(posedge bypassH_0a) begin
    #`DataDelay
    bypassH_0r1d = 0;
    bypassH_0r0d = 0;
    bypassH_0r = 0;
    wait(~bypassH_0a)
    #`DataDelay
    bypassH_0r = 1;
  end

  always @(posedge mType_0a) begin
    #`DataDelay
    mType_0r1d = 0;
    mType_0r0d = 0;
    mType_0r = 0;
    wait(~mType_0a)
    #`DataDelay
    mType_0r = 1;
 end

  always @(posedge a_0a) begin
    #`DataDelay
    a_0r1d = 0;
    a_0r0d = 0;
    a_0r = 0;
    wait(~a_0a)
    #`DataDelay
    a_0r = 1;
 end

  always @(posedge b_0a) begin
    #`DataDelay
    b_0r1d = 0;
    b_0r0d = 0;
    b_0r = 0;
    wait(~b_0a)
    #`DataDelay
    b_0r = 1;
  end

  always @(posedge c_0a) begin
    #`DataDelay
    c_0r1d = 0;
    c_0r0d = 0;
    c_0r = 0;
    wait(~c_0a)
    #`DataDelay
    c_0r = 1;
  end
  
  // return to zero phases @ outputs
  always @(negedge mpH_0r) begin
    #`DataDelay
    mpH_0a = 0;
  end

  always @(negedge mpL_0r) begin
    #`DataDelay
    mpL_0a = 0;
  end

  always @(negedge mZ_0r) begin
    #`DataDelay
    mZ_0a = 0;
  end

  always @(negedge mN_0r) begin
    #`DataDelay
    mN_0a = 0;
  end

  // initialise all input signal to zero:
  initial begin
    bypass_0r1d = 0;
    bypass_0r0d = 0;
    bypassH_0r1d = 0;
    bypassH_0r0d = 0;
    mType_0r1d = 0;
    mType_0r0d = 0;
    a_0r1d = 0;
    a_0r0d = 0;
    b_0r1d = 0;
    b_0r0d = 0;
    c_0r1d = 0;
    c_0r0d = 0;
    mpH_0a = 0;
    mpL_0a = 0;
    mZ_0a  = 0;
    mN_0a  = 0;
    bypass_0r = 0;
    bypassH_0r = 0;
    mType_0r = 0;
    a_0r = 0;
    b_0r = 0;
    c_0r = 0;
    mpH_0a = 0;
    mpL_0a = 0;
    mZ_0a  = 0;
    mN_0a  = 0;
    activate_0r = 0;
    initialise = 1;
  end
  
  // simulation i/o & output formatting setup  
  initial begin
    $timeformat(-9, 4, "", 10);
    `ifdef TUBEFILE
        outFile = $fopen(`TUBEFILE);
    `else
    	outFile = $fopen("out.txt");
    `endif
    if (outFile == 0) begin
	    $display ("test-multiplier: Cannot open TUBE output file");
	    $finish;
	end
    `ifdef DUMPFILE
        $dumpfile ( `DUMPFILE );
        $display("Using dumpfile: \"%s\"", `DUMPFILE );
    `else
    	$dumpfile ("/tmp/test-multiplier.vcd");
        $display("Using dumpfile: \"%s\"", "/tmp/test-multiplier.vcd");
    `endif
    $dumpvars (1,test);

    `ifdef DUMPVARS
	   $dumpvars (`DUMPVARS,mult);
    `endif
  end
  
  // simulation initialise     
  initial begin
    init = 1;
    count = 0;
    bypass_0d = 0;
    bypassH_0d = 1;
    mType_0d = `SMLAL;
    a_0d = `OpA;
    b_0d = `OpB;
    c_0d = `AccL;
    times = 0;
    $display ("test-multiplier: inputs initialised <- 1 @ t=%t", $time);
    $display ("\ntest-multiplier: NOTE this testbench only tests SMLAL\n");
    #1000
    activate_0r = 1;
    initialise = 0;
    bypass_0r = 1;
    bypassH_0r = 1;
    mType_0r = 1;
    a_0r = 1;
    b_0r = 1;
    c_0r = 1;
    prevtime = $time;
    inittime = $time;
    $display ("test-multiplier: activate <- 1 @ t=%t", $time);
    #1000000000
    $display ("test-multiplier: still activate = 1 @ t=%t", $time);
    $finish;
  end

  // activate ack
`ifdef RESET
  initial @(posedge (activate_0a)) begin
    #`DataDelay
    activate_0r = 0;
  end
`endif 
    // read result
    always @(posedge (mpH_0r & mpL_0r & mZ_0r & mN_0r)) begin
        #`DataDelay
        pt = {mpH_0d,mpL_0d} + {`AccH, 32'h00000000};

        sdc = c_0d;
        sc = c_0d;

        pr = (a_0d * b_0d + c_0d +`AccH*(`all_ones + 1));

        sda = a_0d;
        sdb = b_0d;
        sa = a_0d;
        sb = b_0d;
 
        if (c_0d[31])
            sdc= (~sdc) + 1;

        if (a_0d[31]) 
            sda= (~sda) + 1;

        if (b_0d[31]) 
            sdb= (~sdb) + 1;
//        $display("sdb = %d",sdb);

        spt = sda*sdb+{`AccH,`AccL} + sdc;

        if (a_0d[31] != b_0d[31]) begin //result is negative
            if (sc[31])  begin //acc value is negative
                spt = spt + sdc;
               // $display("P & C NEGATIVE : %X * %X + %X = %X",sda,sdb,sdc,spt);
            end
            else begin
                spt = spt - sdc;
                //$display("P NEGATIVE : %X * %X + %X = %X",sda,sdb,sdc,spt);
            end
            if(~spt[63]) begin
                spt = (~spt) + 1;   //translate back
                //$display("BACK TO : %X * %X + %X = %X",sda,sdb,sdc,spt);
            end
        end
        else begin
            if (sc[31])
                spt = spt - sdc;
            else
                spt = spt + sdc;
        end
        ustatus = 45; //"-"
        sstatus = 45; //"-"
        if (pt === spt)
            sstatus = 80; //"P"
        else
            sstatus = 70;//"F"
        
        $display ("< %4.0f > %X * %X + %X = %X [SIGN= %X ( %s )][UNSIG= %X ( %s )]<%t>",times, a_0d, b_0d, sc, pt, spt, sstatus, pr, ustatus, $time - prevtime);
        prevtime = $time;
        mpH_0a = 1;
        mpL_0a = 1;
        mZ_0a = 1;
        mN_0a = 1;
        times = times + 1;
        if (times == `TOTAL) begin
            $display("Total time to complete %4.0f operations: %t ns\n", `TOTAL, $time-inittime);
            $finish;
        end
    end
 endmodule
