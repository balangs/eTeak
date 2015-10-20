`define MemAccessDelay 0.5
`define MemDataDelay 0.5

`ifdef SR
module memoryDualPort(
       i__Access_0r, i__Access_0a, i__Access_0d, 
       i__Mode_0r, i__Mode_0a, i__Mode_0d,
	   i__Di_0r, i__Di_0a, i__Di_0d, 
       i__Abort_0r, i__Abort_0a, i__Abort_0d, 
       d__Access_0r, d__Access_0a, d__Access_0d,
	   d__Mode_0r, d__Mode_0a, d__Mode_0d, 
       d__Di_0r, d__Di_0a, d__Di_0d, 
       d__Do_0r, d__Do_0a, d__Do_0d, 
       d__Abort_0r, d__Abort_0a, d__Abort_0d);

  input  i__Access_0r;
  output i__Access_0a;
  input  [37:0] i__Access_0d;

  input  i__Mode_0r;
  output i__Mode_0a;
  input  i__Mode_0d;

  input  i__Di_0r;
  output i__Di_0a;
  output [31:0] i__Di_0d;

  input  i__Abort_0r;
  output i__Abort_0a;
  output i__Abort_0d;

  input  d__Access_0r;
  output d__Access_0a;
  input  [37:0] d__Access_0d;

  input  d__Mode_0r;
  output d__Mode_0a;
  input  d__Mode_0d;

  input  d__Di_0r;
  output d__Di_0a;
  output [31:0] d__Di_0d;

  input  d__Do_0r;
  output d__Do_0a;
  input  [31:0] d__Do_0d;

  input  d__Abort_0r;
  output d__Abort_0a;
  output d__Abort_0d;

  reg i__Access_0a;
  reg i__Mode_0a;
  reg i__Di_0a;
  reg [31:0] i__Di_0d;
  reg i__Abort_0a;
  reg i__Abort_0d;
  reg d__Access_0a;
  reg d__Mode_0a;
  reg d__Di_0a;
  reg [31:0] d__Di_0d;
  reg d__Do_0a;
  reg d__Abort_0a;
  reg d__Abort_0d;

`else
module memoryDualPort(
       i__Access_0r0d, i__Access_0r1d, i__Access_0a, 
       i__Mode_0r0d, i__Mode_0r1d, i__Mode_0a,
	   i__Di_0r, i__Di_0a0d, i__Di_0a1d, 
       i__Abort_0r, i__Abort_0a0d, i__Abort_0a1d, 
       d__Access_0r0d, d__Access_0r1d, d__Access_0a,
	   d__Mode_0r0d, d__Mode_0r1d, d__Mode_0a, 
       d__Di_0r, d__Di_0a0d, d__Di_0a1d, 
       d__Do_0r0d, d__Do_0r1d, d__Do_0a, 
       d__Abort_0r, d__Abort_0a0d, d__Abort_0a1d);

  input  [37:0] i__Access_0r0d;
  input  [37:0] i__Access_0r1d;
  output i__Access_0a;

  input  i__Mode_0r0d;
  input  i__Mode_0r1d;
  output i__Mode_0a;

  input  i__Di_0r;
  output [31:0] i__Di_0a0d;
  output [31:0] i__Di_0a1d;

  input  i__Abort_0r;
  output i__Abort_0a0d;
  output i__Abort_0a1d;

  input  [37:0] d__Access_0r0d;
  input  [37:0] d__Access_0r1d;
  output d__Access_0a;

  input  d__Mode_0r0d;
  input  d__Mode_0r1d;
  output d__Mode_0a;

  input  d__Di_0r;
  output [31:0] d__Di_0a0d;
  output [31:0] d__Di_0a1d;

  input  [31:0] d__Do_0r0d;
  input  [31:0] d__Do_0r1d;
  output d__Do_0a;

  input  d__Abort_0r;
  output d__Abort_0a0d;
  output d__Abort_0a1d;

  reg i__Access_0a;
  reg i__Mode_0a;
  reg [31:0] i__Di_0a0d;
  reg [31:0] i__Di_0a1d;
  reg i__Abort_0a0d;
  reg i__Abort_0a1d;
  reg d__Access_0a;
  reg d__Mode_0a;
  reg [31:0] d__Di_0a0d;
  reg [31:0] d__Di_0a1d;
  reg d__Do_0a;
  reg d__Abort_0a0d;
  reg d__Abort_0a1d;

  reg  i__Access_0r;
  wire [37:0] i__Access_0d;
  reg  i__Mode_0r;
  wire i__Mode_0d;
  reg  i__Di_0a;
  reg  [31:0] i__Di_0d;
  reg  i__Abort_0a;
  reg  i__Abort_0d;

  reg  d__Access_0r;
  wire [37:0] d__Access_0d;
  reg  d__Mode_0r;
  wire d__Mode_0d;
  reg  d__Di_0a;
  reg  [31:0] d__Di_0d;
  reg  d__Do_0r;
  wire [31:0] d__Do_0d;
  reg  d__Abort_0a;
  reg  d__Abort_0d;

  // dual-rail wrapper around the single-rail core memory
  always @(posedge &(i__Access_0r0d | i__Access_0r1d))   i__Access_0r = 1'b1;
  always @(posedge &(~i__Access_0r0d & ~i__Access_0r1d)) i__Access_0r = 1'b0;
  assign i__Access_0d = i__Access_0r1d;

  always @(posedge i__Mode_0r0d | i__Mode_0r1d)   i__Mode_0r = 1'b1;
  always @(posedge ~i__Mode_0r0d & ~i__Mode_0r1d) i__Mode_0r = 1'b0;
  assign i__Mode_0d = i__Mode_0r1d;

  always @(posedge i__Di_0a) begin
    i__Di_0a1d = i__Di_0d;
    i__Di_0a0d = ~i__Di_0d;
  end
  always @(negedge i__Di_0a) begin
    i__Di_0a1d = 0;
    i__Di_0a0d = 0;
  end

  always @(posedge i__Abort_0a) begin
     i__Abort_0a1d = i__Abort_0d;
     i__Abort_0a0d = ~i__Abort_0d;
  end
  always @(negedge i__Abort_0a) begin
     i__Abort_0a1d = 0;
     i__Abort_0a0d = 0;
  end

  always @(posedge &(d__Access_0r0d | d__Access_0r1d))   d__Access_0r = 1'b1;
  always @(posedge &(~d__Access_0r0d & ~d__Access_0r1d)) d__Access_0r = 1'b0;
  assign d__Access_0d = d__Access_0r1d;

  always @(posedge d__Mode_0r0d | d__Mode_0r1d)   d__Mode_0r = 1'b1;
  always @(posedge ~d__Mode_0r0d & ~d__Mode_0r1d) d__Mode_0r = 1'b0;
  assign d__Mode_0d = d__Mode_0r1d;

  always @(posedge &(d__Do_0r0d | d__Do_0r1d))   d__Do_0r = 1'b1;
  always @(posedge &(~d__Do_0r0d & ~d__Do_0r1d)) d__Do_0r = 1'b0;
  assign d__Do_0d = d__Do_0r1d;

  always @(posedge d__Di_0a) begin
    d__Di_0a1d = d__Di_0d;
    d__Di_0a0d = ~d__Di_0d;
  end
  always @(negedge d__Di_0a) begin
    d__Di_0a1d = 0;
    d__Di_0a0d = 0;
  end

  always @(posedge d__Abort_0a) begin
     d__Abort_0a1d = d__Abort_0d;
     d__Abort_0a0d = ~d__Abort_0d;
  end
  always @(negedge d__Abort_0a) begin
     d__Abort_0a1d = 0;
     d__Abort_0a0d = 0;
  end

`endif

  reg [8:1]  d_m;
  reg [8:1]  d_s;
  integer    d_a;
  integer    d_aw;
  reg [31:0] d_w;
  integer    d_rNw;
  integer    d_size;

  reg    d_sd;
  reg    d_rd;
  reg    d_pd;
  reg	 d_sra;
  reg	 d_swa;

  reg [8:1]  i_m;
  reg [8:1]  i_s;
  integer    i_a;
  integer    i_aw;
  reg [31:0] i_w;
  integer    i_rNw;
  integer    i_size;

  reg    i_sd;
  reg	 i_sra;
  reg	 i_swa;

  integer dir;
  reg [31:0] mem[65535:0];

  initial begin
	$display ("memoryDualPort: Initialising memory.");
	for (dir = 0; dir < 65536; dir = dir + 1)
		mem[dir] = 0;
    `ifdef PROGFILE
        $readmemh(`PROGFILE, mem);
		$display("memoryDualPort: loading RAM from file: \"%s\"", `PROGFILE);
    `else
        $readmemh("image.raw", mem);
		$display("memoryDualPort: loading RAM from file: \"image.raw\"");
    `endif

    d__Access_0a = 0;
    d__Mode_0a = 0;
    d__Do_0a = 0;
    d__Di_0a = 0;
    d__Di_0d = 32'h0;
    d__Abort_0a = 0;
    d__Abort_0d = 0;

	d_sd = 0;
	d_rd = 0;
	d_pd = 0;
	d_sra = 0;
	d_swa = 0;

    i__Access_0a = 0;
    i__Mode_0a = 0;
    i__Di_0a = 0;
    i__Di_0d = 32'h0;
    i__Abort_0a = 0;
    i__Abort_0d = 0;

	i_sd = 0;
	i_sra = 0;
	i_swa = 0;

	$display ("memoryDualPort: Memory initialised.");
  end

  always @(posedge d__Access_0r & d__Mode_0r) begin
	#`MemAccessDelay
	d_rNw = d__Access_0d[0];
	d_a = d__Access_0d[32:1];
	d_aw = d_a >> 2;
	d_size = d__Access_0d[34:33];

	if (d__Mode_0d)
	    d_m = "p";
	else
	    d_m = "u";

	case (d_size)
		0: d_s = "b";
		1: d_s = "h";
		2: d_s = "w";
	endcase

	if (d_rNw)
	    if ((d_aw < 65535) && (d_aw >= 0))
		    d_sd = 1;  // read
		else
			d_sra = 1; // read abort
	else begin
		wait (d__Do_0r)
		d_w = d__Do_0d;
		if (d_a == 32'hfffffffe) begin
		    $display ("d_A 0x%h: w %s %s 0x%h     > Exit    (t=%t)", d_a, d_m, d_s, d_w, $time);
			$fclose(test.outFile);
			$display ("PASS");
			$finish;
	    end else if ((d_a == 32'hffffffff) || (d_a == 32'h0000c000) || (d_a == 32'h03000000) || (d_a == 32'h13000000) || (d_a == 32'he0000000) || (d_a == 32'h00003000))
			if (d_w[7:0] == 8'h4) begin
			    $display ("d_A 0x%h: w %s %s 0x%h     > Exit    (t=%t)", d_a, d_m, d_s, d_w, $time);
				$fclose(test.outFile);
				$display ("PASS");
				$finish;
			end else
				d_pd = 1;  // write to tube
		else if ((d_aw < 65536) && (d_aw >= 0))
		    d_rd = 1;      // write
		else
			d_swa = 1;     // write abort
	end

	d__Access_0a = 1;
	d__Mode_0a = 1;

  end

  always @(posedge d_sd) begin
    // read data from memory and send to processor
	d_sd = 0;
	case (d_size)
		2:
			d_w = mem[d_aw];
		1:
			if ((d_a & 32'h2) == 0)
			    d_w = (mem[d_aw] & 32'h0000ffff) | 32'haaaa0000;
			else
			    d_w = (mem[d_aw] & 32'hffff0000) | 32'h0000aaaa;
		0:
			case (d_a & 32'h3)
				0: d_w = (mem[d_aw] & 32'h000000ff) | 32'haaaaaa00;
				1: d_w = (mem[d_aw] & 32'h0000ff00) | 32'haaaa00aa;
				2: d_w = (mem[d_aw] & 32'h00ff0000) | 32'haa00aaaa;
				3: d_w = (mem[d_aw] & 32'hff000000) | 32'h00aaaaaa;
			endcase
	endcase
    fork
        begin
            wait(d__Abort_0r);
    	    #`MemDataDelay
            d__Abort_0d = 0;
    	    #`MemDataDelay
            d__Abort_0a = 1;
        end
        begin
            wait(d__Di_0r);
    	    #`MemDataDelay
	        d__Di_0d = d_w;
    	    #`MemDataDelay
	        d__Di_0a = 1;
        end
    join
    $display ("d_A 0x%h: r %s %s 0x%h", d_a, d_m, d_s, d_w);
  end

  always @(posedge d_rd) begin
    // write data to memory
	d_rd = 0;
    #0.1
	d_w = d__Do_0d;
	case (d_size)
		2:
			mem[d_aw] = d_w;
		1:
			if ((d_a & 32'h2) == 0)
			    mem[d_aw] = (d_w & 32'h0000ffff) | (mem[d_aw] & 32'hffff0000);
			else
			    mem[d_aw] = (d_w & 32'hffff0000) | (mem[d_aw] & 32'h0000ffff);
		0:
			case (d_a & 32'h3)
				0: mem[d_aw] = (d_w & 32'h000000ff) | (mem[d_aw] & 32'hffffff00);
				1: mem[d_aw] = (d_w & 32'h0000ff00) | (mem[d_aw] & 32'hffff00ff);
				2: mem[d_aw] = (d_w & 32'h00ff0000) | (mem[d_aw] & 32'hff00ffff);
				3: mem[d_aw] = (d_w & 32'hff000000) | (mem[d_aw] & 32'h00ffffff);
			endcase
	endcase
    wait(d__Abort_0r);
    #`MemDataDelay
    d__Abort_0d = 0;
	#0.4
    d__Abort_0a = 1;
    d__Do_0a = 1;
    $display ("d_A 0x%h: w %s %s 0x%h", d_a, d_m, d_s, d_w);
  end

  always @(posedge d_pd) begin
    // write data to tube
	d_pd = 0;
    #0.1
	d_w = d__Do_0d;
	$fwrite(test.outFile, "%s", d_w[7:0]);
	if (d_w[7:0] == 8'h00)
	    $display ("d_A 0x%h: w %s %s 0x%h     > <NULL>  (t=%t)", d_a, d_m, d_s, d_w, $time);
	else if (d_w[7:0] == 8'h05)
	    $display ("d_A 0x%h: w %s %s 0x%h     > <^E>    (t=%t)", d_a, d_m, d_s, d_w, $time);
	else if (d_w[7:0] == 8'h0a)
	    $display ("d_A 0x%h: w %s %s 0x%h     > <LF>    (t=%t)", d_a, d_m, d_s, d_w, $time);
	else if (d_w[7:0] == 8'h0d)
	    $display ("d_A 0x%h: w %s %s 0x%h     > <CR>    (t=%t)", d_a, d_m, d_s, d_w, $time);
	else
	    $display ("d_A 0x%h: w %s %s 0x%h     > %s       (t=%t)", d_a, d_m, d_s, d_w, d_w[7:0], $time);
	#`MemDataDelay
    d__Abort_0d = 0;
	#0.4
    d__Abort_0a = 1;
	d__Do_0a = 1;
  end

  always @(posedge d_sra) begin
    // send read abort
    d_sra = 0;
    fork
        begin
            wait(d__Abort_0r);
	        #`MemAccessDelay
	        d__Abort_0d = 1;
	        #`MemAccessDelay
	        d__Abort_0a = 1;
        end
        begin
            wait(d__Di_0r);
	        #`MemAccessDelay
    	    d__Di_0d = 32'hffffffff;
	        #`MemAccessDelay
            d__Di_0a = 1;
        end 
    join
    $display ("d_A 0x%h: r %s %s 0x--------     > Abort!  (t=%t)", d_a, d_m, d_s, $time);
  end

  always @(posedge d_swa) begin
    // send write abort
    d_swa = 0;
    wait(d__Abort_0r);
    #`MemDataDelay
    d__Abort_0d = 1;
    #0.4
    d__Abort_0a = 1;
    d__Do_0a = 1;
    $display ("d_A 0x%h: w %s %s 0x--------     > Abort!  (t=%t)", d_a, d_m, d_s, $time);
  end

  // return-to-zero phases
  always @(posedge ~d__Access_0r & ~d__Mode_0r) begin
	#`MemAccessDelay
	d__Access_0a = 0;
	d__Mode_0a = 0;
  end

  always @(negedge d__Do_0r) begin
	#`MemAccessDelay
	d__Do_0a = 0;
  end

  always @(negedge d__Di_0r) begin
    // data is not changed (broad protocol)
    #`MemDataDelay
    #`MemDataDelay
    d__Di_0a = 0;
  end

  always @(negedge d__Abort_0r) begin
    // data is not changed (broad protocol)
    #`MemDataDelay
    #`MemDataDelay
    d__Abort_0a = 0;
  end

  always @(posedge i__Access_0r & i__Mode_0r) begin
	#`MemAccessDelay
	i_rNw = i__Access_0d[0];
	i_a = i__Access_0d[32:1];
	i_aw = i_a >> 2;
	i_size = i__Access_0d[34:33];

	if (i__Mode_0d)
	    i_m = "p";
	else
	    i_m = "u";

	case (i_size)
		0: i_s = "b";
		1: i_s = "h";
		2: i_s = "w";
	endcase

	if (i_rNw)
	    if ((i_aw < 65535) && (i_aw >= 0))
		    i_sd = 1;  // read
		else
			i_sra = 1; // read abort
	else begin
		i_swa = 1;     // write abort
	end

	i__Access_0a = 1;
	i__Mode_0a = 1;
  end

  integer fetch_count;
  initial fetch_count = 0;

  always @(posedge i_sd) begin
    // read data from memory and send to processor
	i_sd = 0;
	case (i_size)
		2:
			i_w = mem[i_aw];
		1:
			if ((i_a & 32'h2) == 0)
			    i_w = (mem[i_aw] & 32'h0000ffff) | 32'haaaa0000;
			else
			    i_w = (mem[i_aw] & 32'hffff0000) | 32'h0000aaaa;
		0:
			case (i_a & 32'h3)
				0: i_w = (mem[i_aw] & 32'h000000ff) | 32'haaaaaa00;
				1: i_w = (mem[i_aw] & 32'h0000ff00) | 32'haaaa00aa;
				2: i_w = (mem[i_aw] & 32'h00ff0000) | 32'haa00aaaa;
				3: i_w = (mem[i_aw] & 32'hff000000) | 32'h00aaaaaa;
			endcase
	endcase
    fork
        begin
            wait(i__Abort_0r);
    	    #`MemDataDelay
            i__Abort_0d = 0;
    	    #`MemDataDelay
            i__Abort_0a = 1;
        end
        begin
            wait(i__Di_0r);
    	    #`MemDataDelay
	        i__Di_0d = i_w;
    	    #`MemDataDelay
	        i__Di_0a = 1;
        end
    join
    fetch_count = fetch_count + 1;
    $display ("i_A 0x%h: r %s %s 0x%h (%d)", i_a, i_m, i_s, i_w, fetch_count);
  end

  always @(posedge i_sra) begin
    // send read abort
    i_sra = 0;
    fork
        begin
            wait(i__Abort_0r);
	        #`MemAccessDelay
	        i__Abort_0d = 1;
	        #`MemAccessDelay
	        i__Abort_0a = 1;
        end
        begin
            wait(i__Di_0r);
	        #`MemAccessDelay
    	    i__Di_0d = 32'hffffffff;
	        #`MemAccessDelay
            i__Di_0a = 1;
        end 
    join
    $display ("i_A 0x%h: r %s %s 0x--------     > Abort!  (t=%t)", i_a, i_m, i_s, $time);
  end

  always @(posedge i_swa) begin
    // send write abort
    i_swa = 0;
    wait(i__Abort_0r);
    #`MemDataDelay
    i__Abort_0d = 1;
    #`MemDataDelay
    i__Abort_0a = 1;
    $display ("i_A 0x%h: w %s %s 0x--------     > Abort!  (t=%t)", i_a, i_m, i_s, $time);
  end

  // return-to-zero phases
  always @(posedge ~i__Access_0r & ~i__Mode_0r) begin
	#`MemAccessDelay
	i__Access_0a = 0;
	i__Mode_0a = 0;
  end

  always @(negedge i__Di_0r) begin
    // data is not changed (broad protocol)
    #`MemDataDelay
    #`MemDataDelay
    i__Di_0a = 0;
  end

  always @(negedge i__Abort_0r) begin
    // data is not changed (broad protocol)
    #`MemDataDelay
    #`MemDataDelay
    i__Abort_0a = 0;
  end
endmodule
