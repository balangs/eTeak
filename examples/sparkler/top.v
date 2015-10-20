`timescale 1ns/1ps

`define delay 0.05

module memory (a_r0, a_r1, a_a, access_r0, access_r1, access_a,
	readd_r0, readd_r1, readd_a,
	writed_r0, writed_r1, writed_a, reset);

	input [31:0] a_r0, a_r1;
	output reg a_a;
	input [2:0] access_r0, access_r1;
	output reg access_a;
	output reg [31:0] readd_r0, readd_r1;
	input readd_a;
	input [31:0] writed_r0, writed_r1;
	output reg writed_a;
	input reset;

	reg rNw;
	reg [1:0] kind;
	reg [31:0] a;

	`define MEM_SIZE 1024

	reg [31:0] mem [0:`MEM_SIZE - 1];

	reg [0:7] kindStr [0:3];
	reg [0:7] rNwStr [0:1];

	`define instruction 0
	`define byte 1
	`define halfword 2
	`define word 3

	`define write 0
	`define read 1

	// initial #20000000 $stop;

	integer i;

	initial begin
		kindStr[0] = "I";
		kindStr[1] = "B";
		kindStr[2] = "H";
		kindStr[3] = "W";
		rNwStr[0] = "W";
		rNwStr[1] = "R";
		for (i = 0; i < `MEM_SIZE; i = i + 1)
			mem[i] = 32'hDEADBEEF;
		mem[32'h00000000 >> 2] = 32'h03000000;
		mem[32'h00000004 >> 2] = 32'hC4486044;
		mem[32'h00000008 >> 2] = 32'h86106044;
		mem[32'h0000000C >> 2] = 32'h80A0A000;
		mem[32'h00000010 >> 2] = 32'h0280000A;
		mem[32'h00000014 >> 2] = 32'hC4086044;
		mem[32'h00000018 >> 2] = 32'h09000000;
		mem[32'h0000001C >> 2] = 32'h8600E001;
		mem[32'h00000020 >> 2] = 32'hC2012040;
		mem[32'h00000024 >> 2] = 32'hC4284000;
		mem[32'h00000028 >> 2] = 32'hC448C000;
		mem[32'h0000002C >> 2] = 32'h80A0A000;
		mem[32'h00000030 >> 2] = 32'h12BFFFFB;
		mem[32'h00000034 >> 2] = 32'hC408C000;
		mem[32'h00000038 >> 2] = 32'hC0282000;
		mem[32'h0000003C >> 2] = 32'h01000000;
		mem[32'h00000040 >> 2] = 32'h0000FFFF;
		mem[32'h00000044 >> 2] = 32'h48656C6C;
		mem[32'h00000048 >> 2] = 32'h6F2C2077;
		mem[32'h0000004C >> 2] = 32'h6F726C64;
		mem[32'h00000050 >> 2] = 32'h0A9F4B00;
		mem[32'h00000054 >> 2] = 32'h00000000;
		mem[32'h00000058 >> 2] = 32'h00000000;
	end

	always @(posedge writed_a & writed_r0 == 0 & writed_r1 == 0) begin
		#`delay;
		writed_a = 0;
	end

	always @(posedge access_a & access_r0 == 0 & access_r1 == 0) begin
		#`delay;
		access_a = 0;
	end

	always @(posedge a_a & a_r0 == 0 & a_r1 == 0) begin
		#`delay;
		a_a = 0;
	end

	always @(posedge readd_a) begin
		#`delay;
		readd_r0 = 0;
		readd_r1 = 0;
		@(negedge readd_a);
	end

	reg access_arrived;
	reg a_arrived;
	reg writed_arrived;

	always @(posedge &(access_r0 | access_r1)) access_arrived = 1;
	always @(posedge &(a_r0 | a_r1)) a_arrived = 1;
	always @(posedge &(writed_r0 | writed_r1)) writed_arrived = 1;

	initial begin
		a_a = 0;
		access_a = 0;
		readd_r0 = 0;
		readd_r1 = 0;
		writed_a = 0;
		access_arrived = 0;
		a_arrived = 0;
		@(negedge reset);
	end

	always begin
		wait (access_arrived & a_arrived);
		#`delay;
		rNw = access_r1[0];
		kind = access_r1[2:1];
		a = a_r1;
		access_arrived = 0;
		a_arrived = 0;
		access_a = 1;
		a_a = 1;

		// $display ("rNw: %X, kind: %X, addr: %X", rNw, kind, a);

		if (a < `MEM_SIZE) begin
			if (rNw == `write) begin
				wait (writed_arrived);
				mem[a >> 2] = writed_r1;
				writed_arrived = 0;
				writed_a = 1;
				$display ("MEM W%s %X: %X", kindStr[kind], a, writed_r1);
				if (a == 0 & kind == `byte) begin
					$display ("END");
					$finish;
				end
			end else begin
				readd_r1 = mem[a >> 2];
				case (kind)
						`halfword:
						readd_r1 = (readd_r1 >> (16 * (1 - a[0]))) & 32'h0000FFFF;
						`byte:
						readd_r1 = (readd_r1 >> (8 * (3 - a[1:0]))) & 32'h000000FF;
				endcase
				readd_r0 = ~readd_r1;
				$display ("MEM R%s %X: %X", kindStr[kind], a, readd_r1);
			end
		end else begin
			if (a == 32'h0000_FFFF & rNw == `write & kind == `byte) begin
				wait (writed_arrived);
				$display ("MEM CW %X: %X '%s'", a, writed_r1, writed_r1[7:0]);
				writed_arrived = 0;
				writed_a = 1;
			end else begin
				$display ("Address out of range: %X", a);
				$finish;
			end
		end
	end
endmodule

module top;
	reg go_0r;
	wire go_0a;
	wire [31:0] a_0r0;
	wire [31:0] a_0r1;
	wire a_0a;
	wire [2:0] access_0r0;
	wire [2:0] access_0r1;
	wire access_0a;
	wire [31:0] di_0r0;
	wire [31:0] di_0r1;
	wire di_0a;
	wire [31:0] do_0r0;
	wire [31:0] do_0r1;
	wire do_0a;
	reg reset;

	initial begin
		go_0r = 0;
		@(negedge reset);
	end

	initial begin
		reset = 1;
		#20;
		reset = 0;
		#20;
	end

	teak_Sparkler2 DUT (// go_0r, go_0a,
		a_0r0, a_0r1, a_0a,
		access_0r0, access_0r1, access_0a,
		di_0r0, di_0r1, di_0a,
		do_0r0, do_0r1, do_0a,
		reset);
	memory MEM (a_0r0, a_0r1, a_0a,
		access_0r0, access_0r1, access_0a,
		di_0r0, di_0r1, di_0a,
		do_0r0, do_0r1, do_0a,
		reset);
endmodule
