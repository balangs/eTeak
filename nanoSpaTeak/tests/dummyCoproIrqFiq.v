// $Id: dummyCoproIrqFiq.v,v 1.1 2002/05/20 10:57:38 plana Exp $

`timescale 1ns/1ps

`ifdef SR
module dummyCoproIrqFiq(
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
  input  cpId_0r;
  output cpId_0a;
  input  [3:0] cpId_0d;
  input  cpInst_0r;
  output cpInst_0a;
  input  [32:0] cpInst_0d;
  input  cpCapableP_0r;
  output cpCapableP_0a;
  output [3:0] cpCapableP_0d;
  input  cpId2_0r;
  output cpId2_0a;
  input  [3:0] cpId2_0d;
  input  cpExec_0r;
  output cpExec_0a;
  input  [5:0] cpExec_0d;
  input  cpRegisterSpa2cp_0r;
  output cpRegisterSpa2cp_0a;
  input  [31:0] cpRegisterSpa2cp_0d;
  input  cpRegisterCp2spa_0r;
  output cpRegisterCp2spa_0a;
  output [31:0] cpRegisterCp2spa_0d;
  input  cpMemAccessLastP_0r;
  output cpMemAccessLastP_0a;
  output cpMemAccessLastP_0d;
  output irq_0r;
  input  irq_0a;
  output irq_0d;
  output fiq_0r;
  input  fiq_0a;
  output fiq_0d;

  wire cpId_0r;
  reg  cpId_0a;
  wire [3:0] cpId_0d;
  wire cpInst_0r;
  reg  cpInst_0a;
  wire [32:0] cpInst_0d;
  wire cpCapableP_0r;
  reg  cpCapableP_0a;
  reg  [3:0] cpCapableP_0d;
  wire cpId2_0r;
  reg  cpId2_0a;
  wire [3:0] cpId2_0d;
  wire cpExec_0r;
  reg  cpExec_0a;
  wire [5:0] cpExec_0d;
  wire cpRegisterSpa2cp_0r;
  reg  cpRegisterSpa2cp_0a;
  wire [31:0] cpRegisterSpa2cp_0d;
  wire cpRegisterCp2spa_0r;
  reg  cpRegisterCp2spa_0a;
  reg  [31:0] cpRegisterCp2spa_0d;
  wire cpMemAccessLastP_0r;
  reg  cpMemAccessLastP_0a;
  reg  cpMemAccessLastP_0d;

  reg  irq_0r;
  wire irq_0a;
  reg  irq_0d;
  reg  fiq_0r;
  wire fiq_0a;
  reg  fiq_0d;

	always @(posedge cpId_0r) begin
		#2
		cpId_0a = 1;
		wait (!cpId_0r)
		#2
		cpId_0a = 0;
	end

	always @(posedge cpInst_0r) begin
		#2
		cpInst_0a = 1;
		wait (!cpInst_0r)
		#2
		cpInst_0a = 0;
	end

	always @(posedge cpCapableP_0r) begin
		#2
		cpCapableP_0d = 4'b0000;
	        cpCapableP_0a = 1;
		wait (!cpCapableP_0r)
		#2
		cpCapableP_0a = 0;
	end

	always @(posedge cpMemAccessLastP_0r) begin
		#2
		cpMemAccessLastP_0d = 1'b1;
		cpMemAccessLastP_0a = 1;
		wait (!cpMemAccessLastP_0r)
		#2
		cpMemAccessLastP_0a = 0;
	end

    initial begin
		cpId_0a = 1'b0;
		cpInst_0a = 1'b0;
		cpCapableP_0d = 4'b0000;
		cpCapableP_0a = 0;

		cpId2_0a = 1'b0;
		cpExec_0a = 1'b0;
		cpRegisterSpa2cp_0a = 1'b0;
		cpRegisterCp2spa_0d = 32'h0;
		cpRegisterCp2spa_0a = 0;
		cpMemAccessLastP_0d = 1'b0;
		cpMemAccessLastP_0a = 0;

		fiq_0r = 1'b0;
		fiq_0d = 1'b0;
		irq_0r = 1'b0;
		irq_0d = 1'b0;
	end
endmodule
`else
module dummyCoproIrqFiq(
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
  input  [3:0] cpId_0r0d;
  input  [3:0] cpId_0r1d;
  output cpId_0a;
  input  [32:0] cpInst_0r0d;
  input  [32:0] cpInst_0r1d;
  output cpInst_0a;
  input  cpCapableP_0r;
  output [3:0] cpCapableP_0a0d;
  output [3:0] cpCapableP_0a1d;
  input  [3:0] cpId2_0r0d;
  input  [3:0] cpId2_0r1d;
  output cpId2_0a;
  input  [5:0] cpExec_0r0d;
  input  [5:0] cpExec_0r1d;
  output cpExec_0a;
  input  [31:0] cpRegisterSpa2cp_0r0d;
  input  [31:0] cpRegisterSpa2cp_0r1d;
  output cpRegisterSpa2cp_0a;
  input  cpRegisterCp2spa_0r;
  output [31:0] cpRegisterCp2spa_0a0d;
  output [31:0] cpRegisterCp2spa_0a1d;
  input  cpMemAccessLastP_0r;
  output cpMemAccessLastP_0a0d;
  output cpMemAccessLastP_0a1d;
  output irq_0r0d;
  output irq_0r1d;
  input  irq_0a;
  output fiq_0r0d;
  output fiq_0r1d;
  input  fiq_0a;

  wire [3:0] cpId_0r0d;
  wire [3:0] cpId_0r1d;
  reg  cpId_0a;
  wire [32:0] cpInst_0r0d;
  wire [32:0] cpInst_0r1d;
  reg  cpInst_0a;
  wire cpCapableP_0r;
  reg  [3:0] cpCapableP_0a0d;
  reg  [3:0] cpCapableP_0a1d;
  wire [3:0] cpId2_0r0d;
  wire [3:0] cpId2_0r1d;
  reg  cpId2_0a;
  wire [5:0] cpExec_0r0d;
  wire [5:0] cpExec_0r1d;
  reg  cpExec_0a;
  wire [31:0] cpRegisterSpa2cp_0r0d;
  wire [31:0] cpRegisterSpa2cp_0r1d;
  reg  cpRegisterSpa2cp_0a;
  wire cpRegisterCp2spa_0r;
  reg  [31:0] cpRegisterCp2spa_0a0d;
  reg  [31:0] cpRegisterCp2spa_0a1d;
  wire cpMemAccessLastP_0r;
  reg  cpMemAccessLastP_0a0d;
  reg  cpMemAccessLastP_0a1d;

  reg  irq_0r0d;
  reg  irq_0r1d;
  wire irq_0a;
  reg  fiq_0r0d;
  reg  fiq_0r1d;
  wire fiq_0a;


	//AB always @(&(cpId_0r0d | cpId_0r1d)) begin
	always @(posedge &(cpId_0r0d | cpId_0r1d)) begin
		#2
		cpId_0a = 1;
		wait (&(!cpId_0r0d & !cpId_0r1d))
		#2
		cpId_0a = 0;
	end

	//AB @(posedge &(cpInst_0r0d | cpInst_0r1d)) begin
	always @(posedge &(cpInst_0r0d | cpInst_0r1d)) begin
		#2
		cpInst_0a = 1;
		wait (&(!cpInst_0r0d & !cpInst_0r1d))
		#2
		cpInst_0a = 0;
	end

	always @(posedge cpCapableP_0r) begin
		#2
		cpCapableP_0a1d = 4'b0000;
		cpCapableP_0a0d = ~cpCapableP_0a1d;
		wait (!cpCapableP_0r)
		#2
		cpCapableP_0a1d = 4'b0000;
		cpCapableP_0a0d = 4'b0000;
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
		cpId_0a = 1'b0;
		cpInst_0a = 1'b0;
		cpCapableP_0a1d = 4'b0000;
		cpCapableP_0a0d = 4'b0000;

		cpId2_0a = 1'b0;
		cpExec_0a = 1'b0;
		cpRegisterSpa2cp_0a = 1'b0;
		cpRegisterCp2spa_0a1d = 32'h0;
		cpRegisterCp2spa_0a0d = 32'h0;
		cpMemAccessLastP_0a1d = 1'b0;
		cpMemAccessLastP_0a0d = 1'b0;

		fiq_0r0d = 1'b0;
		fiq_0r1d = 1'b0;
		irq_0r0d = 1'b0;
		irq_0r1d = 1'b0;
	end
endmodule
`endif
