/*
	eTeak synthesiser for the Balsa language
	Copyright (C) 2012-2016 The University of Manchester

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.

	Mahdi Jelodari <m.j.1989@ieee.org> 
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK
*/

`timescale 1ns/1ps

`define HAS_GO 1
`define HAS_DONE 0
`define HAS_INOUT 1
`define DUT teak_top

`ifdef RESET_PERIOD
`else
`define RESET_PERIOD 20
`endif

`ifdef HS_PERIOD
`else
`define HS_PERIOD 0.05 // Handshake Period
`endif

module top;
	reg reset;
	integer               data_file    ; // file handler
	integer               out_file    ; // file handler

	initial begin
		out_file = $fopen("out.dat", "w");
		if (out_file == 0) begin
			$display("out_file handle was NULL");
			$finish;
		end
	end


`ifdef HAS_GO
	reg go_r;
	wire go_a;

	initial begin
		go_r = 0;
		@(negedge reset);
		#`HS_PERIOD;
		$display ("go");
		go_r = 1;
		@(posedge go_a);
		#`HS_PERIOD;
		go_r = 0;
		@(negedge go_a);
		#`HS_PERIOD;
	end
`endif

`ifdef HAS_DONE
     
     wire done_r;
     reg done_a;
	always begin

		done_a = 0;
		@(posedge done_r);
		#`HS_PERIOD;
		$display ("done");
		done_a = 1;
		@(negedge done_r);
		#`HS_PERIOD;
		done_a = 0;
	end
`endif

//----------------------------------HAS INOUT-------------------------------------------------------------
// Has to be set based on the input/output signals of DUT
// This particular inpout setup is generated for examples/punf/encode.balsa

`ifdef HAS_INOUT

  reg [7:0] iout_0r0;  //input
  reg [7:0] iout_0r1;
  wire iout_0a;

  wire [7:0] iin_0r0;  //output
  wire [7:0] iin_0r1;
  reg iin_0a;

  reg [7:0] oout_0r0;  //input
  reg [7:0] oout_0r1;
  wire oout_0a;

  wire [7:0] oin_0r0;  //output
  wire [7:0] oin_0r1;
  reg oin_0a;


	always begin	
		//set input to zero
		iout_0r0 = 8'b0000_0000;
		iout_0r1 = 8'b0000_0000;
 	
		@(negedge reset);
		#`HS_PERIOD;
		$display ("iout");

		//set input to all one (or some random value)	
		iout_0r0 = 8'b0000_0000;
		iout_0r1 = 8'b1111_1111;

		@(posedge iout_0a);
		#`HS_PERIOD;

		//set input to zero (return to zero)
		iout_0r0 = 8'b0000_0000;
		iout_0r1 = 8'b0000_0000;
	end

	always begin
	
		//set input to zero
		oout_0r0 = 8'b0000_0000;
		oout_0r1 = 8'b0000_0000;
 	
		@(negedge reset);
		#`HS_PERIOD;
		$display ("oout");

		//set input to all one (or some random value)	
		oout_0r0 = 8'b0000_0000;
		oout_0r1 = 8'b1111_1111;

		@(posedge oout_0a);
		#`HS_PERIOD;

		//set input to zero (return to zero)
		oout_0r0 = 8'b0000_0000;
		oout_0r1 = 8'b0000_0000;
	end

	always begin
		iin_0a = 0;
		@(posedge &(iin_0r0 | iin_0r1));
		#`HS_PERIOD;
		$display ("iin");
		$fwrite(out_file,"out: %h\n",iin_0r1);
		iin_0a = 1;
		@(negedge &(iin_0r0 | iin_0r1));
		#`HS_PERIOD;
		iin_0a = 0;
	end

	always begin
		oin_0a = 0;
		@(posedge &(oin_0r0 | oin_0r1));
		#`HS_PERIOD;
		$display ("oin");
		$fwrite(out_file,"out: %h\n",res_r1, );
		oin_0a = 1;
		@(negedge &(oin_0r0 | oin_0r1));
		#`HS_PERIOD;
		oin_0a = 0;
	end
`endif

//--------------------------------------------------------------------------------------------------------------------
	initial begin
		reset = 1;
`ifdef DUMPFILE
		$dumpfile (`DUMPFILE);
		$dumpvars (0, top);
`endif
		$display ("reset");
		#`RESET_PERIOD;
		reset = 0;
	end

	`DUT DUT (
		`ifdef HAS_GO
		.go_0r(go_r), .go_0a(go_a),
		`endif
		`ifdef HAS_DONE
		.done_0r(done_r), .done_0a(done_a),
		`endif
          	`ifdef HAS_INOUT
 	  	.iout_0r0(iout_0r0), .iout_0r0(iout_0r1), .iout_0a(iout_0a),
	  	.iin_0r0(iin_0r0), .iin_0r1(iin_0r1), .iin_0a(iin_0a),
         	.oout_0r0(oout_0r0), .oout_0r1(oout_0r1), .oout_0a(oout_0a),
          	.oin_0r0(oin_0r0), .oin_0r1(oin_0r1), .oin_0a(oin_0a),
          	`endif
          	.reset(reset)
         );

endmodule
