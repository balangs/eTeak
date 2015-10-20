/*
	Teak synthesiser for the Balsa language
	Copyright (C) 2007-2010 The University of Manchester

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

	Andrew Bardsley <bardsley@cs.man.ac.uk> (and others, see AUTHORS)
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK
*/

`timescale 1ns/1ps

`ifdef RESET_PERIOD
`else
`define RESET_PERIOD 10
`endif

`ifdef HS_PERIOD
`else
`define HS_PERIOD 10
`endif

module top;
	reg reset;
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

	initial begin
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
		.reset(reset)
	);
endmodule
