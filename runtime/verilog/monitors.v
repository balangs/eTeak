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

/* Note this timescale so we can pick up events to 1ps resolution */
`timescale 1ns/1ps

`ifdef TKR_SITE_LENGTH
`else
	`define TKR_SITE_LENGTH (128)
`endif

`define TKR_SITE_DECL [0:(`TKR_SITE_LENGTH*8)-1]

`define TKR_HS_RESET (0)
`define TKR_HS_SPACER (1)
`define TKR_HS_r (2)
`define TKR_HS_R (3)
`define TKR_HS_RA (4)
`define TKR_HS_rA (5)
`define TKR_HS_A (6)

`define TKR_HS_STATE_DECL [2:0]

// `define TKR_MONITOR_VERBOSE

module tkr_monitor_ctrl;

	parameter verbose =
`ifdef TKR_MONITOR_VERBOSE
	1;
`else
	0;
`endif

	event signal_report;

	/* File name and handle for report */
	reg `TKR_SITE_DECL report_filename;
	integer report_file;

	/* Action to take after calling report, reset both to just continue */
	reg stop_after_report;
	reg finish_after_report;

	/* Spacer links should be reported as well as ones in other states.
		Set to 0 for shorted post-error reports */
	reg report_spacer_links;

	/* Continuously trace */
	reg trace;

	initial begin
		report_filename = "tkr.report";
		report_file = 0;
		stop_after_report = 1;
		finish_after_report = 0;
		report_spacer_links = 1;
		trace = 0; 
	end

	task open_report_file;
		begin
			if (verbose) $display ("Writing tkr report to file: %s", report_filename);
			report_file = $fopen (report_filename, "w");
		end
	endtask

	task checkpoint;
		begin
`ifdef TKR_MONITOR_VERBOSE
			$display ("Checkpointing #%t", $time);
`endif
			-> signal_report;
		end
	endtask

	task report;
		begin
			if (report_file == 0) open_report_file;
			checkpoint;
			#0;
			$fclose (report_file);
			$fflush;
			if (finish_after_report) $finish;
			if (stop_after_report) $stop;
		end
	endtask
endmodule

/* Note this timescale so we can pick up events to 1ps resolution */
`timescale 1ps/1ps

module tkr_ra_monitor (r, a);
	parameter `TKR_SITE_DECL site = "SITE";

	input r;
	input a;

	reg `TKR_HS_STATE_DECL state;
	reg `TKR_HS_STATE_DECL old_state;

	initial begin
		state = `TKR_HS_RESET;
		old_state = `TKR_HS_RESET;
		if (tkr_monitor_ctrl.verbose) $display ("%s RESET", site);
	end

	task report_state;
		begin
			$timeformat (-12, 0, "", 0);
			case (state)
			`TKR_HS_SPACER:
					if (tkr_monitor_ctrl.report_spacer_links)
						$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s SPACER", $time, site);
			`TKR_HS_R:
					$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s R", $time, site);
			`TKR_HS_RA:
					$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s RA", $time, site);
			`TKR_HS_A:
					$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s A", $time, site);
			endcase
		end
	endtask

	task trace;
		if (tkr_monitor_ctrl.trace && old_state != state) report_state;
	endtask

	always @tkr_monitor_ctrl.signal_report report_state;

	always @(r or a) begin
		case (state)
			`TKR_HS_RESET: begin
				// FIXME, check for Zs
				if (r === 0 && a === 0) begin
					if (tkr_monitor_ctrl.verbose) $display ("%s RESET -> SPACER", site);
					state = `TKR_HS_SPACER;
				end
			end
			default: begin
				// FIXME, check for Xs and Zs
			end
		endcase
		trace;
		old_state = state;
		case (state)
			`TKR_HS_SPACER: begin
				// Check a
				if (a) $display ("%t %s ERROR SPACER, early ack rising", $time, site);
				if (r) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s SPACER -> R", $time, site);
					state = `TKR_HS_R;
				end
			end
			`TKR_HS_R: begin
				// Check that R is still complete
				if (! r) $display ("%t %s ERROR R, early req falling", $time, site);
				if (a) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s R -> RA", $time, site);
					state = `TKR_HS_RA;
				end
			end
			`TKR_HS_RA: begin
				// Check a
				if (! a) $display ("%t %s ERROR RA, early ack falling", $time, site);
				if (! r) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s RA -> A", $time, site);
					state = `TKR_HS_A;
				end
			end
			`TKR_HS_A: begin
				// Check for r0=0 r1=0
				if (r) $fdisplay (tkr_monitor_ctrl.report_file, "%t %s ERROR A, req doesn't stay 0", $time, site);
				if (!a) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s A -> SPACER", $time, site);
					state = `TKR_HS_SPACER;
				end
			end
		endcase
		trace;
		old_state = state;
	end
endmodule

/* Note this timescale so we can pick up events to 1ps resolution */
`timescale 1ps/1ps

module tkr_dr_monitor (r0, r1, a);
	parameter `TKR_SITE_DECL site = "SITE";
	parameter w = 1;

	input [w-1:0] r0;
	input [w-1:0] r1;
	input a;

	reg `TKR_HS_STATE_DECL state;
	reg `TKR_HS_STATE_DECL old_state;
	reg [w-1:0] old_r0;
	reg [w-1:0] old_r1;

	initial begin
		state = `TKR_HS_RESET;
		old_state = `TKR_HS_RESET;
		old_r0 = 0;
		old_r1 = 0;
		if (tkr_monitor_ctrl.verbose) $display ("%s RESET", site);
	end

	task report_state;
		begin
			$timeformat (-12, 0, "", 0);
			case (state)
			`TKR_HS_SPACER:
				if (tkr_monitor_ctrl.report_spacer_links)
					$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s SPACER", $time, site);
			`TKR_HS_r:
				$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s r", $time, site);
			`TKR_HS_R:
				$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s R %d", $time, site, r1);
			`TKR_HS_RA:
				$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s RA", $time, site);
			`TKR_HS_rA:
				$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s rA", $time, site);
			`TKR_HS_A:
				$fdisplay (tkr_monitor_ctrl.report_file, "#%t %s A", $time, site);
			endcase
		end
	endtask

	task trace;
		if (tkr_monitor_ctrl.trace && old_state != state) report_state;
	endtask

	always @tkr_monitor_ctrl.signal_report report_state;

	always @(r0 or r1 or a) begin
		case (state)
			`TKR_HS_RESET: begin
				// FIXME, check for Zs
				if (r0 === 0 && r1 === 0 && a === 0) begin
					if (tkr_monitor_ctrl.verbose) $display ("%s RESET -> SPACER", site);
					state = `TKR_HS_SPACER;
				end
			end
			default: begin
				// Check for conflicting 1s on 0 and 1 wires
				if ((r0 & r1) != 0) $display ("%t %s ERROR, bad data, r0=%x r1=%x r1&r0=%x",
					$time, site, r0, r1, r0 & r1);
				// FIXME, check for Xs and Zs
			end
		endcase
		trace;
		old_state = state;
		case (state)
			`TKR_HS_SPACER: begin
				// Check a
				if (a) $display ("%t %s ERROR SPACER, early ack rising", $time, site);
				if (& (r0 | r1)) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s SPACER -> R", $time, site);
					state = `TKR_HS_R;
				end
				else if (| (r0 | r1)) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s SPACER -> r", $time, site);
					state = `TKR_HS_r;
				end
			end
			`TKR_HS_r: begin
				// Check a
				if (a) $display ("%t %s ERROR r, early ack rising", $time, site);
				// Check for falling transitions on already risen Rs
				if (old_r0 > r0) $display ("%t %s ERROR r, falling r0 bits, r0=%x->%x ", $time, site, old_r0, r0);
				if (old_r1 > r1) $display ("%t %s ERROR r, falling r1 bits, r1=%x->%x ", $time, site, old_r1, r1);
				if (& (r0 | r1)) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s r -> R", $time, site);
					state = `TKR_HS_R;
				end
			end
			`TKR_HS_R: begin
				// Check that R is still complete
				if (! (& (r0 | r1))) $display ("%t %s ERROR R, data becomes invalid, r0=%x r1=%x r1|r0=%x",
					$time, site, r0, r1, r0 | r1);
				if (a) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s R -> RA", $time, site);
					state = `TKR_HS_RA;
				end
			end
			`TKR_HS_RA: begin
				// Check a
				if (! a) $display ("%t %s ERROR RA, early ack falling", $time, site);
				if (r0 == 0 && r1 == 0) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s RA -> A", $time, site);
					state = `TKR_HS_A;
				end
				else if (! (& (r0 | r1)))
				begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s RA -> rA", $time, site);
					state = `TKR_HS_rA;
				end
			end
			`TKR_HS_rA: begin
				// Check a
				if (! a) $display ("%t %s ERROR rA, early ack falling", $time, site);
				// Check for rising transitions on already fallen Rs
				if (r0 > old_r0) $display ("%t %s ERROR rA, rising r0 bits, r0=%x->%x ", $time, site, old_r0, r0);
				if (r1 > old_r1) $display ("%t %s ERROR rA, rising r1 bits, r1=%x->%x ", $time, site, old_r1, r1);
				if (r0 == 0 && r1 == 0) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s rA -> A", $time, site);
					state = `TKR_HS_A;
				end
			end
			`TKR_HS_A: begin
				// Check for r0=0 r1=0
				if (r0 !== 0 || r1 !== 0) $display ("%t %s ERROR A, data doesn't stay 0, r0=%x r1=%x",
					$time, site, r0, r1);
				if (!a) begin
					if (tkr_monitor_ctrl.verbose) $display ("%t %s A -> SPACER", $time, site);
					state = `TKR_HS_SPACER;
				end
			end
		endcase
		trace;
		old_r0 = r0;
		old_r1 = r1;
		old_state = state;
	end
endmodule
