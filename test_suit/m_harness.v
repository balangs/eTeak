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

`define HAS_GO 1
`define HAS_DONE 0
`define HAS_INOUT 1
`define DUT teak_top

`include "eteak_inclusions.v"
`include "encode.v"

module my_top;
	initial begin
		tkr_monitor_ctrl.report_filename = "encode.report";
		tkr_monitor_ctrl.report_spacer_links = 1;
		tkr_monitor_ctrl.trace = 1;
		tkr_monitor_ctrl.open_report_file;
		tkr_monitor_ctrl.finish_after_report = 1;
		$dumpfile ("encode.vcd");
		$dumpvars (2, top);

		#5000;
		tkr_monitor_ctrl.report;
	end
endmodule
