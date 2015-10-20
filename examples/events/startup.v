`timescale 1ns/1ps

module report;
	// defparam tkr_monitor_ctrl.verbose = 1;

	initial #1 begin
		tkr_monitor_ctrl.report_filename = "a.report";
		tkr_monitor_ctrl.report_spacer_links = 1;
		tkr_monitor_ctrl.trace = 1;
		tkr_monitor_ctrl.open_report_file;
	end

	// initial @(posedge top.reset) tkr_monitor_ctrl.checkpoint;
endmodule
