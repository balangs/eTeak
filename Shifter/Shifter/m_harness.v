

`include "teak_inclusions.v"
`include "shifter.v"

module my_top;
	initial begin
		tkr_monitor_ctrl.report_filename = "shifter.report";
		tkr_monitor_ctrl.report_spacer_links = 1;
		tkr_monitor_ctrl.trace = 1;
		tkr_monitor_ctrl.open_report_file;
		tkr_monitor_ctrl.finish_after_report = 1;
		$dumpfile ("shifter.vcd");
		$dumpvars (2, top);

		#5000;
		tkr_monitor_ctrl.report;
	end
endmodule
