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

`ifdef TKR_STR_LENGTH
`else
	`define TKR_STR_LENGTH (128)
`endif

`ifdef TKR_STR_COUNT
`else
	`define TKR_STR_COUNT (100)
`endif

`define TKR_REF_COUNT_DECL [7:0]
`define TKR_STR_DECL [(`TKR_STR_LENGTH*8)-1:0]
`define TKR_OBJ_DECL [63:0]

`define TKR_MAGIC_MASK (64'hFFFF_0000_0000_0000)
`define TKR_STR_MAGIC (64'h5453_0000_0000_0000)

`define TKR_PERMANENT (8'hFF)

module tkr_global;
	reg `TKR_STR_DECL strings [0:`TKR_STR_COUNT-1];
	reg `TKR_REF_COUNT_DECL string_ref_count [0:`TKR_STR_COUNT-1];

	integer i;

	reg string_print_left_justify;
	initial string_print_left_justify = 1;

	reg `TKR_STR_DECL space_string;
	initial begin
		space_string = {`TKR_STR_LENGTH{" "}};
	end

	reg debug;
	initial debug = 0;

	initial begin
		for (i = 0; i < `TKR_STR_COUNT; i = i + 1)
			string_ref_count[i] = 0;
	end

	function [31:0] check_string_magic;
		input `TKR_OBJ_DECL obj_num;

		begin
			if ((`TKR_MAGIC_MASK & obj_num) != `TKR_STR_MAGIC)
				$display ("!!! bad string magic!: %x", obj_num);
			check_string_magic = (~ `TKR_MAGIC_MASK) & obj_num;
		end
	endfunction

	function `TKR_REF_COUNT_DECL ref_string;
		input `TKR_OBJ_DECL obj;

		integer string_num;
		begin
			string_num = check_string_magic (obj);

			if (debug) $display ("*** refing string %d `%s' (%0d)", string_num, strings[string_num],
				string_ref_count[string_num]);
			if (string_ref_count[string_num] != `TKR_PERMANENT)
				string_ref_count[string_num] = string_ref_count[string_num] + 1;
			ref_string = string_ref_count[string_num];
		end
	endfunction

	function `TKR_REF_COUNT_DECL unref_string;
		input `TKR_OBJ_DECL obj;

		integer string_num;
		begin
			string_num = check_string_magic (obj);

			if (string_ref_count[string_num] != `TKR_PERMANENT)
			begin
				if (debug) $display ("*** unrefing string %d `%s' (%0d)", string_num, strings[string_num],
					string_ref_count[string_num]);
				string_ref_count[string_num] = string_ref_count[string_num] - 1;
				if (string_ref_count[string_num] == 0) begin
					if (debug) $display ("*** deallocating string %d `%s'", string_num, strings[string_num]);
					strings[string_num] = "Unreferenced string FIXME"; // 'bX;
				end
			end
			unref_string = string_ref_count[string_num];
		end
	endfunction

	function `TKR_OBJ_DECL new_string;
		input `TKR_STR_DECL new_value;
		input `TKR_REF_COUNT_DECL new_ref_count;

		integer string_num;
		begin
			string_num = 0;
			while (string_num < `TKR_STR_COUNT && string_ref_count[string_num] != 0)
				string_num = string_num + 1;

			if (string_num == `TKR_STR_COUNT) begin
				$display ("!!! Too many strings (%0d)", string_num);
				for (string_num = 0; string_num < `TKR_STR_COUNT; string_num = string_num + 1) begin
					$display ("STR[%0d] = '%s'", string_num, strings[string_num]);
				end
				$finish;
			end else begin
				if (debug) $display ("***   allocating string %d `%s'", string_num, new_value);
				strings[string_num] = new_value;
				string_ref_count[string_num] = new_ref_count;
			end
			new_string = `TKR_STR_MAGIC | string_num;
		end
	endfunction

	function `TKR_STR_DECL get_string;
		input `TKR_OBJ_DECL obj;

		integer string_num;
		begin
			string_num = check_string_magic (obj);
			get_string = strings[string_num];
		end
	endfunction

	function [31:0] string_length;
		input `TKR_STR_DECL str;

		integer length;

		begin
			length = 0;
			while (((str >> (8 * length)) & 8'hFF) != 0)
				length = length + 1;

			string_length = length;
		end
	endfunction

	function `TKR_STR_DECL string_left_justify;
		input `TKR_STR_DECL str;

		integer pad;

		if (tkr_global.string_print_left_justify) begin
			pad = 8 * (`TKR_STR_LENGTH - tkr_global.string_length (str));
			string_left_justify = str << pad | tkr_global.space_string & ((1 << pad) - 1);
		end else begin
			string_left_justify = str;
		end
	endfunction
	
	function `TKR_STR_DECL string_append;
		input `TKR_STR_DECL l;
		input `TKR_STR_DECL r;

		string_append = r | (l << (8 * string_length (r)));
	endfunction
endmodule

module tkr_print (inpF, inpT);
	input `TKR_OBJ_DECL inpF;
	input `TKR_OBJ_DECL inpT;
	integer rc;

	reg `TKR_STR_DECL str;

	always @(posedge (& (inpF | inpT))) begin
		str = tkr_global.get_string (inpT);
		str = tkr_global.string_left_justify (str);
		$display ("%s", str);
		// rc = tkr_global.unref_string (inpT);
	end
endmodule

module tkr_string /* #(str) */ (go, outF, outT);
	parameter `TKR_STR_DECL str = "NOT SET";
	input go;
	output `TKR_OBJ_DECL outF;
	output `TKR_OBJ_DECL outT;
	reg `TKR_OBJ_DECL outF;
	reg `TKR_OBJ_DECL outT;

	reg `TKR_OBJ_DECL strChoice;

	reg has_initialised;
	initial has_initialised = 0;

	initial outF = 0;
	initial outT = 0;

	always @(go) begin
		if (! has_initialised) begin
			has_initialised = 1;
			strChoice = tkr_global.new_string (str, `TKR_PERMANENT);
		end
		if (go)
		begin
			outT = strChoice;
			outF = ~outT;
		end else begin
			outT = 0;
			outF = 0;
		end
	end
endmodule

module tkr_dr_complete /* #(width) */ (inpF, inpT, complete);
	parameter [31:0] width = 1;
	input [width-1:0] inpF;
	input [width-1:0] inpT;
	output complete;

	reg complete;

	always begin
		complete = 0;
		wait (&(inpT | inpF));
		complete = 1;
		wait (inpT == 0 & inpF == 0);
	end
endmodule

module tkr_const /* #(width, value) */ (go, outF, outT);
	parameter [31:0] width = 1;
	parameter [width-1:0] value = 0;
	input go;
	output [width-1:0] outF;
	output [width-1:0] outT;

	reg [width-1:0] outF;
	reg [width-1:0] outT;

	always begin
		outF = 0; outT = 0;
		wait (go);
		outT = value; outF = ~outT;
		wait (!go);
	end
endmodule

module tkr_to_string /* #(width) */ (inpF, inpT, outF, outT);
	parameter [31:0] width = 1;
	input [width-1:0] inpF;
	input [width-1:0] inpT;
	output `TKR_OBJ_DECL outF;
	output `TKR_OBJ_DECL outT;

	wire [5:0] radixF;
	wire [5:0] radixT;
	wire [7:0] underscoreSpacingF;
	wire [7:0] underscoreSpacingT;
	wire showLeadingZeroesF;
	wire showLeadingZeroesT;

	tkr_dr_complete #(width) I0 (inpF, inpT, inpComplete);
	tkr_const #(6, 10) I1 (inpComplete, radixF, radixT);
	tkr_const #(8, 0) I2 (inpComplete, underscoreSpacingF, underscoreSpacingT);
	tkr_const #(1, 0) I3 (inpComplete, showLeadingZeroesF, showLeadingZeroesT);
	tkr_number_to_string #(width) I4 (inpF, inpT,
		radixF, radixT,
		underscoreSpacingF, underscoreSpacingT,
		showLeadingZeroesF, showLeadingZeroesT,
		outF, outT);
endmodule

module tkr_number_to_string /* #(width) */ (inpF, inpT,
	radixF, radixT,
	underscoreSpacingF, underscoreSpacingT,
	showLeadingZeroesF, showLeadingZeroesT,
	outF, outT);
	parameter [31:0] width = 1;
	input [width-1:0] inpF;
	input [width-1:0] inpT;
	input [5:0] radixF;
	input [5:0] radixT;
	input [7:0] underscoreSpacingF;
	input [7:0] underscoreSpacingT;
	input showLeadingZeroesF;
	input showLeadingZeroesT;
	output `TKR_OBJ_DECL outF;
	output `TKR_OBJ_DECL outT;

	reg `TKR_OBJ_DECL outF;
	reg `TKR_OBJ_DECL outT;

	reg `TKR_STR_DECL str;
	reg [width-1:0] i;
	integer strPtr;

	integer radix;
	integer digit;
	integer showLeadingZeroes;
	integer underscoreSpacing;
	integer digitCount;
	reg binaryRadix;
	integer digitWidth;
	integer digitNo;

	initial outF = 0;
	initial outT = 0;

	always begin
		// @(posedge &(inpT | inpF));
		wait ((&(inpT | inpF)) &
			(&(underscoreSpacingF | underscoreSpacingT)) &
			(&(radixF | radixT)) &
			(showLeadingZeroesF | showLeadingZeroesT));
		i = inpT;
		radix = radixT;
		underscoreSpacing = underscoreSpacingT;
		showLeadingZeroes = showLeadingZeroesT;
		binaryRadix = 0; digitWidth = 0;
		case (radix)
			2: begin digitWidth = 1; binaryRadix = 1; end
			4: begin digitWidth = 2; binaryRadix = 1; end
			8: begin digitWidth = 3; binaryRadix = 1; end
			16: begin digitWidth = 4; binaryRadix = 1; end
			32: begin digitWidth = 5; binaryRadix = 1; end
		endcase
		if (! binaryRadix) showLeadingZeroes = 0;
		digitCount = (width + (digitWidth - 1)) / digitWidth;

		if (! showLeadingZeroes && i == 0) begin
			str = "0";
		end else begin
			str = 0;
			strPtr = 0;
			digitNo = 0;
			while (showLeadingZeroes ? digitNo < digitCount : i != 0)
			begin
				digit = i % radix;
				str = str | (digit + (digit <= 9 ? "0" : "A" - 10)) << strPtr;
				strPtr = strPtr + 8;
				i = i / radix;
				digitNo = digitNo + 1;
				if (underscoreSpacing != 0 && digitNo % underscoreSpacing == 0 && digitNo < digitCount)
				begin
					str = str | "_" << strPtr;
					strPtr = strPtr + 8;
				end
			end
		end

		outT = tkr_global.new_string (str, 1);
		outF = ~outT;
		// @(negedge |(inpT | inpF));
		wait (inpT == 0 & inpF == 0 &
			underscoreSpacingF == 0 & underscoreSpacingT == 0 &
			radixF == 0 & radixT == 0 &
			showLeadingZeroesF == 0 & showLeadingZeroesT == 0);
		outT = 0;
		outF = 0;
	end
endmodule

module tkr_string_append (inpLF, inpLT, inpRF, inpRT, outF, outT);
	input `TKR_OBJ_DECL inpLF;
	input `TKR_OBJ_DECL inpLT;
	input `TKR_OBJ_DECL inpRF;
	input `TKR_OBJ_DECL inpRT;
	output `TKR_OBJ_DECL outF;
	output `TKR_OBJ_DECL outT;

	reg `TKR_OBJ_DECL outF;
	reg `TKR_OBJ_DECL outT;

	reg `TKR_STR_DECL str;
	reg `TKR_OBJ_DECL l;
	reg `TKR_OBJ_DECL r;
	reg sink;

	initial outF = 0;
	initial outT = 0;

	always begin
		// @(posedge &(inpT | inpF));
		wait ((&(inpLT | inpLF)) & (&(inpRT | inpRF)));
		l = inpLT;
		r = inpRT;

		str = tkr_global.string_append (tkr_global.get_string (l), tkr_global.get_string (r));

		// sink = tkr_global.unref_string (l);
		// sink = tkr_global.unref_string (r);

		outT = tkr_global.new_string (str, 1);
		outF = ~outT;

		// @(negedge |(inpT | inpF));
		wait (inpLT == 0 & inpLF == 0 & inpRT == 0 & inpRF == 0);
		outT = 0;
		outF = 0;
	end
endmodule

module tkr_chr (inpF, inpT, outF, outT);
	input [7:0] inpF;
	input [7:0] inpT;
	output `TKR_OBJ_DECL outF;
	output `TKR_OBJ_DECL outT;

	reg `TKR_OBJ_DECL outF;
	reg `TKR_OBJ_DECL outT;

	reg `TKR_STR_DECL str;
	reg [7:0] i;
	reg sink;

	initial outF = 0;
	initial outT = 0;

	always begin
		// @(posedge &(inpT | inpF));
		wait ((&(inpT | inpF)));
		i = inpT;

		str = 0;
		str[7:0] = i;

		outT = tkr_global.new_string (str, 1);
		outF = ~outT;
		// @(negedge |(inpT | inpF));
		wait (inpT == 0 & inpF == 0);
		outT = 0;
		outF = 0;
	end
endmodule

module tkr_builtin_var_write_reref /* #(name, width, offset) */ (bitens, currentT, nextF, nextT);
	parameter `TKR_STR_DECL name = "NO NAME";
	parameter [31:0] width = 64;
	parameter [31:0] offset = 0;

	input [width-1:0] bitens;
	input [width-1:0] nextF;
	input [width-1:0] nextT;
	input [width-1:0] currentT;

	reg `TKR_OBJ_DECL current;
	reg `TKR_OBJ_DECL next;
	reg `TKR_OBJ_DECL magic;
	reg sink;

	always begin
		wait ((&(nextF[offset+63:offset] | nextT[offset+63:offset])) & (&(bitens[offset+63:offset])));
		next = nextT[offset+63:offset];
		current = currentT[offset+63:offset];

		if (tkr_global.debug) $display ("*** write reref %s[%0d:%0d] %x %x", name, offset+63, offset, current, next);

		/*
		magic = `TKR_MAGIC_MASK & next;
		if (magic == 0)
			$display ("!!! tkr_builtin_var_write_reref: not an object %x", next);
		else case (magic)
			`TKR_STR_MAGIC: sink = tkr_global.ref_string (next);
			default: $display ("!!! tkr_builtin_var_write_reref: bad magic %x", magic);
		endcase
		*/

		magic = `TKR_MAGIC_MASK & current;
		if (magic != 0) begin
			case (magic)
				`TKR_STR_MAGIC: sink = tkr_global.unref_string (current);
				default: $display ("!!! tkr_builtin_var_write_reref: bad magic %x", magic);
			endcase
		end

		wait (nextF[offset+63:offset] == 0 & nextT[offset+63:offset] == 0 & bitens[offset+63:offset] == 0);
	end
endmodule

module tkr_builtin_var_read_reref /* #(name, width, offset) */ (currentT, readGo);
	parameter `TKR_STR_DECL name = "NO NAME";
	parameter [31:0] width = 64;
	parameter [31:0] offset = 0;

	input [width-1:0] currentT;
	input readGo;

	reg `TKR_OBJ_DECL current;
	reg `TKR_OBJ_DECL next;
	reg `TKR_OBJ_DECL magic;
	reg sink;

	always begin
		wait (readGo);
		current = currentT[offset+63:offset];

		if (tkr_global.debug) $display ("*** read reref %s[%0d:%0d] %x", name, offset+63, offset, current);

		magic = `TKR_MAGIC_MASK & current;
		if (magic == 0)
			$display ("!!! tkr_builtin_var_reref: not an object %x", current);
		else case (magic)
			`TKR_STR_MAGIC: sink = tkr_global.ref_string (current);
			default: $display ("!!! tkr_builtin_var_reref: bad magic %x", magic);
		endcase

		wait (! readGo);
	end
endmodule

module tkr_stop (signal);
	input signal;

	always @(posedge signal) begin
		$display ("BalsaSimulationStop called");
		$stop;
	end
endmodule
