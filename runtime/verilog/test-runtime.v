
`define TKR_OBJ_DECL [63:0]
`define TKR_STR_DECL [(128*8)-1:0]

module test;
	reg `TKR_OBJ_DECL str1;
	reg `TKR_OBJ_DECL str2;
	reg `TKR_OBJ_DECL str3;
	reg `TKR_OBJ_DECL acc;

	reg `TKR_STR_DECL str;

	initial begin
		str1 = tkr_global.new_string ("A", 1);
		str2 = tkr_global.new_string ("Bb", 1);
		str3 = tkr_global.new_string ("Ccc", 1);
		acc = tkr_global.new_string (tkr_global.string_append (tkr_global.get_string (str1),
			tkr_global.get_string (str2)), 1);

		str = tkr_global.get_string (acc);
		// str = tkr_global.string_left_justify (str);
		$display ("acc = %x, %d, %s", acc, tkr_global.string_length (str), str);

		acc = tkr_global.new_string (tkr_global.string_append (tkr_global.get_string (acc),
			tkr_global.get_string (str3)), 1);

		str = tkr_global.get_string (acc);
		// str = tkr_global.string_left_justify (str);
		$display ("acc = %x, %d, %s", acc, tkr_global.string_length (str), str);

		str = tkr_global.get_string (str1);
		str = tkr_global.string_left_justify (str);
		$display ("str1 = %x, %d, %s", str1, tkr_global.string_length (str), str);

		str = tkr_global.get_string (str2);
		// str = tkr_global.string_left_justify (str);
		$display ("str2 = %x, %d, %s", str2, tkr_global.string_length (str), str);

		str = tkr_global.get_string (str3);
		// str = tkr_global.string_left_justify (str);
		$display ("str3 = %x, %d, %s", str3, tkr_global.string_length (str), str);

		str = tkr_global.get_string (acc);
		// str = tkr_global.string_left_justify (str);
		$display ("acc = %x, %d, %s", acc, tkr_global.string_length (str), str);
	end
endmodule
