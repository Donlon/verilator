// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2017 by Johan Bjork.
// SPDX-License-Identifier: CC0-1.0

// bug1593

interface simple_bus #(PARAMETER = 0);
   parameter [6:0] dummy = 22;
endinterface

interface simple_bus2 #(PARAMETER = 0);
   parameter [6:0] dummy = 22;
   if (PARAMETER != 7) $error("Wrong parameter");
endinterface

module t ();
   simple_bus sb_intf();
   simple_bus2 #(.PARAMETER($bits(sb_intf.dummy))) simple();
endmodule
