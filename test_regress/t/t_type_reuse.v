// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2023 by Anthony Donlon.
// SPDX-License-Identifier: CC0-1.0

package mypkg;
    typedef int cls_typedef1_t;
    typedef int cls_typedef2_t;
    typedef int cls_typedef3_t;

    function static cls_typedef1_t test(cls_typedef1_t cls_typedef3_t);
        automatic cls_typedef1_t cls_typedef2_t = 9;
        return cls_typedef2_t * cls_typedef3_t;
    endfunction : test

    function static int test0(int arg);
        automatic int val = 9;
        return val * arg;
    endfunction : test0
endpackage

class myclass #(
    parameter PARAM = 7
);
    typedef int cls_typedef1_t;
    typedef int cls_typedef2_t;
    typedef int cls_typedef3_t;

    static function cls_typedef1_t test(cls_typedef1_t cls_typedef3_t);
        cls_typedef1_t cls_typedef2_t = 9;
        return PARAM * cls_typedef2_t * cls_typedef3_t;
    endfunction : test

endclass : myclass

class myclass2 #(
    parameter PARAM = 7
);
    typedef myclass#(PARAM) cls_typedef1_t;
endclass : myclass2


module t();
    typedef logic [7:0] typedef_t;

    myclass obj = new;
    myclass #(10) obj2 = new;

    if (mypkg::test0(111) != 9*11) $error("mypkg::test0(11) is %0d", mypkg::test0(11)); // VCS failed
`ifndef MODEL_TECH
    if (myclass#()::test0(11) != 7*9*11) $error("");
    if (myclass#(13)::test0(11) != 13*9*11) $error("");
    //if (myclass2#(5)::cls_typedef1_t::test0(17) != 5*9*17) $error("");

    // if ($typename(obj) != $typename(myclass)) $error("");
    //if ($typename(obj2) != $typename(myclass #(10))) $error(""); // xcelium failed
`endif

    if (1) begin : begin1
        typedef_t myclass = 8'd10; // reuse 'myclass'
        if ($typename(myclass) != "logic[7:0]") $error("");
        if ($typename(myclass[1:0]) != "logic[1:0]") $error("");
    end

    if (1) begin : begin2
        int typedef_t = 10; // reuse 'typedef_t'
        if ($typename(typedef_t) != "int") $error("");
    end

    if (1) begin : begin3
`ifndef MODEL_TECH
`ifndef VCS
        parameter typedef_t = 100; //questa/VCS failed
        if (typedef_t != 100) $error("");
`endif
`endif
    end
endmodule
