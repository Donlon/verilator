// -*- mode: C++; c-file-style: "cc-mode" -*-
//=============================================================================
//
// Copyright 2001-2023 by Wilson Snyder. This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// Lesser General Public License Version 3 or the Perl Artistic License
// Version 2.0.
// SPDX-License-Identifier: LGPL-3.0-only OR Artistic-2.0
//
//=============================================================================
///
/// \file
/// \brief Verilated tracing for SystemC implementation code
///
///
///
//=============================================================================

#ifndef VERILATOR_VERILATED_SC_TRACE_H_
#define VERILATOR_VERILATED_SC_TRACE_H_

#include "verilatedos.h"

#include "verilated_sc.h"

#if SYSTEMC_VERSION < 20140417  // SystemC's simulation phase callback introduced in 2.3.1
#define VERILATOR_SYSTEMC_NO_PHASE_CALLBACK
#endif

//=============================================================================
// VerilatedScTraceBase
///
/// Base class for VCD/FST trace format on SystemC

class VerilatedScTraceBase VL_NOT_FINAL : private sc_core::sc_object,
                                          private sc_core::sc_trace_file {
    bool m_enableDeltaCycles = false;
    bool m_traceFileAdded = false;
    static void stubReportHandler(const sc_core::sc_report&, const sc_core::sc_actions&){};

public:
    void enableDeltaCycles(bool flag = true) {
#ifndef VERILATOR_SYSTEMC_NO_PHASE_CALLBACK
        if (flag) {
            // Register delta cycle callback
            sc_object::register_simulation_phase_callback(sc_core::SC_END_OF_UPDATE);
        } else {
            sc_object::unregister_simulation_phase_callback(sc_core::SC_END_OF_UPDATE);
        }
#endif
        m_enableDeltaCycles = flag;
    }

protected:
    VerilatedScTraceBase()
        : sc_object(sc_core::sc_gen_unique_name("$$$$verilator_sc_trace$$$$"))
        , sc_trace_file() {
        registerTraceCallback();
    };
    ~VerilatedScTraceBase() override {
        // Phase callback is automatically unregistered in ~sc_object(), only trace file need to be
        // removed here.
        if (m_traceFileAdded) simcontext()->remove_trace_file(this);
    };
    void registerTraceCallback() {
#ifndef VERILATOR_SYSTEMC_NO_PHASE_CALLBACK
        using namespace sc_core;
        // Save old report handler before overriding it
        const auto oldHandler = sc_report_handler::get_handler();
        // Override the old handler to hide 'phase callbacks not enabled' message
        sc_report_handler::set_handler(&stubReportHandler);
        // Register regular (non-delta cycle) callback
        phase_cb_mask cb_mask = sc_object::register_simulation_phase_callback(SC_BEFORE_TIMESTEP);
        if (cb_mask == SC_UNITIALIZED) {
#endif
            // Phase callback not enabled, use trace file instead
            simcontext()->add_trace_file(this);
            m_traceFileAdded = true;
#ifndef VERILATOR_SYSTEMC_NO_PHASE_CALLBACK
        }
        // Restore the old handler
        sc_report_handler::set_handler(oldHandler);
#endif
    }

    // METHODS - for SC kernel
    // Called if using phase callback
    void simulation_phase_callback() final { cycle(); }
    // Called if using trace file
    void cycle(bool delta_cycle) final {
        if (!delta_cycle || m_enableDeltaCycles) cycle();
    }
    // METHODS - callbacks
    // Callback for subclasses
    virtual void cycle() = 0;

private:
    // METHODS - Fake outs for linker

    // LCOV_EXCL_START
#ifdef NC_SYSTEMC
    // Cadence Incisive has these as abstract functions so we must create them
    void set_time_unit(int exponent10_seconds) override {}  // deprecated
#endif
    void set_time_unit(double v, sc_core::sc_time_unit tu) override {}  // LCOV_EXCL_LINE

    //--------------------------------------------------
    // SystemC 2.1.v1

    void write_comment(const std::string&) override {}
    void trace(const unsigned int&, const std::string&, const char**) override {}

#define DECL_TRACE_METHOD_A(tp) \
    void trace(const tp& object, const std::string& name) override {}
#define DECL_TRACE_METHOD_B(tp) \
    void trace(const tp& object, const std::string& name, int width) override {}

    // clang-format off
    // Formatting matches that of sc_trace.h
#if SYSTEMC_VERSION >= 20171012  // SystemC >= 2.3.2
    DECL_TRACE_METHOD_A( sc_core::sc_event )
    DECL_TRACE_METHOD_A( sc_core::sc_time )
#endif

    DECL_TRACE_METHOD_A( bool )
    DECL_TRACE_METHOD_A( sc_dt::sc_bit )
    DECL_TRACE_METHOD_A( sc_dt::sc_logic )

    DECL_TRACE_METHOD_B( unsigned char )
    DECL_TRACE_METHOD_B( unsigned short )
    DECL_TRACE_METHOD_B( unsigned int )
    DECL_TRACE_METHOD_B( unsigned long )
    DECL_TRACE_METHOD_B( char )
    DECL_TRACE_METHOD_B( short )
    DECL_TRACE_METHOD_B( int )
    DECL_TRACE_METHOD_B( long )
    DECL_TRACE_METHOD_B( sc_dt::int64 )
    DECL_TRACE_METHOD_B( sc_dt::uint64 )

    DECL_TRACE_METHOD_A( float )
    DECL_TRACE_METHOD_A( double )
    DECL_TRACE_METHOD_A( sc_dt::sc_int_base )
    DECL_TRACE_METHOD_A( sc_dt::sc_uint_base )
    DECL_TRACE_METHOD_A( sc_dt::sc_signed )
    DECL_TRACE_METHOD_A( sc_dt::sc_unsigned )

    DECL_TRACE_METHOD_A( sc_dt::sc_fxval )
    DECL_TRACE_METHOD_A( sc_dt::sc_fxval_fast )
    DECL_TRACE_METHOD_A( sc_dt::sc_fxnum )
    DECL_TRACE_METHOD_A( sc_dt::sc_fxnum_fast )

    DECL_TRACE_METHOD_A( sc_dt::sc_bv_base )
    DECL_TRACE_METHOD_A( sc_dt::sc_lv_base )
    // LCOV_EXCL_STOP
    // clang-format on

#undef DECL_TRACE_METHOD_A
#undef DECL_TRACE_METHOD_B
};

#ifdef VERILATOR_SYSTEMC_NO_PHASE_CALLBACK
#undef VERILATOR_SYSTEMC_NO_PHASE_CALLBACK
#endif

#endif  // Guard
