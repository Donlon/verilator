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
/// \brief Verilated tracing in FST format for SystemC header
///
/// User wrapper code should use this header when creating FST SystemC
/// traces.
///
/// This class is not threadsafe, as the SystemC kernel is not threadsafe.
///
//=============================================================================

#ifndef VERILATOR_VERILATED_FST_SC_H_
#define VERILATOR_VERILATED_FST_SC_H_

#include "verilatedos.h"

#include "verilated_fst_c.h"
#include "verilated_sc_trace.h"

//=============================================================================
// VerilatedFstSc
///
/// Class representing a Verilator-friendly FST trace format registered
/// with the SystemC simulation kernel, just like a SystemC-documented
/// trace format.

class VerilatedFstSc final : VerilatedScTraceBase, public VerilatedFstC {
    // CONSTRUCTORS
    VL_UNCOPYABLE(VerilatedFstSc);

public:
    /// Construct a SC trace object, and register with the SystemC kernel
    VerilatedFstSc() {
        // We want to avoid a depreciated warning, but still be back compatible.
        // Turning off the message just for this still results in an
        // annoying "to turn off" message.
        const sc_core::sc_time t1sec{1, sc_core::SC_SEC};
        if (t1sec.to_default_time_units() != 0) {
            const sc_core::sc_time tunits{1.0 / t1sec.to_default_time_units(), sc_core::SC_SEC};
            spTrace()->set_time_unit(tunits.to_string());
        }
        spTrace()->set_time_resolution(sc_core::sc_get_time_resolution().to_string());
        enableDeltaCycles(false);
    }
    /// Destruct, flush, and close the dump
    ~VerilatedFstSc() override { close(); }

    // Override VerilatedFstC. Must be called after starting simulation.
    void open(const char* filename) override VL_MT_SAFE {
        if (VL_UNLIKELY(!sc_core::sc_get_curr_simcontext()->elaboration_done())) {
            Verilated::scTraceBeforeElaborationError();
        }
        VerilatedFstC::open(filename);
    }

    // METHODS - for SC kernel
    // Called from SystemC kernel
    void cycle() override { VerilatedFstC::dump(sc_core::sc_time_stamp().to_double()); }
};

#endif  // Guard
