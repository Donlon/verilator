// -*- mode: C++; c-file-style: "cc-mode" -*-
//*************************************************************************
// DESCRIPTION: Verilator: Basic data structure to keep names unique
//
// Code available from: https://verilator.org
//
//*************************************************************************
//
// Copyright 2003-2023 by Wilson Snyder. This program is free software; you
// can redistribute it and/or modify it under the terms of either the GNU
// Lesser General Public License Version 3 or the Perl Artistic License
// Version 2.0.
// SPDX-License-Identifier: LGPL-3.0-only OR Artistic-2.0
//
//*************************************************************************
//
//*************************************************************************

#ifndef VERILATOR_V3CONSTSTRING_H_
#define VERILATOR_V3CONSTSTRING_H_

#include "config_build.h"
#include "verilatedos.h"

#include <memory>
#include <string>
#include <unordered_map>

class VConstString;

class VConstStringData final {
    const string m_str;
    const size_t m_hash;

protected:
    VConstStringData(const string& str) noexcept
        : m_str(str)
        , m_hash(std::hash<string>{}(str)) {}
    friend class VConstString;
};

class VConstString final {
    // VConstStringData* m_data;
    std::shared_ptr<VConstStringData> m_data;
    // const size_t m_hash;

public:
    // CONSTRUCTORS
    VConstString()
        : VConstString(emptyStr()){};
    ~VConstString() = default;
    VConstString(const VConstString& cstr) = default;  // Copy
    // VConstString(VConstString&& cstr) = default;  // Move
    VConstString(const string& str)  // new
        : m_data{new VConstStringData(str)} {}
    VConstString(const char* str)  // new
        : m_data{new VConstStringData(str)} {}

    // METHODS
    uint64_t hash() const { return m_data->m_hash; }
    const string& str() const { return m_data->m_str; }
    const char* c_str() const { return m_data->m_str.c_str(); }
    bool empty() const { return m_data->m_str.empty(); }

    operator string() const { return m_data->m_str; }
    const string* operator->() { return &m_data->m_str; }
    const char& operator[](size_t pos) const { return m_data->m_str[pos]; }

    // STATIC METHODS
    static const VConstString& emptyStr() noexcept {
        static VConstString s_emptyStr{""};  // FIXME:
        return s_emptyStr;
    }
};

namespace std {
template <>
struct hash<VConstString> {
    size_t operator()(const VConstString& str) const { return str.hash(); }
};
}  //namespace std

inline string operator+(const VConstString& lhs, const char* rhs) { return lhs.str() + rhs; }
inline string operator+(const char* lhs, const VConstString& rhs) { return lhs + rhs.str(); }
inline string operator+(const VConstString& lhs, const VConstString& rhs) {
    return lhs.str() + rhs.str();
}

inline bool operator<(const VConstString& lhs, const VConstString& rhs) {
    return lhs.hash() < rhs.hash();
}
inline bool operator>(const VConstString& lhs, const VConstString& rhs) {
    return lhs.hash() > rhs.hash();
}
inline bool operator==(const VConstString& lhs, const char* rhs) { return lhs.str() == rhs; }
inline bool operator==(const VConstString& lhs, const string& rhs) { return lhs.str() == rhs; }
inline bool operator==(const VConstString& lhs, const VConstString& rhs) {
    return lhs.str() == rhs.str();
}
inline bool operator!=(const VConstString& lhs, const char* rhs) { return lhs.str() != rhs; }
inline bool operator!=(const VConstString& lhs, const string& rhs) { return lhs.str() != rhs; }
inline bool operator!=(const VConstString& lhs, const VConstString& rhs) {
    return lhs.str() != rhs.str();
}

inline std::ostream& operator<<(std::ostream& os, const VConstString& str) { return os << str.str(); }

#endif  // Guard
