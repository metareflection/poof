#pragma once

// C4: Flavorful Multiple Inheritance in C++
//
// This is the main header file that provides the complete C4 implementation.
// Include this file to use the C4 linearization algorithm and optimal inheritance.

// ============================================================================
// Core Components
// ============================================================================

#include "core/mixin.hpp"
#include "core/spec_list.hpp"
#include "core/spec.hpp"

// ============================================================================
// Metaprogramming Infrastructure
// ============================================================================

#include "meta/type_list.hpp"
#include "meta/type_map.hpp"
#include "meta/utils.hpp"
#include "meta/dag.hpp"

// ============================================================================
// C4 Algorithm
// ============================================================================

#include "algorithm/c4_linearize.hpp"
#include "algorithm/c3_merge.hpp"
#include "algorithm/merge_suffix.hpp"

// ============================================================================
// Validation and Error Detection
// ============================================================================

#include "validation.hpp"

namespace c4 {

// Re-export key types and functions for convenience
using meta::TypeList;
using algorithm::C4Linearize;
using algorithm::C4Linearize_t;
using algorithm::GetPrecedenceList_t;
using algorithm::GetMRO_t;

// ============================================================================
// High-Level Composition API
// ============================================================================

// Helper: Chain mixins from most general to most specific
// MRO is [MostSpecific, ..., MostGeneral]
// We want to build: MostSpecific<...<MostGeneral<EmptyBase>>>
// So we reverse and fold from the right
template <typename ReversedMRO, typename Base>
struct ChainMixins;

template <typename Base>
struct ChainMixins<TypeList<>, Base> {
    using type = Base;
};

template <typename S, typename... Rest, typename Base>
struct ChainMixins<TypeList<S, Rest...>, Base> {
private:
    // Apply this spec's mixin to the base
    using ThisLevel = typename S::template apply_mixin<Base>;

    // Continue with the rest
    using RestResult = typename ChainMixins<TypeList<Rest...>, ThisLevel>::type;

public:
    using type = RestResult;
};

// Compose - Main entry point for composing a spec into a final class
// Takes a spec template and returns the composed class following C4 linearization
template <template<typename> class Spec>
struct Compose {
private:
    // Convert to internal specification
    using SpecInternal = MakeSpecInternal_t<Spec>;

    // Get the precedence list (most specific to most general)
    using PrecedenceList = GetPrecedenceList_t<SpecInternal>;

    // Reverse to get most general to most specific
    using ReversedMRO = meta::Reverse_t<PrecedenceList>;

public:
    // Chain the mixins together
    using type = typename ChainMixins<ReversedMRO, EmptyBase>::type;

    // Also expose the precedence list for introspection
    using precedence_list = PrecedenceList;
    using mro = PrecedenceList;
};

template <template<typename> class Spec>
using Compose_t = typename Compose<Spec>::type;

// ============================================================================
// MRO Membership Checking
// ============================================================================

// Check if Target spec is in the MRO of Derived spec (compile-time)
template <template<typename> class Derived, template<typename> class Target>
struct IsInMRO {
private:
    using DerivedSpec = MakeSpecInternal_t<Derived>;
    using TargetSpec = MakeSpecInternal_t<Target>;
    using MRO = GetPrecedenceList_t<DerivedSpec>;

public:
    static constexpr bool value = meta::Contains_v<MRO, TargetSpec>;
};

template <template<typename> class Derived, template<typename> class Target>
inline constexpr bool IsInMRO_v = IsInMRO<Derived, Target>::value;

// ============================================================================
// Casting Notes
// ============================================================================

// Note: The composed type creates a linear inheritance chain:
//   MostSpecific<...Parent<...Base<EmptyBase>>>
//
// Standard C++ upcasts work automatically because each spec inherits from
// the next in the chain.
//
// For dynamic downcasts, use dynamic_cast as usual.
//
// The linear chain ensures that all specs in the MRO are direct or indirect
// base classes, making casts safe when the spec is in the MRO (check with
// IsInMRO_v first).

// ============================================================================
// Convenience Functions for Debugging/Introspection
// ============================================================================

// PrintMRO - Helper to print MRO at compile time (shows in error messages)
template <template<typename> class Spec>
struct PrintMRO {
private:
    using SpecInternal = MakeSpecInternal_t<Spec>;
public:
    using mro = GetMRO_t<SpecInternal>;
    static_assert(sizeof(SpecInternal) == 0, "MRO printed above in error message");
};

// MROSize - Get the size of the MRO
template <template<typename> class Spec>
struct MROSize {
private:
    using SpecInternal = MakeSpecInternal_t<Spec>;
public:
    static constexpr size_t value = GetMRO_t<SpecInternal>::size;
};

template <template<typename> class Spec>
inline constexpr size_t MROSize_v = MROSize<Spec>::value;

// ============================================================================
// Validation Helpers
// ============================================================================

// ValidateHierarchy - Validate a spec hierarchy
template <template<typename> class Spec>
struct ValidateHierarchy {
private:
    using SpecInternal = MakeSpecInternal_t<Spec>;
public:
    // Check for cycles
    static_assert(!meta::HasCycle_v<SpecInternal>,
        "Circular dependency detected");

    // Compute MRO (will fail if C4 constraints violated)
    using mro = GetMRO_t<SpecInternal>;

    static constexpr bool value = true;
};

template <template<typename> class Spec>
inline constexpr bool ValidateHierarchy_v = ValidateHierarchy<Spec>::value;

} // namespace c4

// ============================================================================
// Version Information
// ============================================================================

#define C4_VERSION_MAJOR 0
#define C4_VERSION_MINOR 2
#define C4_VERSION_PATCH 0
#define C4_VERSION "0.2.0"
