#pragma once

#include "c4.hpp"
#include "core/spec.hpp"
#include "meta/dag.hpp"
#include "algorithm/merge_suffix.hpp"

namespace c4 {
namespace validation {

// ============================================================================
// Error Detection Predicates (No Static Asserts)
// ============================================================================
// These check for error conditions WITHOUT triggering static_asserts,
// allowing us to test that error detection works correctly.

// Check if a spec has circular dependencies
template <template<typename> class Spec>
struct HasCircularDependency {
private:
    using SpecInternal = MakeSpecInternal_t<Spec>;
public:
    static constexpr bool value = meta::HasCycle_v<SpecInternal>;
};

template <template<typename> class Spec>
inline constexpr bool HasCircularDependency_v = HasCircularDependency<Spec>::value;

// Check if suffix lists are compatible (no static_assert version)
// This checks the same condition as MergeTwoSuffixes but without asserting
template <typename Suffix1, typename Suffix2>
struct AreSuffixesCompatible {
private:
    static constexpr bool disjoint = algorithm::AreDisjoint_v<Suffix1, Suffix2>;
    static constexpr bool s1_is_suffix_of_s2 = algorithm::IsSuffixOf_v<Suffix2, Suffix1>;
    static constexpr bool s2_is_suffix_of_s1 = algorithm::IsSuffixOf_v<Suffix1, Suffix2>;

public:
    static constexpr bool value = disjoint || s1_is_suffix_of_s2 || s2_is_suffix_of_s1;
};

template <typename Suffix1, typename Suffix2>
inline constexpr bool AreSuffixesCompatible_v = AreSuffixesCompatible<Suffix1, Suffix2>::value;

// Check if a spec would trigger suffix incompatibility
// We need to check all pairs of suffix lists in the hierarchy
template <template<typename> class Spec>
struct HasSuffixIncompatibility {
private:
    // Helper to check if a spec's parents would have compatible suffixes
    template <typename S, typename = void>
    struct CheckImpl {
        static constexpr bool value = false;  // No parents, no conflict
    };

    template <typename S>
    struct CheckImpl<S, std::void_t<typename S::parents_type>> {
    private:
        using Parents = typename S::parents_type;

        // Note: Full suffix compatibility checking would require attempting
        // the merge, which triggers static_assert on incompatibility.
        // This predicate is conservative - returns false (no error detected)
        // unless we can determine incompatibility without triggering asserts.
        // Actual suffix errors are caught by MergeTwoSuffixes static_assert.
        static constexpr bool has_multiple_parents = (Parents::size > 1);

    public:
        // Conservative: assume compatible (actual errors caught at compose time)
        static constexpr bool value = false;
    };

    using SpecInternal = MakeSpecInternal_t<Spec>;

public:
    static constexpr bool value = CheckImpl<SpecInternal>::value;
};

template <template<typename> class Spec>
inline constexpr bool HasSuffixIncompatibility_v = HasSuffixIncompatibility<Spec>::value;

// ============================================================================
// CanCompose - Master Check
// ============================================================================
// This is the tricky one: we can't actually try to compose without triggering
// static_asserts. Instead, we check known error conditions and return false
// if any are present.
//
// NOTE: If this returns true, composition SHOULD succeed
//       If this returns false, composition MIGHT fail (we detected an error condition)

template <template<typename> class Spec>
struct CanCompose {
private:
    // Check for known error conditions
    static constexpr bool has_cycle = HasCircularDependency_v<Spec>;
    static constexpr bool has_suffix_conflict = HasSuffixIncompatibility_v<Spec>;

    // If any error condition is present, can't compose
    static constexpr bool has_error = has_cycle || has_suffix_conflict;

public:
    static constexpr bool value = !has_error;

    // Provide information about which error was detected
    static constexpr bool has_circular_dependency = has_cycle;
    static constexpr bool has_suffix_incompatibility = has_suffix_conflict;
};

template <template<typename> class Spec>
inline constexpr bool CanCompose_v = CanCompose<Spec>::value;

// ============================================================================
// ValidateSpec - Check spec is valid and provide detailed error info
// ============================================================================

template <template<typename> class Spec>
struct ValidateSpec {
    static constexpr bool is_valid = CanCompose_v<Spec>;
    static constexpr bool has_circular_dependency = CanCompose<Spec>::has_circular_dependency;
    static constexpr bool has_suffix_incompatibility = CanCompose<Spec>::has_suffix_incompatibility;

    // User-friendly check
    static constexpr bool passes_validation = is_valid;
};

template <template<typename> class Spec>
inline constexpr bool ValidateSpec_v = ValidateSpec<Spec>::is_valid;

} // namespace validation
} // namespace c4
