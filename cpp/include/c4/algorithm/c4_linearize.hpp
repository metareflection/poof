#pragma once

#include "../meta/type_list.hpp"
#include "../meta/dag.hpp"
#include "../core/spec.hpp"
#include "c3_merge.hpp"
#include "merge_suffix.hpp"

namespace c4 {
namespace algorithm {

using namespace meta;

// ============================================================================
// C4 Linearization Algorithm
// ============================================================================

// The C4 algorithm computes the precedence list (method resolution order)
// for a specification. It extends C3 with support for suffix specifications.
//
// Algorithm steps:
// 1. Base cases (0 or 1 parents)
// 2. Extract parent precedence lists
// 3. Split each into prefix (infix specs) and suffix (suffix specs)  [C4]
// 4. Merge all suffix lists (must form total order)                  [C4]
// 5. Append local order (parents) to candidate lists
// 6. Remove redundant suffix elements from candidates                 [C4]
// 7. C3 merge on cleaned prefixes
// 8. Join merged prefix and merged suffix                            [C4]

// Forward declaration for recursive use
template <typename Spec>
struct C4Linearize;

// Helper: Compute implementation for 0 parents
template <typename Spec, typename Parents>
struct ComputeImpl0 {
    using precedence_list = TypeList<Spec>;
    using most_specific_suffix = If<IsInternalSuffixSpec_v<Spec>, Spec, void>;
};

// Helper: Compute implementation for 1 parent
template <typename Spec, typename Parents>
struct ComputeImpl1 {
private:
    using Parent = Head_t<Parents>;
    using ParentPL = typename C4Linearize<Parent>::precedence_list;
    using ParentSuffix = typename C4Linearize<Parent>::most_specific_suffix;

public:
    using precedence_list = Cons_t<Spec, ParentPL>;
    using most_specific_suffix = If<
        IsInternalSuffixSpec_v<Spec>,
        Spec,
        ParentSuffix
    >;
};

// Helper to get head of a list only if not empty (for MSS computation)
template <typename List, bool IsEmpty>
struct GetMSSFromListHelper {
    using type = void;
};

template <typename List>
struct GetMSSFromListHelper<List, false> {
    using type = Head_t<List>;
};

// Helper: Compute implementation for 2+ parents (full C4 algorithm)
template <typename Spec, typename Parents>
struct ComputeImplN {
private:
    // Step 1: Extract parent precedence lists
    template <typename P>
    struct GetPL {
        using type = typename C4Linearize<P>::precedence_list;
    };

    using ParentPLs = Map_t<GetPL, Parents>;

    // Step 2: Split into prefix and suffix
    using Splits = SplitAllLists<ParentPLs>;
    using Prefixes = typename Splits::prefixes;
    using Suffixes = typename Splits::suffixes;

    // Step 3: Merge suffix lists
    using MergedSuffix = MergeSuffixLists_t<Suffixes>;

    // Step 4: Append local order (parents list)
    using CandidatesWithLocal = Append_t<Prefixes, Parents>;

    // Step 5: Remove redundant suffix elements
    using CleanedCandidates = RemoveSuffixRedundancy_t<
        CandidatesWithLocal,
        MergedSuffix
    >;

    // Step 6: C3 merge on cleaned prefixes
    using MergedPrefix = C3Merge_t<CleanedCandidates>;

    // Step 7: Join prefix and suffix
    using Joined = Concat_t<MergedPrefix, MergedSuffix>;

    // Step 8: Prepend the spec itself
    using Final = Cons_t<Spec, Joined>;

    // Find most specific suffix (use helper to avoid Head_t on empty list)
    using MSS = If<
        IsInternalSuffixSpec_v<Spec>,
        Spec,
        typename GetMSSFromListHelper<MergedSuffix, IsEmpty_v<MergedSuffix>>::type
    >;

public:
    using precedence_list = Final;
    using most_specific_suffix = MSS;
};

template <typename Spec>
struct C4Linearize {
private:
    using Parents = typename Spec::parents_type;
    static constexpr size_t num_parents = Parents::size;

    // Validate: no cycles
    static_assert(!HasCycle_v<Spec>,
        "Circular dependency detected in specification");

    // Select the appropriate implementation based on number of parents
    using Result = std::conditional_t<
        num_parents == 0,
        ComputeImpl0<Spec, Parents>,
        std::conditional_t<
            num_parents == 1,
            ComputeImpl1<Spec, Parents>,
            ComputeImplN<Spec, Parents>
        >
    >;

public:
    using precedence_list = typename Result::precedence_list;
    using most_specific_suffix = typename Result::most_specific_suffix;

    // Convenience alias
    using type = precedence_list;
};

template <typename Spec>
using C4Linearize_t = typename C4Linearize<Spec>::type;

// ============================================================================
// GetPrecedenceList - Main entry point
// ============================================================================

template <typename Spec>
struct GetPrecedenceList {
    using type = C4Linearize_t<Spec>;
};

template <typename Spec>
using GetPrecedenceList_t = typename GetPrecedenceList<Spec>::type;

// ============================================================================
// Precedence List Queries
// ============================================================================

// GetMRO - Method Resolution Order (same as precedence list)
template <typename Spec>
using GetMRO_t = GetPrecedenceList_t<Spec>;

// IsInMRO - Check if a spec is in another's MRO
template <typename Spec, typename Ancestor>
struct IsInMRO {
    static constexpr bool value = Contains_v<GetMRO_t<Spec>, Ancestor>;
};

template <typename Spec, typename Ancestor>
inline constexpr bool IsInMRO_v = IsInMRO<Spec, Ancestor>::value;

} // namespace algorithm
} // namespace c4
