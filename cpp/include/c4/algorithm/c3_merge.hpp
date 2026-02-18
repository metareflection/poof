#pragma once

#include "../meta/type_list.hpp"
#include "../meta/type_map.hpp"
#include "../meta/utils.hpp"

namespace c4 {
namespace algorithm {

using namespace meta;

// ============================================================================
// C3 Merge Algorithm with O(dn) Optimization
// ============================================================================

// The C3 merge algorithm takes a list of candidate lists and merges them
// into a single precedence list that respects:
// - Local order: elements appear in same relative order as in input lists
// - Monotonicity: no element appears before its predecessors
//
// Optimization: Use ancestor counting to find eligible candidates in O(1)
// instead of scanning all tails in O(dn), reducing overall complexity from
// O(d²n²) to O(dn).

// ============================================================================
// Build Ancestor Counts - Count occurrences in tails
// ============================================================================

// Helper: Get all tails (everything except first element) from all lists
template <typename Lists>
struct GetAllTailsHelper;

template <>
struct GetAllTailsHelper<TypeList<>> {
    using type = TypeList<>;
};

template <typename List, typename... Rest>
struct GetAllTailsHelper<TypeList<List, Rest...>> {
private:
    using ThisTail = Tail_t<List>;
    using RestTails = typename GetAllTailsHelper<TypeList<Rest...>>::type;
public:
    using type = If<IsEmpty_v<List>, RestTails, Cons_t<ThisTail, RestTails>>;
};

// Flatten list of lists into single list
template <typename ListOfLists>
struct FlattenHelper;

template <>
struct FlattenHelper<TypeList<>> {
    using type = TypeList<>;
};

template <typename List, typename... Rest>
struct FlattenHelper<TypeList<List, Rest...>> {
    using type = Concat_t<List, typename FlattenHelper<TypeList<Rest...>>::type>;
};

// Count occurrences of each element
template <typename Elements, typename Map>
struct CountOccurrencesHelper;

template <typename Map>
struct CountOccurrencesHelper<TypeList<>, Map> {
    using type = Map;
};

template <typename E, typename... Rest, typename Map>
struct CountOccurrencesHelper<TypeList<E, Rest...>, Map> {
    using type = typename CountOccurrencesHelper<
        TypeList<Rest...>,
        Increment_t<Map, E>
    >::type;
};

template <typename CandidateLists>
struct BuildAncestorCounts {
private:
    using AllTails = typename GetAllTailsHelper<CandidateLists>::type;
    using AllTailElements = typename FlattenHelper<AllTails>::type;

public:
    using type = typename CountOccurrencesHelper<AllTailElements, TypeMap<>>::type;
};

template <typename CandidateLists>
using BuildAncestorCounts_t = typename BuildAncestorCounts<CandidateLists>::type;

// ============================================================================
// Select Next Candidate - Find first head with count == 0
// ============================================================================

// Helper to try each list to find a valid candidate
template <typename L, typename Counts>
struct TryListHelper;

template <typename Counts>
struct TryListHelper<TypeList<>, Counts> {
    using type = void; // No valid candidate found
    static constexpr bool found = false;
};

template <typename List, typename... Rest, typename Counts>
struct TryListHelper<TypeList<List, Rest...>, Counts> {
private:
    using Head = Head_t<List>;
    static constexpr int count = Get_v<Counts, Head, 0>;

    using RestResult = TryListHelper<TypeList<Rest...>, Counts>;
public:
    using type = If<(count == 0), Head, typename RestResult::type>;
    static constexpr bool found = (count == 0) || RestResult::found;
};

template <typename Lists, typename Counts>
struct SelectNext {
private:
    using Result = TryListHelper<Lists, Counts>;

public:
    using type = typename Result::type;
    static constexpr bool found = Result::found;

    static_assert(found,
        "C3 merge failed: No valid candidate found. "
        "This indicates a violation of linearization constraints. "
        "Check that your inheritance hierarchy does not have conflicting "
        "local precedence orders or violates monotonicity.");
};

template <typename Lists, typename Counts>
using SelectNext_t = typename SelectNext<Lists, Counts>::type;

// ============================================================================
// Remove Selected - Remove candidate from all lists and update counts
// ============================================================================

// Helper for lazy evaluation of new head after removal
template <bool Matches, bool TailNotEmpty, typename T>
struct GetModifiedHead {
    using type = TypeList<>;
};

template <typename T>
struct GetModifiedHead<true, true, T> {
    using type = TypeList<Head_t<T>>;
};

// Helper to remove Selected from front of each list AND track which were modified
template <typename Selected, typename L>
struct RemoveFromListAndTrackHelper;

template <typename Selected>
struct RemoveFromListAndTrackHelper<Selected, TypeList<>> {
    using lists = TypeList<>;
    using modified = TypeList<>;  // List of new heads from modified lists
};

template <typename Selected, typename List, typename... Rest>
struct RemoveFromListAndTrackHelper<Selected, TypeList<List, Rest...>> {
private:
    using H = Head_t<List>;
    using T = Tail_t<List>;
    static constexpr bool matches = IsSameType_v<H, Selected>;

    using RestResult = RemoveFromListAndTrackHelper<Selected, TypeList<Rest...>>;

    using ThisModified = typename GetModifiedHead<matches, !IsEmpty_v<T>, T>::type;

public:
    using lists = Cons_t<
        std::conditional_t<matches, T, List>,
        typename RestResult::lists
    >;
    using modified = Concat_t<ThisModified, typename RestResult::modified>;
};

// Helper to decrement counts for modified heads
template <typename ModifiedHeads, typename Counts>
struct DecrementModifiedHelper;

template <typename Counts>
struct DecrementModifiedHelper<TypeList<>, Counts> {
    using type = Counts;
};

template <typename H, typename... Rest, typename Counts>
struct DecrementModifiedHelper<TypeList<H, Rest...>, Counts> {
    using type = typename DecrementModifiedHelper<
        TypeList<Rest...>,
        Decrement_t<Counts, H>
    >::type;
};

template <typename Selected, typename Lists, typename Counts>
struct RemoveSelectedAndUpdate {
private:
    using RemovalResult = RemoveFromListAndTrackHelper<Selected, Lists>;
    using ListsAfterRemoval = typename RemovalResult::lists;
    using ModifiedHeads = typename RemovalResult::modified;
    using UpdatedCounts = typename DecrementModifiedHelper<ModifiedHeads, Counts>::type;

public:
    using lists = ListsAfterRemoval;
    using counts = UpdatedCounts;
};

// ============================================================================
// C3 Merge - Main algorithm
// ============================================================================

// Helper for MergeLoop recursion (must be outside C3Merge)
template <typename Result, typename Lists, typename Counts>
struct MergeLoopHelper {
private:
    using CleanedLists = RemoveNulls_t<Lists>;

    // Empty list case
    struct EmptyCase {
        using type = Result;
    };

    // Non-empty list case
    struct NonEmptyCase {
    private:
        using Next = SelectNext_t<CleanedLists, Counts>;
        using Updated = RemoveSelectedAndUpdate<Next, CleanedLists, Counts>;
        using NextResult = Append_t<Result, Next>;
    public:
        using type = typename MergeLoopHelper<NextResult, typename Updated::lists, typename Updated::counts>::type;
    };

public:
    using type = typename std::conditional_t<
        IsEmpty_v<CleanedLists>,
        EmptyCase,
        NonEmptyCase
    >::type;
};

// C3Merge implementation
template <typename CandidateLists>
struct C3Merge {
private:
    using CleanedLists = RemoveNulls_t<CandidateLists>;
    using InitialCounts = BuildAncestorCounts_t<CleanedLists>;

public:
    using type = typename MergeLoopHelper<TypeList<>, CleanedLists, InitialCounts>::type;
};

template <typename CandidateLists>
using C3Merge_t = typename C3Merge<CandidateLists>::type;

} // namespace algorithm
} // namespace c4
