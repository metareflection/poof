#pragma once

// C4 Linearization Algorithm
//
// This file merges all algorithm components:
//   - C3 merge with O(dn) ancestor-counting optimization
//   - C4 suffix specification support (split, merge, redundancy removal)
//   - C4Linearize: main entry point computing the precedence list (MRO)
//
// All of these are implementation details of each other and are not
// intended to be used independently.
//
// Spec types (Mixin, SpecificationInternal, etc.) are defined in c4.hpp
// before this file is included.

#include "type_list.hpp"
#include "type_map.hpp"
#include "dag.hpp"

namespace c4 {

using namespace meta;

// ============================================================================
// C3 Merge with O(dn) Optimization
// ============================================================================
//
// Takes a list of candidate lists and merges them into a single precedence
// list respecting:
//   - Local order: elements appear in same relative order as in input lists
//   - Monotonicity: no element appears before its predecessors
//
// Uses ancestor counting so eligible candidates are found in O(1) per step
// instead of scanning all tails in O(dn), reducing overall complexity from
// O(d²n²) to O(dn).

// --- Build ancestor counts (occurrences in tails) ---

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
    using type = std::conditional_t<IsEmpty_v<List>, RestTails, Cons_t<ThisTail, RestTails>>;
};

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

// --- Select next candidate: first head with tail-occurrence count == 0 ---

template <typename L, typename Counts>
struct TryListHelper;

template <typename Counts>
struct TryListHelper<TypeList<>, Counts> {
    using type = void;
    static constexpr bool found = false;
};

template <typename List, typename... Rest, typename Counts>
struct TryListHelper<TypeList<List, Rest...>, Counts> {
private:
    using Head = Head_t<List>;
    static constexpr int count = Get_v<Counts, Head, 0>;
    using RestResult = TryListHelper<TypeList<Rest...>, Counts>;
public:
    using type = std::conditional_t<(count == 0), Head, typename RestResult::type>;
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

// --- Remove selected from all lists, update counts for new heads ---

template <bool Matches, bool TailNotEmpty, typename T>
struct GetModifiedHead {
    using type = TypeList<>;
};

template <typename T>
struct GetModifiedHead<true, true, T> {
    using type = TypeList<Head_t<T>>;
};

template <typename Selected, typename L>
struct RemoveFromListAndTrackHelper;

template <typename Selected>
struct RemoveFromListAndTrackHelper<Selected, TypeList<>> {
    using lists = TypeList<>;
    using modified = TypeList<>;
};

template <typename Selected, typename List, typename... Rest>
struct RemoveFromListAndTrackHelper<Selected, TypeList<List, Rest...>> {
private:
    using H = Head_t<List>;
    using T = Tail_t<List>;
    static constexpr bool matches = std::is_same_v<H, Selected>;
    using RestResult = RemoveFromListAndTrackHelper<Selected, TypeList<Rest...>>;
    using ThisModified = typename GetModifiedHead<matches, !IsEmpty_v<T>, T>::type;
public:
    using lists = Cons_t<
        std::conditional_t<matches, T, List>,
        typename RestResult::lists
    >;
    using modified = Concat_t<ThisModified, typename RestResult::modified>;
};

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

// --- Main C3 merge loop ---

template <typename Result, typename Lists, typename Counts>
struct MergeLoopHelper {
private:
    using CleanedLists = RemoveNulls_t<Lists>;

    struct EmptyCase {
        using type = Result;
    };

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

// ============================================================================
// Suffix List Operations (C4 extension)
// ============================================================================
//
// C4 requires suffix specifications to form a total order.  Given any two
// suffix specifications, one's precedence list must be a suffix of the other's.

// --- IsSuffixOf: check if List2 is a suffix of List1 ---

template <typename L1, typename L2>
struct IsPrefixOfHelper;

template <>
struct IsPrefixOfHelper<TypeList<>, TypeList<>> {
    static constexpr bool value = true;
};

template <typename H2, typename... T2>
struct IsPrefixOfHelper<TypeList<>, TypeList<H2, T2...>> {
    static constexpr bool value = false;
};

template <typename H1, typename... T1>
struct IsPrefixOfHelper<TypeList<H1, T1...>, TypeList<>> {
    static constexpr bool value = true;
};

template <typename H1, typename... T1, typename H2, typename... T2>
struct IsPrefixOfHelper<TypeList<H1, T1...>, TypeList<H2, T2...>> {
    static constexpr bool value =
        std::is_same_v<H1, H2> &&
        IsPrefixOfHelper<TypeList<T1...>, TypeList<T2...>>::value;
};

template <typename List1, typename List2>
struct IsSuffixOf {
private:
    using Rev1 = Reverse_t<List1>;
    using Rev2 = Reverse_t<List2>;
public:
    static constexpr bool value = IsPrefixOfHelper<Rev1, Rev2>::value;
};

template <typename List1, typename List2>
inline constexpr bool IsSuffixOf_v = IsSuffixOf<List1, List2>::value;

// --- AreDisjoint: no common elements ---

template <typename List1, typename List2>
struct AreDisjoint {
private:
    template <typename L1, typename L2>
    struct HasCommonElement;

    template <typename L2>
    struct HasCommonElement<TypeList<>, L2> {
        static constexpr bool value = false;
    };

    template <typename H, typename... T, typename L2>
    struct HasCommonElement<TypeList<H, T...>, L2> {
        static constexpr bool value = Contains_v<L2, H> || HasCommonElement<TypeList<T...>, L2>::value;
    };

public:
    static constexpr bool value = !HasCommonElement<List1, List2>::value;
};

template <typename List1, typename List2>
inline constexpr bool AreDisjoint_v = AreDisjoint<List1, List2>::value;

// --- MergeTwoSuffixes: merge two suffix lists, asserting compatibility ---

template <typename Suffix1, typename Suffix2>
struct MergeTwoSuffixes {
private:
    static constexpr bool disjoint          = AreDisjoint_v<Suffix1, Suffix2>;
    static constexpr bool s1_suffix_of_s2   = IsSuffixOf_v<Suffix2, Suffix1>;
    static constexpr bool s2_suffix_of_s1   = IsSuffixOf_v<Suffix1, Suffix2>;
    static constexpr bool compatible = disjoint || s1_suffix_of_s2 || s2_suffix_of_s1;

public:
    static_assert(compatible,
        "Suffix incompatibility detected:\n"
        "  The suffix specifications in your hierarchy are not in total order.\n"
        "  The suffix lists share elements but neither is a suffix of the other.\n"
        "  This violates the suffix property required by C4.\n"
        "  Check which specifications are marked as suffix and their inheritance.");

    using type = std::conditional_t<
        disjoint,
        Concat_t<Suffix1, Suffix2>,
        std::conditional_t<(Suffix1::size >= Suffix2::size), Suffix1, Suffix2>
    >;
};

template <typename Suffix1, typename Suffix2>
using MergeTwoSuffixes_t = typename MergeTwoSuffixes<Suffix1, Suffix2>::type;

// --- MergeSuffixLists: fold-merge a list of suffix lists ---

template <typename SuffixLists>
struct MergeSuffixLists {
private:
    template <typename Acc, typename S>
    struct Merge2 {
        using type = MergeTwoSuffixes_t<Acc, S>;
    };
public:
    using type = FoldLeft_t<Merge2, TypeList<>, SuffixLists>;
};

template <typename SuffixLists>
using MergeSuffixLists_t = typename MergeSuffixLists<SuffixLists>::type;

// --- SplitPrefixSuffix: split a precedence list at first suffix spec ---

template <typename PrecedenceList>
struct SplitPrefixSuffix {
private:
    template <typename T>
    struct IsSuffixPred {
        static constexpr bool value = IsInternalSuffixSpec_v<T>;
    };

    using SplitResult = AppendReverseUntil<IsSuffixPred, PrecedenceList, TypeList<>>;

public:
    using prefix = Reverse_t<typename SplitResult::result>;
    using suffix = typename SplitResult::remaining;
};

// --- SplitAllLists: split multiple precedence lists ---

template <typename PrecedenceListList>
struct SplitAllLists {
private:
    template <typename PL>
    struct SplitOne {
        using type = SplitPrefixSuffix<PL>;
    };

    using splits = Map_t<SplitOne, PrecedenceListList>;

    template <typename SR> struct GetPrefix { using type = typename SR::prefix; };
    template <typename SR> struct GetSuffix { using type = typename SR::suffix; };

public:
    using prefixes = Map_t<GetPrefix, splits>;
    using suffixes = Map_t<GetSuffix, splits>;
};

// --- RemoveSuffixRedundancy: strip suffix elements from candidate lists ---

template <typename CandidateLists, typename MergedSuffix>
struct RemoveSuffixRedundancy {
private:
    template <typename Suffix, size_t Pos>
    struct BuildSuffixIndex;

    template <size_t Pos>
    struct BuildSuffixIndex<TypeList<>, Pos> {
        using type = TypeMap<>;
    };

    template <typename S, typename... Rest, size_t Pos>
    struct BuildSuffixIndex<TypeList<S, Rest...>, Pos> {
        using type = Insert_t<
            typename BuildSuffixIndex<TypeList<Rest...>, Pos + 1>::type,
            S, Pos
        >;
    };

    using SuffixIndex = typename BuildSuffixIndex<MergedSuffix, 0>::type;

    template <typename List>
    struct CleanOne {
    private:
        template <typename L, int LastPos>
        struct RemoveFromEnd;

        template <int LastPos>
        struct RemoveFromEnd<TypeList<>, LastPos> {
            using type = TypeList<>;
        };

        template <typename H, typename... T, int LastPos>
        struct RemoveFromEnd<TypeList<H, T...>, LastPos> {
        private:
            static constexpr int pos       = Get_v<SuffixIndex, H, -1>;
            static constexpr bool in_suffix = (pos >= 0);
            static constexpr bool in_order  = (pos > LastPos) || (LastPos < 0);
            using RestResult = RemoveFromEnd<TypeList<T...>, pos>;

        public:
            using type = std::conditional_t<
                (in_suffix && in_order),
                typename RestResult::type,
                TypeList<H, T...>
            >;

            static_assert(!in_suffix || in_order,
                "Suffix element out of order: an ancestor appears in the candidate list "
                "but its position relative to the merged suffix is inconsistent. "
                "This violates the suffix property.");
        };

    public:
        using type = typename RemoveFromEnd<List, -1>::type;
    };

public:
    using type = Map_t<CleanOne, CandidateLists>;
};

template <typename CandidateLists, typename MergedSuffix>
using RemoveSuffixRedundancy_t = typename RemoveSuffixRedundancy<CandidateLists, MergedSuffix>::type;

// ============================================================================
// C4 Linearization Algorithm
// ============================================================================
//
// Computes the precedence list (MRO) for a specification, extending C3 with
// suffix specification support.
//
// For each specification, based on number of parents:
//   0 parents: trivial list [Spec]
//   1 parent:  prepend Spec to parent's list
//   N parents: (full C4)
//     1. Extract parent precedence lists
//     2. Split each into prefix (infix specs) and suffix (suffix specs)
//     3. Merge all suffix lists (must form total order)
//     4. Append local order (parents) to prefix candidate lists
//     5. Remove redundant suffix elements from candidates
//     6. C3-merge the cleaned prefix candidates
//     7. Concatenate merged prefix + merged suffix
//     8. Prepend Spec

// Forward declaration for mutual recursion
template <typename Spec>
struct C4Linearize;

// --- 0-parent case ---
template <typename Spec, typename Parents>
struct ComputeImpl0 {
    using precedence_list = TypeList<Spec>;
    using most_specific_suffix = std::conditional_t<IsInternalSuffixSpec_v<Spec>, Spec, void>;
};

// --- 1-parent case ---
template <typename Spec, typename Parents>
struct ComputeImpl1 {
private:
    using Parent = Head_t<Parents>;
    using ParentPL = typename C4Linearize<Parent>::precedence_list;
    using ParentSuffix = typename C4Linearize<Parent>::most_specific_suffix;
public:
    using precedence_list = Cons_t<Spec, ParentPL>;
    using most_specific_suffix = std::conditional_t<IsInternalSuffixSpec_v<Spec>, Spec, ParentSuffix>;
};

// Helper: safe Head_t on possibly-empty list (returns void)
template <typename List, bool IsEmpty>
struct GetMSSFromListHelper {
    using type = void;
};

template <typename List>
struct GetMSSFromListHelper<List, false> {
    using type = Head_t<List>;
};

// --- N-parent case (full C4) ---
template <typename Spec, typename Parents>
struct ComputeImplN {
private:
    template <typename P>
    struct GetPL {
        using type = typename C4Linearize<P>::precedence_list;
    };

    using ParentPLs = Map_t<GetPL, Parents>;
    using Splits = SplitAllLists<ParentPLs>;
    using MergedSuffix = MergeSuffixLists_t<typename Splits::suffixes>;
    using CandidatesWithLocal = Append_t<typename Splits::prefixes, Parents>;
    using CleanedCandidates = RemoveSuffixRedundancy_t<CandidatesWithLocal, MergedSuffix>;
    using MergedPrefix = C3Merge_t<CleanedCandidates>;
    using Joined = Concat_t<MergedPrefix, MergedSuffix>;

    using MSS = std::conditional_t<
        IsInternalSuffixSpec_v<Spec>,
        Spec,
        typename GetMSSFromListHelper<MergedSuffix, IsEmpty_v<MergedSuffix>>::type
    >;

public:
    using precedence_list = Cons_t<Spec, Joined>;
    using most_specific_suffix = MSS;
};

// --- Main C4Linearize entry ---

template <typename Spec>
struct C4Linearize {
private:
    using Parents = typename Spec::__c4__parents_type;
    static constexpr size_t num_parents = Parents::size;

    static_assert(!HasCycle_v<Spec>,
        "Circular dependency detected in specification");

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
};

// ============================================================================
// Public API
// ============================================================================

template <typename Spec>
using GetPrecedenceList_t = typename C4Linearize<Spec>::precedence_list;

// Alias: MRO == precedence list
template <typename Spec>
using GetMRO_t = GetPrecedenceList_t<Spec>;

} // namespace c4
