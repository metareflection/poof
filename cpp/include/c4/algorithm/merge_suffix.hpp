#pragma once

#include "../meta/type_list.hpp"
#include "../meta/utils.hpp"
#include "../core/spec.hpp"

namespace c4 {
namespace algorithm {

using namespace meta;

// ============================================================================
// Suffix Merging for C4 Algorithm
// ============================================================================

// The C4 algorithm requires that suffix specifications form a total order.
// That is, given any two suffix specifications, one must be a suffix of the
// other's precedence list.

// ============================================================================
// IsSuffixOf - Check if List2 is a suffix of List1
// ============================================================================

// Helper: Check if L2 is a prefix of L1
template <typename L1, typename L2>
struct IsPrefixOfHelper;

// Both empty - true
template <>
struct IsPrefixOfHelper<TypeList<>, TypeList<>> {
    static constexpr bool value = true;
};

// L1 empty, L2 non-empty - false
template <typename H2, typename... T2>
struct IsPrefixOfHelper<TypeList<>, TypeList<H2, T2...>> {
    static constexpr bool value = false;
};

// L1 non-empty, L2 empty - true (empty is prefix of anything)
template <typename H1, typename... T1>
struct IsPrefixOfHelper<TypeList<H1, T1...>, TypeList<>> {
    static constexpr bool value = true;
};

// Both non-empty - check heads and recurse
template <typename H1, typename... T1, typename H2, typename... T2>
struct IsPrefixOfHelper<TypeList<H1, T1...>, TypeList<H2, T2...>> {
    static constexpr bool value =
        IsSameType_v<H1, H2> &&
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

// ============================================================================
// MergeTwoSuffixes - Merge two suffix lists
// ============================================================================

// Check if two lists are disjoint (no common elements)
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

template <typename Suffix1, typename Suffix2>
struct MergeTwoSuffixes {
private:
    // Check if lists are disjoint
    static constexpr bool disjoint = AreDisjoint_v<Suffix1, Suffix2>;

    // Check if one is suffix of the other
    static constexpr bool s1_is_suffix_of_s2 = IsSuffixOf_v<Suffix2, Suffix1>;
    static constexpr bool s2_is_suffix_of_s1 = IsSuffixOf_v<Suffix1, Suffix2>;

    // Compatible if disjoint OR one is suffix of other
    static constexpr bool compatible = disjoint || s1_is_suffix_of_s2 || s2_is_suffix_of_s1;

public:
    static_assert(compatible,
        "Suffix incompatibility detected:\n"
        "  The suffix specifications in your hierarchy are not in total order.\n"
        "  The suffix lists share elements but neither is a suffix of the other.\n"
        "  This violates the suffix property required by C4.\n"
        "  Check which specifications are marked as suffix and their inheritance.");

    // If disjoint, concatenate; otherwise return the longer one
    using type = std::conditional_t<
        disjoint,
        Concat_t<Suffix1, Suffix2>,
        std::conditional_t<
            (Suffix1::size >= Suffix2::size),
            Suffix1,
            Suffix2
        >
    >;
};

template <typename Suffix1, typename Suffix2>
using MergeTwoSuffixes_t = typename MergeTwoSuffixes<Suffix1, Suffix2>::type;

// ============================================================================
// MergeSuffixLists - Merge multiple suffix lists
// ============================================================================

template <typename SuffixLists>
struct MergeSuffixLists {
private:
    // Fold using MergeTwoSuffixes
    template <typename Acc, typename S>
    struct Merge2 {
        using type = MergeTwoSuffixes_t<Acc, S>;
    };

public:
    using type = FoldLeft_t<Merge2, TypeList<>, SuffixLists>;
};

template <typename SuffixLists>
using MergeSuffixLists_t = typename MergeSuffixLists<SuffixLists>::type;

// ============================================================================
// SplitPrefixSuffix - Split a precedence list at first suffix spec
// ============================================================================

template <typename PrecedenceList>
struct SplitPrefixSuffix {
private:
    // Predicate: is this a suffix specification?
    template <typename T>
    struct IsSuffixPred {
        static constexpr bool value = IsInternalSuffixSpec_v<T>;
    };

    // Use AppendReverseUntil to split
    using SplitResult = AppendReverseUntil<IsSuffixPred, PrecedenceList, TypeList<>>;

public:
    // prefix: infix specs (reversed, will be reversed back)
    using prefix = Reverse_t<typename SplitResult::result>;
    // suffix: suffix specs to end
    using suffix = typename SplitResult::remaining;
};

// ============================================================================
// SplitAllLists - Split multiple precedence lists
// ============================================================================

template <typename PrecedenceListList>
struct SplitAllLists {
private:
    template <typename PL>
    struct SplitOne {
        using type = SplitPrefixSuffix<PL>;
    };

public:
    using splits = Map_t<SplitOne, PrecedenceListList>;

    // Extract all prefixes
    template <typename SplitResult>
    struct GetPrefix {
        using type = typename SplitResult::prefix;
    };

    using prefixes = Map_t<GetPrefix, splits>;

    // Extract all suffixes
    template <typename SplitResult>
    struct GetSuffix {
        using type = typename SplitResult::suffix;
    };

    using suffixes = Map_t<GetSuffix, splits>;
};

// ============================================================================
// RemoveSuffixRedundancy - Remove suffix elements from candidate lists
// ============================================================================

template <typename CandidateLists, typename MergedSuffix>
struct RemoveSuffixRedundancy {
private:
    // Build index map: suffix element -> position from end
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
            S,
            Pos
        >;
    };

    using SuffixIndex = typename BuildSuffixIndex<MergedSuffix, 0>::type;

    // Clean one list: remove suffix elements that appear in correct order
    template <typename List>
    struct CleanOne {
    private:
        // Walk from end, removing elements in suffix until we hit one not in suffix
        template <typename L, int LastPos>
        struct RemoveFromEnd;

        template <int LastPos>
        struct RemoveFromEnd<TypeList<>, LastPos> {
            using type = TypeList<>;
        };

        template <typename H, typename... T, int LastPos>
        struct RemoveFromEnd<TypeList<H, T...>, LastPos> {
        private:
            static constexpr int pos = Get_v<SuffixIndex, H, -1>;
            static constexpr bool in_suffix = (pos >= 0);
            static constexpr bool in_order = (pos > LastPos) || (LastPos < 0);

            // If in suffix and in order, remove and continue
            // Otherwise, keep rest
            using RestResult = RemoveFromEnd<TypeList<T...>, pos>;

        public:
            using type = If<
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

} // namespace algorithm
} // namespace c4
