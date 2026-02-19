#pragma once

#include <cstddef>
#include <type_traits>

namespace c4 {
namespace meta {

// ============================================================================
// TypeList - Fundamental compile-time list type
// ============================================================================

template <typename... Types>
struct TypeList {
    static constexpr size_t size = sizeof...(Types);
};

// ============================================================================
// Basic Operations
// ============================================================================

// Head - Get first element of list
template <typename List>
struct Head;

template <typename T, typename... Rest>
struct Head<TypeList<T, Rest...>> {
    using type = T;
};

template <typename List>
using Head_t = typename Head<List>::type;

// Tail - Get all elements except first
template <typename List>
struct Tail;

template <typename T, typename... Rest>
struct Tail<TypeList<T, Rest...>> {
    using type = TypeList<Rest...>;
};

template <>
struct Tail<TypeList<>> {
    using type = TypeList<>;
};

template <typename List>
using Tail_t = typename Tail<List>::type;

// Cons - Prepend element to list
template <typename T, typename List>
struct Cons;

template <typename T, typename... Types>
struct Cons<T, TypeList<Types...>> {
    using type = TypeList<T, Types...>;
};

template <typename T, typename List>
using Cons_t = typename Cons<T, List>::type;

// Append - Add element to end of list
template <typename List, typename T>
struct Append;

template <typename... Types, typename T>
struct Append<TypeList<Types...>, T> {
    using type = TypeList<Types..., T>;
};

template <typename List, typename T>
using Append_t = typename Append<List, T>::type;

// Concat - Concatenate two lists
template <typename List1, typename List2>
struct Concat;

template <typename... Types1, typename... Types2>
struct Concat<TypeList<Types1...>, TypeList<Types2...>> {
    using type = TypeList<Types1..., Types2...>;
};

template <typename List1, typename List2>
using Concat_t = typename Concat<List1, List2>::type;

// ============================================================================
// Query Operations
// ============================================================================

// IsEmpty - Check if list is empty
template <typename List>
struct IsEmpty : std::false_type {};

template <>
struct IsEmpty<TypeList<>> : std::true_type {};

template <typename List>
inline constexpr bool IsEmpty_v = IsEmpty<List>::value;

// Contains - Check if type is in list
template <typename List, typename T>
struct Contains;

template <typename T>
struct Contains<TypeList<>, T> : std::false_type {};

template <typename T, typename... Rest>
struct Contains<TypeList<T, Rest...>, T> : std::true_type {};

template <typename T, typename U, typename... Rest>
struct Contains<TypeList<U, Rest...>, T> : Contains<TypeList<Rest...>, T> {};

template <typename List, typename T>
inline constexpr bool Contains_v = Contains<List, T>::value;

// IndexOf - Find index of type in list (-1 if not found)
template <typename List, typename T, size_t Index = 0>
struct IndexOf;

template <typename T, size_t Index>
struct IndexOf<TypeList<>, T, Index> {
    static constexpr int value = -1;
};

template <typename T, typename... Rest, size_t Index>
struct IndexOf<TypeList<T, Rest...>, T, Index> {
    static constexpr int value = Index;
};

template <typename T, typename U, typename... Rest, size_t Index>
struct IndexOf<TypeList<U, Rest...>, T, Index>
    : IndexOf<TypeList<Rest...>, T, Index + 1> {};

template <typename List, typename T>
inline constexpr int IndexOf_v = IndexOf<List, T>::value;

// At - Get element at index
template <typename List, size_t Index>
struct At;

template <typename T, typename... Rest>
struct At<TypeList<T, Rest...>, 0> {
    using type = T;
};

template <typename T, typename... Rest, size_t Index>
struct At<TypeList<T, Rest...>, Index> {
    using type = typename At<TypeList<Rest...>, Index - 1>::type;
};

template <typename List, size_t Index>
using At_t = typename At<List, Index>::type;

// ============================================================================
// Transform Operations
// ============================================================================

// Reverse - Reverse the list
template <typename List, typename Acc = TypeList<>>
struct Reverse;

template <typename Acc>
struct Reverse<TypeList<>, Acc> {
    using type = Acc;
};

template <typename T, typename... Rest, typename Acc>
struct Reverse<TypeList<T, Rest...>, Acc> {
    using type = typename Reverse<TypeList<Rest...>, Cons_t<T, Acc>>::type;
};

template <typename List>
using Reverse_t = typename Reverse<List>::type;

// Map - Apply metafunction to each element
template <template<typename> class F, typename List>
struct Map;

template <template<typename> class F>
struct Map<F, TypeList<>> {
    using type = TypeList<>;
};

template <template<typename> class F, typename T, typename... Rest>
struct Map<F, TypeList<T, Rest...>> {
    using type = Cons_t<typename F<T>::type, typename Map<F, TypeList<Rest...>>::type>;
};

template <template<typename> class F, typename List>
using Map_t = typename Map<F, List>::type;

// Filter - Keep elements matching predicate
template <template<typename> class Pred, typename List>
struct Filter;

template <template<typename> class Pred>
struct Filter<Pred, TypeList<>> {
    using type = TypeList<>;
};

template <template<typename> class Pred, typename T, typename... Rest>
struct Filter<Pred, TypeList<T, Rest...>> {
private:
    using rest_filtered = typename Filter<Pred, TypeList<Rest...>>::type;
public:
    using type = std::conditional_t<
        Pred<T>::value,
        Cons_t<T, rest_filtered>,
        rest_filtered
    >;
};

template <template<typename> class Pred, typename List>
using Filter_t = typename Filter<Pred, List>::type;

// RemoveDuplicates - Remove duplicate types from list
template <typename List, typename Seen = TypeList<>>
struct RemoveDuplicates;

template <typename Seen>
struct RemoveDuplicates<TypeList<>, Seen> {
    using type = TypeList<>;
};

template <typename T, typename... Rest, typename Seen>
struct RemoveDuplicates<TypeList<T, Rest...>, Seen> {
private:
    using rest_deduped = typename RemoveDuplicates<TypeList<Rest...>, Cons_t<T, Seen>>::type;
public:
    using type = std::conditional_t<
        Contains_v<Seen, T>,
        rest_deduped,
        Cons_t<T, rest_deduped>
    >;
};

template <typename List>
using RemoveDuplicates_t = typename RemoveDuplicates<List>::type;

// ============================================================================
// Special Operations (needed by C4 algorithm)
// ============================================================================

// RemoveNulls - Remove empty lists from list of lists
template <typename ListOfLists>
struct RemoveNulls;

template <>
struct RemoveNulls<TypeList<>> {
    using type = TypeList<>;
};

template <typename List, typename... Rest>
struct RemoveNulls<TypeList<List, Rest...>> {
private:
    using rest_cleaned = typename RemoveNulls<TypeList<Rest...>>::type;
public:
    using type = std::conditional_t<
        IsEmpty_v<List>,
        rest_cleaned,
        Cons_t<List, rest_cleaned>
    >;
};

template <typename ListOfLists>
using RemoveNulls_t = typename RemoveNulls<ListOfLists>::type;

// AppendReverseUntil - Reverse src and append to dst until predicate matches
// Returns pair: (remaining src, result dst)
// This is used by C4 to split lists at suffix specifications
template <template<typename> class Pred, typename Src, typename Dst>
struct AppendReverseUntil;

template <template<typename> class Pred, typename Dst>
struct AppendReverseUntil<Pred, TypeList<>, Dst> {
    using remaining = TypeList<>;
    using result = Dst;
};

template <template<typename> class Pred, typename T, typename... Rest, typename Dst>
struct AppendReverseUntil<Pred, TypeList<T, Rest...>, Dst> {
private:
    static constexpr bool matches = Pred<T>::value;
    using next = AppendReverseUntil<Pred, TypeList<Rest...>, Cons_t<T, Dst>>;
public:
    using remaining = std::conditional_t<matches, TypeList<T, Rest...>, typename next::remaining>;
    using result = std::conditional_t<matches, Dst, typename next::result>;
};

// ============================================================================
// Fold Operations
// ============================================================================

// FoldLeft - Left fold over list
template <template<typename, typename> class F, typename Init, typename List>
struct FoldLeft;

template <template<typename, typename> class F, typename Init>
struct FoldLeft<F, Init, TypeList<>> {
    using type = Init;
};

template <template<typename, typename> class F, typename Init, typename T, typename... Rest>
struct FoldLeft<F, Init, TypeList<T, Rest...>> {
    using type = typename FoldLeft<F, typename F<Init, T>::type, TypeList<Rest...>>::type;
};

template <template<typename, typename> class F, typename Init, typename List>
using FoldLeft_t = typename FoldLeft<F, Init, List>::type;

// FoldRight - Right fold over list
template <template<typename, typename> class F, typename Init, typename List>
struct FoldRight;

template <template<typename, typename> class F, typename Init>
struct FoldRight<F, Init, TypeList<>> {
    using type = Init;
};

template <template<typename, typename> class F, typename Init, typename T, typename... Rest>
struct FoldRight<F, Init, TypeList<T, Rest...>> {
    using type = typename F<T, typename FoldRight<F, Init, TypeList<Rest...>>::type>::type;
};

template <template<typename, typename> class F, typename Init, typename List>
using FoldRight_t = typename FoldRight<F, Init, List>::type;

// ============================================================================
// Utilities
// ============================================================================

// IsSame - Check if two lists contain same types in same order
template <typename List1, typename List2>
struct IsSame : std::false_type {};

template <typename... Types>
struct IsSame<TypeList<Types...>, TypeList<Types...>> : std::true_type {};

template <typename List1, typename List2>
inline constexpr bool IsSame_v = IsSame<List1, List2>::value;

} // namespace meta
} // namespace c4
