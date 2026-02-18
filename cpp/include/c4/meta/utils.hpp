#pragma once

#include <type_traits>
#include <cstddef>

namespace c4 {
namespace meta {

// ============================================================================
// Identity - Return the type unchanged
// ============================================================================

template <typename T>
struct Identity {
    using type = T;
};

template <typename T>
using Identity_t = typename Identity<T>::type;

// ============================================================================
// Conditional utilities
// ============================================================================

// If-then-else at type level (already in std, but for completeness)
template <bool Cond, typename Then, typename Else>
using If = std::conditional_t<Cond, Then, Else>;

// ============================================================================
// Always - Metafunction that always returns same type
// ============================================================================

template <typename T>
struct Always {
    template <typename...>
    using type = T;
};

// ============================================================================
// Not - Boolean negation
// ============================================================================

template <typename Pred>
struct Not {
    static constexpr bool value = !Pred::value;
};

template <typename Pred>
inline constexpr bool Not_v = Not<Pred>::value;

// ============================================================================
// And/Or - Boolean operations
// ============================================================================

template <typename... Preds>
struct And : std::true_type {};

template <typename P, typename... Rest>
struct And<P, Rest...> : std::conditional_t<P::value, And<Rest...>, std::false_type> {};

template <typename... Preds>
inline constexpr bool And_v = And<Preds...>::value;

template <typename... Preds>
struct Or : std::false_type {};

template <typename P, typename... Rest>
struct Or<P, Rest...> : std::conditional_t<P::value, std::true_type, Or<Rest...>> {};

template <typename... Preds>
inline constexpr bool Or_v = Or<Preds...>::value;

// ============================================================================
// Max/Min for compile-time values
// ============================================================================

template <auto A, auto B>
struct Max {
    static constexpr auto value = (A > B) ? A : B;
};

template <auto A, auto B>
inline constexpr auto Max_v = Max<A, B>::value;

template <auto A, auto B>
struct Min {
    static constexpr auto value = (A < B) ? A : B;
};

template <auto A, auto B>
inline constexpr auto Min_v = Min<A, B>::value;

// ============================================================================
// Type equality check
// ============================================================================

template <typename T, typename U>
struct IsSameType : std::false_type {};

template <typename T>
struct IsSameType<T, T> : std::true_type {};

template <typename T, typename U>
inline constexpr bool IsSameType_v = IsSameType<T, U>::value;

} // namespace meta
} // namespace c4
