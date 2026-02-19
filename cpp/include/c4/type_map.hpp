#pragma once

#include "type_list.hpp"
#include <type_traits>

namespace c4 {
namespace meta {

// ============================================================================
// Pair - Key-value pair for type maps
// ============================================================================

template <typename Key, auto Value>
struct Pair {
    using key_type = Key;
    static constexpr auto value = Value;
};

// ============================================================================
// TypeMap - Compile-time associative map
// Represented as sorted list of Pairs for efficient lookup
// ============================================================================

template <typename... Pairs>
struct TypeMap {
    using pairs_list = TypeList<Pairs...>;
    static constexpr size_t size = sizeof...(Pairs);
};

// ============================================================================
// Get - Retrieve value for key (returns default if not found)
// ============================================================================

template <typename Map, typename Key, auto Default = 0>
struct Get;

template <typename Key, auto Default>
struct Get<TypeMap<>, Key, Default> {
    static constexpr auto value = Default;
};

template <typename Key, auto Value, auto Default, typename... Rest>
struct Get<TypeMap<Pair<Key, Value>, Rest...>, Key, Default> {
    static constexpr auto value = Value;
};

template <typename Key, typename OtherKey, auto OtherValue, auto Default, typename... Rest>
struct Get<TypeMap<Pair<OtherKey, OtherValue>, Rest...>, Key, Default> {
    static constexpr auto value = Get<TypeMap<Rest...>, Key, Default>::value;
};

template <typename Map, typename Key, auto Default = 0>
inline constexpr auto Get_v = Get<Map, Key, Default>::value;

// ============================================================================
// Insert - Add or update key-value pair
// ============================================================================

template <typename Map, typename Key, auto Value>
struct Insert;

template <typename Key, auto Value>
struct Insert<TypeMap<>, Key, Value> {
    using type = TypeMap<Pair<Key, Value>>;
};

// Key already exists - update value
template <typename Key, auto OldValue, auto NewValue, typename... Rest>
struct Insert<TypeMap<Pair<Key, OldValue>, Rest...>, Key, NewValue> {
    using type = TypeMap<Pair<Key, NewValue>, Rest...>;
};

// Key doesn't match - recurse
template <typename Key, auto Value, typename OtherKey, auto OtherValue, typename... Rest>
struct Insert<TypeMap<Pair<OtherKey, OtherValue>, Rest...>, Key, Value> {
    using head_pair = Pair<OtherKey, OtherValue>;
private:
    template <typename... Ps>
    struct Prepend;

    template <typename... Ps>
    struct Prepend<TypeMap<Ps...>> {
        using type = TypeMap<head_pair, Ps...>;
    };
public:
    using type = typename Prepend<typename Insert<TypeMap<Rest...>, Key, Value>::type>::type;
};

template <typename Map, typename Key, auto Value>
using Insert_t = typename Insert<Map, Key, Value>::type;

// ============================================================================
// Increment - Increment value for key (initializes to 1 if not present)
// ============================================================================

template <typename Map, typename Key, auto Delta = 1>
struct Increment {
private:
    static constexpr auto current = Get_v<Map, Key, 0>;
    static constexpr auto new_value = current + Delta;
public:
    using type = Insert_t<Map, Key, new_value>;
};

template <typename Map, typename Key, auto Delta = 1>
using Increment_t = typename Increment<Map, Key, Delta>::type;

// ============================================================================
// Decrement - Decrement value for key
// ============================================================================

template <typename Map, typename Key, auto Delta = 1>
struct Decrement {
private:
    static constexpr auto current = Get_v<Map, Key, 0>;
    static constexpr auto new_value = current - Delta;
public:
    using type = Insert_t<Map, Key, new_value>;
};

template <typename Map, typename Key, auto Delta = 1>
using Decrement_t = typename Decrement<Map, Key, Delta>::type;

} // namespace meta
} // namespace c4
