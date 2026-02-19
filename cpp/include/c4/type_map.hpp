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
// Contains - Check if key exists in map
// ============================================================================

template <typename Map, typename Key>
struct MapContains;

template <typename Key>
struct MapContains<TypeMap<>, Key> : std::false_type {};

template <typename Key, auto Value, typename... Rest>
struct MapContains<TypeMap<Pair<Key, Value>, Rest...>, Key> : std::true_type {};

template <typename Key, typename OtherKey, auto OtherValue, typename... Rest>
struct MapContains<TypeMap<Pair<OtherKey, OtherValue>, Rest...>, Key>
    : MapContains<TypeMap<Rest...>, Key> {};

template <typename Map, typename Key>
inline constexpr bool MapContains_v = MapContains<Map, Key>::value;

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

// ============================================================================
// BuildFromList - Build map from list of keys with initial value
// ============================================================================

template <typename KeyList, auto InitialValue = 0>
struct BuildFromList;

template <auto InitialValue>
struct BuildFromList<TypeList<>, InitialValue> {
    using type = TypeMap<>;
};

template <typename Key, typename... Rest, auto InitialValue>
struct BuildFromList<TypeList<Key, Rest...>, InitialValue> {
    using type = Insert_t<
        typename BuildFromList<TypeList<Rest...>, InitialValue>::type,
        Key,
        InitialValue
    >;
};

template <typename KeyList, auto InitialValue = 0>
using BuildFromList_t = typename BuildFromList<KeyList, InitialValue>::type;

// ============================================================================
// IncrementAll - Increment all keys in a list
// ============================================================================

template <typename Map, typename KeyList, auto Delta = 1>
struct IncrementAll;

template <typename Map, auto Delta>
struct IncrementAll<Map, TypeList<>, Delta> {
    using type = Map;
};

template <typename Map, typename Key, typename... Rest, auto Delta>
struct IncrementAll<Map, TypeList<Key, Rest...>, Delta> {
    using type = typename IncrementAll<
        Increment_t<Map, Key, Delta>,
        TypeList<Rest...>,
        Delta
    >::type;
};

template <typename Map, typename KeyList, auto Delta = 1>
using IncrementAll_t = typename IncrementAll<Map, KeyList, Delta>::type;

// ============================================================================
// Keys - Extract all keys from map as TypeList
// ============================================================================

template <typename Map>
struct Keys;

template <>
struct Keys<TypeMap<>> {
    using type = TypeList<>;
};

template <typename Key, auto Value, typename... Rest>
struct Keys<TypeMap<Pair<Key, Value>, Rest...>> {
    using type = Cons_t<Key, typename Keys<TypeMap<Rest...>>::type>;
};

template <typename Map>
using Keys_t = typename Keys<Map>::type;

// ============================================================================
// Values - Extract all values from map as integer sequence
// (Useful for debugging/validation)
// ============================================================================

template <typename Map>
struct Values;

template <>
struct Values<TypeMap<>> {
    template <typename T>
    struct Wrapper {};
    using type = Wrapper<void>; // Placeholder since we can't directly store values
};

// Note: In a real implementation, you might use std::integer_sequence
// but for our purposes, we mainly need Keys

} // namespace meta
} // namespace c4
