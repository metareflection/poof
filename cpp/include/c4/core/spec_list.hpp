#pragma once

#include <cstddef>

namespace c4 {

// ============================================================================
// SpecList - List of parent specs (replaces TypeList for spec parents)
// ============================================================================

template <template<typename> class... Specs>
struct SpecList {
    static constexpr size_t size = sizeof...(Specs);
};

// Helper to check if a spec list is empty
template <typename SL>
struct IsSpecListEmpty : std::false_type {};

template <>
struct IsSpecListEmpty<SpecList<>> : std::true_type {};

template <typename SL>
inline constexpr bool IsSpecListEmpty_v = IsSpecListEmpty<SL>::value;

} // namespace c4
