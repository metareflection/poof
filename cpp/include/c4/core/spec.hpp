#pragma once

#include "spec_list.hpp"
#include "../meta/type_list.hpp"
#include "mixin.hpp"
#include <cstddef>

namespace c4 {

// ============================================================================
// Internal Specification Wrapper
// ============================================================================
// This is used internally by the C4 algorithm. Users don't see this.

template <template<typename> class Spec, typename ParentsTypeList, bool IsSuffix, size_t UniqueId>
struct SpecificationInternal {
    // Apply the spec as a mixin
    template <typename Base>
    using apply_mixin = Spec<Base>;

    using parents_type = ParentsTypeList;
    static constexpr bool is_suffix = IsSuffix;
    static constexpr size_t id = UniqueId;
};

// ============================================================================
// Convert SpecList to TypeList of Internal Specifications
// ============================================================================

template <template<typename> class Spec>
struct MakeSpecInternal;

template <typename SL>
struct SpecListToTypeList;

template <>
struct SpecListToTypeList<SpecList<>> {
    using type = meta::TypeList<>;
};

template <template<typename> class S, template<typename> class... Rest>
struct SpecListToTypeList<SpecList<S, Rest...>> {
    using type = typename meta::Cons<
        typename MakeSpecInternal<S>::type,
        typename SpecListToTypeList<SpecList<Rest...>>::type
    >::type;
};

template <typename SL>
using SpecListToTypeList_t = typename SpecListToTypeList<SL>::type;

// ============================================================================
// MakeSpecInternal - Convert a user spec to internal representation
// ============================================================================

template <template<typename> class Spec>
struct MakeSpecInternal {
private:
    // Instantiate with EmptyBase to extract metadata
    using Instance = Spec<EmptyBase>;

    // Extract parents SpecList and convert to TypeList of internal specs
    using ParentsList = typename Instance::parents;
    using ParentsTypeList = SpecListToTypeList_t<ParentsList>;

    // Extract suffix property
    static constexpr bool IsSuffix = Instance::is_suffix;

public:
    // Create the internal specification type
    using type = SpecificationInternal<Spec, ParentsTypeList, IsSuffix, __COUNTER__>;
};

template <template<typename> class Spec>
using MakeSpecInternal_t = typename MakeSpecInternal<Spec>::type;

// ============================================================================
// Spec Queries
// ============================================================================

// Check if a spec is a suffix spec (works with user specs)
template <template<typename> class Spec>
struct IsSuffixSpec {
    static constexpr bool value = Spec<EmptyBase>::is_suffix;
};

template <template<typename> class Spec>
inline constexpr bool IsSuffixSpec_v = IsSuffixSpec<Spec>::value;

// Check if an internal spec is a suffix spec (works with SpecificationInternal)
template <typename Spec>
struct IsInternalSuffixSpec : std::false_type {};

template <template<typename> class M, typename P, size_t Id>
struct IsInternalSuffixSpec<SpecificationInternal<M, P, true, Id>> : std::true_type {};

template <typename Spec>
inline constexpr bool IsInternalSuffixSpec_v = IsInternalSuffixSpec<Spec>::value;

// Get parents of a spec
template <template<typename> class Spec>
struct GetParents {
    using type = typename Spec<EmptyBase>::parents;
};

template <template<typename> class Spec>
using GetParents_t = typename GetParents<Spec>::type;

} // namespace c4
