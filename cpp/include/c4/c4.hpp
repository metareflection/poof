#pragma once

// C4: Flavorful Multiple Inheritance in C++
// Reference: Smaragdakis & Batory, "Mixin-Based Programming in C++" (2000)
//
// This is the only header users need to include.

#include <type_traits>
#include <cstddef>
#include <vector>
#include <string>

// ============================================================================
// Metaprogramming Infrastructure
// ============================================================================

#include "type_list.hpp"
#include "type_map.hpp"
#include "dag.hpp"

namespace c4 {

// ============================================================================
// Mixin - Bottom of every composition chain
// ============================================================================
// Every composed object ultimately derives from Mixin through the chained
// mixin hierarchy.  The virtual interface here is the c4 protocol that
// traversal / introspection code can rely on.

struct Mixin {
    Mixin() = default;
    virtual ~Mixin() = default;

    // Protocol method: collect mixin names in MRO order.
    // Each mixin layer overrides this, appends its own name, then calls
    // Super::__c4__collectNames to continue up the chain.
    virtual void __c4__collectNames(std::vector<std::string>& names) const { }
};

// ============================================================================
// SpecList - Declare a spec's parents
// ============================================================================

template <template<typename> class... Specs>
struct SpecList {
    static constexpr size_t size = sizeof...(Specs);
};

template <typename SL>
struct IsSpecListEmpty : std::false_type {};

template <>
struct IsSpecListEmpty<SpecList<>> : std::true_type {};

template <typename SL>
inline constexpr bool IsSpecListEmpty_v = IsSpecListEmpty<SL>::value;

// ============================================================================
// SpecificationInternal - Internal metadata wrapper
// ============================================================================
// Used internally by the C4 algorithm; users do not interact with this type
// directly.  SpecificationInternal is a subclass of Mixin so that any internal
// spec is-a Mixin at the type level.

template <template<typename> class Spec, typename ParentsTypeList, bool IsSuffix, size_t UniqueId>
struct SpecificationInternal : public Mixin {
    template <typename Base>
    using __c4__apply_mixin = Spec<Base>;

    using __c4__parents_type = ParentsTypeList;
    static constexpr bool __c4__is_suffix = IsSuffix;
    static constexpr size_t __c4__id = UniqueId;
};

// ============================================================================
// SpecList -> TypeList of SpecificationInternal
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

template <template<typename> class Spec>
struct MakeSpecInternal {
private:
    using Instance = Spec<Mixin>;
    using ParentsList = typename Instance::__c4__parents;
    using ParentsTypeList = SpecListToTypeList_t<ParentsList>;
    static constexpr bool IsSuffix = Instance::__c4__is_suffix;
public:
    using type = SpecificationInternal<Spec, ParentsTypeList, IsSuffix, __COUNTER__>;
};

template <template<typename> class Spec>
using MakeSpecInternal_t = typename MakeSpecInternal<Spec>::type;

// ============================================================================
// Spec Queries
// ============================================================================

// Check if a user spec is a suffix spec
template <template<typename> class Spec>
struct IsSuffixSpec {
    static constexpr bool value = Spec<Mixin>::__c4__is_suffix;
};

template <template<typename> class Spec>
inline constexpr bool IsSuffixSpec_v = IsSuffixSpec<Spec>::value;

// Check if an internal spec is a suffix spec
template <typename Spec>
struct IsInternalSuffixSpec : std::false_type {};

template <template<typename> class M, typename P, size_t Id>
struct IsInternalSuffixSpec<SpecificationInternal<M, P, true, Id>> : std::true_type {};

template <typename Spec>
inline constexpr bool IsInternalSuffixSpec_v = IsInternalSuffixSpec<Spec>::value;

// Get the SpecList of parents of a user spec
template <template<typename> class Spec>
struct GetParents {
    using type = typename Spec<Mixin>::__c4__parents;
};

template <template<typename> class Spec>
using GetParents_t = typename GetParents<Spec>::type;

} // namespace c4

// ============================================================================
// Algorithm
// ============================================================================

#include "c4_linearize.hpp"

namespace c4 {

// ============================================================================
// High-Level Composition API
// ============================================================================

// Chain mixins from most general to most specific.
// The MRO is [MostSpecific, ..., MostGeneral]; we reverse it and fold from
// the right so the resulting class is MostSpecific<...<MostGeneral<Mixin>>>.

template <typename ReversedMRO, typename Base>
struct ChainMixins;

template <typename Base>
struct ChainMixins<meta::TypeList<>, Base> {
    using type = Base;
};

template <typename S, typename... Rest, typename Base>
struct ChainMixins<meta::TypeList<S, Rest...>, Base> {
private:
    using ThisLevel = typename S::template __c4__apply_mixin<Base>;
    using RestResult = typename ChainMixins<meta::TypeList<Rest...>, ThisLevel>::type;
public:
    using type = RestResult;
};

// Compose - transform a user spec template into its composed concrete class.
template <template<typename> class Spec>
struct Compose {
private:
    using SpecInternal = MakeSpecInternal_t<Spec>;
    using PrecedenceList = GetPrecedenceList_t<SpecInternal>;
    using ReversedMRO = meta::Reverse_t<PrecedenceList>;
public:
    using type = typename ChainMixins<ReversedMRO, Mixin>::type;
    using precedence_list = PrecedenceList;
    using mro = PrecedenceList;
};

template <template<typename> class Spec>
using Compose_t = typename Compose<Spec>::type;

// ============================================================================
// MRO Membership Checking
// ============================================================================

// IsInMRO_v<Derived, Target> - check at compile time whether Target is in
// the MRO of Derived.
template <template<typename> class Derived, template<typename> class Target>
struct IsInMRO {
private:
    using DerivedSpec = MakeSpecInternal_t<Derived>;
    using TargetSpec  = MakeSpecInternal_t<Target>;
    using MRO = GetPrecedenceList_t<DerivedSpec>;
public:
    static constexpr bool value = meta::Contains_v<MRO, TargetSpec>;
};

template <template<typename> class Derived, template<typename> class Target>
inline constexpr bool IsInMRO_v = IsInMRO<Derived, Target>::value;

} // namespace c4

// ============================================================================
// Version Information
// ============================================================================

#define C4_VERSION_MAJOR 0
#define C4_VERSION_MINOR 2
#define C4_VERSION_PATCH 0
#define C4_VERSION "0.2.0"
