#pragma once

// C4: Flavorful Multiple Inheritance in C++
// Reference: Smaragdakis & Batory, "Mixin-Based Programming in C++" (2000)
//
// This is the only header users need to include.

#include <type_traits>
#include <cstddef>

// ============================================================================
// Metaprogramming Infrastructure
// ============================================================================

#include "type_list.hpp"
#include "type_map.hpp"
#include "dag.hpp"

namespace c4 {

// Make TypeList available in the c4 namespace for __c4__parents declarations
using meta::TypeList;

// ============================================================================
// Mixin - Bottom of every composition chain
// ============================================================================
// Every composed type ultimately inherits from Mixin.
// Mixin carries no methods beyond the destructor; application-level
// protocols (e.g. collectNames for introspection) live in separate mixins
// in examples/ or tests/, not here.

struct Mixin {
    Mixin() = default;
    virtual ~Mixin() = default;
};

// ============================================================================
// SpecList - Declare a spec's parents
// ============================================================================

template <template<typename> class... Specs>
struct SpecList {
    static constexpr size_t size = sizeof...(Specs);
};

// ============================================================================
// SpecHelper - Internal metadata wrapper
// ============================================================================
// Used internally by the C4 algorithm; users do not interact with this type
// directly.  SpecHelper is a subclass of Mixin so that any internal
// spec is-a Mixin at the type level.

template <template<typename> class Spec, typename FlatParents, typename ParentGroups, bool IsSuffix, size_t UniqueId>
struct SpecHelper : public Mixin {
    template <typename Base>
    using __c4__apply_mixin = Spec<Base>;

    using __c4__parents_type  = FlatParents;    // flat TypeList<SpecHelper<...>,...> for cycle detection + 1-parent opt
    using __c4__parent_groups = ParentGroups;   // TypeList<TypeList<SpecHelper<...>,...>,...> for local ordering
    static constexpr bool __c4__is_suffix = IsSuffix;
    static constexpr size_t __c4__id = UniqueId;
};

// ============================================================================
// Convert __c4__parents (TypeList<SpecList<...>,...>) to internal representation
// ============================================================================

// Forward declaration
template <template<typename> class Spec>
struct MakeSpecInternal;

// Convert one SpecList<A,B,...> to TypeList<SpecHelper<A>, SpecHelper<B>, ...>
template <typename SL>
struct SpecListToTypeList;

template <>
struct SpecListToTypeList<SpecList<>> {
    using type = meta::TypeList<>;
};

template <template<typename> class S, template<typename> class... Rest>
struct SpecListToTypeList<SpecList<S, Rest...>> {
    using type = meta::Cons_t<
        typename MakeSpecInternal<S>::type,
        typename SpecListToTypeList<SpecList<Rest...>>::type
    >;
};

template <typename SL>
using SpecListToTypeList_t = typename SpecListToTypeList<SL>::type;

// Convert TypeList<SpecList<A,B>, SpecList<C>,...> to:
//   groups = TypeList<TypeList<SpecHelper<A>,SpecHelper<B>>, TypeList<SpecHelper<C>>, ...>
//   flat   = TypeList<SpecHelper<A>, SpecHelper<B>, SpecHelper<C>, ...>
template <typename ParentsDecl>
struct ParentGroupsToInternal;

template <>
struct ParentGroupsToInternal<meta::TypeList<>> {
    using groups = meta::TypeList<>;
    using flat   = meta::TypeList<>;
};

template <typename FirstSL, typename... RestSLs>
struct ParentGroupsToInternal<meta::TypeList<FirstSL, RestSLs...>> {
private:
    using ThisGroup = SpecListToTypeList_t<FirstSL>;
    using RestConverted = ParentGroupsToInternal<meta::TypeList<RestSLs...>>;
public:
    using groups = meta::Cons_t<ThisGroup, typename RestConverted::groups>;
    using flat   = meta::Concat_t<ThisGroup, typename RestConverted::flat>;
};

template <template<typename> class Spec>
struct MakeSpecInternal {
private:
    using Instance      = Spec<Mixin>;
    using ParentsDecl   = typename Instance::__c4__parents;  // TypeList<SpecList<...>, ...>
    using Converted     = ParentGroupsToInternal<ParentsDecl>;
    static constexpr bool IsSuffix = Instance::__c4__is_suffix;
public:
    using type = SpecHelper<Spec,
                            typename Converted::flat,
                            typename Converted::groups,
                            IsSuffix,
                            __COUNTER__>;
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

template <template<typename> class M, typename FP, typename PG, size_t Id>
struct IsInternalSuffixSpec<SpecHelper<M, FP, PG, true, Id>> : std::true_type {};

template <typename Spec>
inline constexpr bool IsInternalSuffixSpec_v = IsInternalSuffixSpec<Spec>::value;

// Get the TypeList<SpecList<...>,...> of parent groups of a user spec
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

// ComposeImpl - transform a user spec template into its composed concrete class.
// Base is the root of the composition chain; defaults to Mixin.
// Use a custom Base (e.g. MixinNames from examples/mixin_names.hpp) to inject
// application-level protocols without coupling them to the core library.
template <template<typename> class Spec, typename Base = Mixin>
struct ComposeImpl {
private:
    using SpecInternal = MakeSpecInternal_t<Spec>;
    using PrecedenceList = GetPrecedenceList_t<SpecInternal>;
    using ReversedMRO = meta::Reverse_t<PrecedenceList>;
public:
    using type = typename ChainMixins<ReversedMRO, Base>::type;
    using precedence_list = PrecedenceList;
    using mro = PrecedenceList;
};

// C4<Spec[, Base]> - compose a spec into a concrete class.
// Base defaults to Mixin; supply a richer base to add protocols.
template <template<typename> class Spec, typename Base = Mixin>
using C4 = typename ComposeImpl<Spec, Base>::type;

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
