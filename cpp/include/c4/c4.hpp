#pragma once

// C4: Flavorful Multiple Inheritance in C++
// Reference: Smaragdakis & Batory, "Mixin-Based Programming in C++" (2000)
//
// This is the only header users need to include.
//
// Specs are binary templates: template <typename Self, typename Super> struct Foo : Super
//   Self  = the final composed concrete class (for self-reference, CRTP-style)
//   Super = next class in the mixin chain (call Super::method() for cooperation)

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

struct Mixin {
    Mixin() = default;
    virtual ~Mixin() = default;
};

// ============================================================================
// SpecList - Declare a spec's parents
// ============================================================================
// Specs are binary templates: template<typename Self, typename Super>

template <template<typename, typename> class... Specs>
struct SpecList {
    static constexpr size_t size = sizeof...(Specs);
};

// ============================================================================
// SpecHelper - Internal metadata wrapper
// ============================================================================
// Used internally by the C4 algorithm; users do not interact with this type.

template <template<typename, typename> class Spec,
          typename FlatParents, typename ParentGroups, bool IsSuffix>
struct SpecHelper : public Mixin {
    template <typename Self, typename Base>
    using __c4__apply_mixin = Spec<Self, Base>;

    using __c4__parents_type  = FlatParents;    // flat TypeList<SpecHelper<...>,...>
    using __c4__parent_groups = ParentGroups;   // TypeList<TypeList<SpecHelper<...>,...>,...>
    static constexpr bool __c4__is_suffix = IsSuffix;
};

// ============================================================================
// Convert __c4__parents (TypeList<SpecList<...>,...>) to internal representation
// ============================================================================

// Forward declaration
template <template<typename, typename> class Spec>
struct MakeSpecInternal;

// Convert one SpecList<A,B,...> to TypeList<SpecHelper<A>, SpecHelper<B>, ...>
template <typename SL>
struct SpecListToTypeList;

template <>
struct SpecListToTypeList<SpecList<>> {
    using type = meta::TypeList<>;
};

template <template<typename, typename> class S, template<typename, typename> class... Rest>
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

// Instantiate Spec<Mixin, Mixin> as a sentinel to read its metadata.
// Self=Mixin and Super=Mixin are placeholders — only __c4__parents and
// __c4__is_suffix are read, and those must not depend on Self or Super.
template <template<typename, typename> class Spec>
struct MakeSpecInternal {
private:
    using Instance    = Spec<Mixin, Mixin>;
    using ParentsDecl = typename Instance::__c4__parents;
    using Converted   = ParentGroupsToInternal<ParentsDecl>;
public:
    using type = SpecHelper<Spec,
                            typename Converted::flat,
                            typename Converted::groups,
                            Instance::__c4__is_suffix>;
};

template <template<typename, typename> class Spec>
using MakeSpecInternal_t = typename MakeSpecInternal<Spec>::type;

// ============================================================================
// Spec Queries
// ============================================================================

// Check if an internal SpecHelper is a suffix spec
template <typename Spec>
struct IsInternalSuffixSpec : std::false_type {};

template <template<typename, typename> class M, typename FP, typename PG>
struct IsInternalSuffixSpec<SpecHelper<M, FP, PG, true>> : std::true_type {};

template <typename Spec>
inline constexpr bool IsInternalSuffixSpec_v = IsInternalSuffixSpec<Spec>::value;

} // namespace c4

// ============================================================================
// Algorithm
// ============================================================================

#include "c4_linearize.hpp"

namespace c4 {

// ============================================================================
// High-Level Composition API
// ============================================================================

// Chain mixins from most general to most specific (left-fold over reversed CPL).
// The CPL is [MostSpecific, ..., MostGeneral]; we reverse it and left-fold so
// the result is MostSpecific<Self, ...<MostGeneral<Self, Base>>>.
// Self is the final concrete type and is passed to every mixin in the chain.

template <typename Self, typename ReversedCPL, typename Base>
struct ChainMixins;

template <typename Self, typename Base>
struct ChainMixins<Self, meta::TypeList<>, Base> {
    using type = Base;
};

// Left-fold: apply S to the running Base, then continue with the new Base.
template <typename Self, typename S, typename... Rest, typename Base>
struct ChainMixins<Self, meta::TypeList<S, Rest...>, Base> {
private:
    using ThisLevel = typename S::template __c4__apply_mixin<Self, Base>;
public:
    using type = typename ChainMixins<Self, meta::TypeList<Rest...>, ThisLevel>::type;
};

// Forward-declare C4Impl so it can serve as Self before being fully defined.
// C4Impl IS the concrete composed class; it passes itself as Self to every mixin.
template <template<typename, typename> class Spec, typename Base = Mixin>
struct C4Impl;

// Compute the chain base outside C4Impl — C4Impl<Spec,Base> is incomplete
// when used here, but only as a type argument, never dereferenced.
template <template<typename, typename> class Spec, typename Base>
struct C4ImplChain {
    using SpecInternal   = MakeSpecInternal_t<Spec>;
    using PrecedenceList = GetPrecedenceList_t<SpecInternal>;
    using chain_base     = typename ChainMixins<
                               C4Impl<Spec, Base>,
                               meta::Reverse_t<PrecedenceList>,
                               Base>::type;
};

// C4Impl - the concrete composed class.
// Inherits from the full mixin chain with Self = C4Impl<Spec, Base>.
template <template<typename, typename> class Spec, typename Base>
struct C4Impl : C4ImplChain<Spec, Base>::chain_base {
    using precedence_list = typename C4ImplChain<Spec, Base>::PrecedenceList;
};

// C4<Spec[, Base]> - compose a spec into a concrete class.
// Base defaults to Mixin; supply a richer base (e.g. MixinNames) to add protocols.
template <template<typename, typename> class Spec, typename Base = Mixin>
using C4 = C4Impl<Spec, Base>;

// ============================================================================
// CPL Membership Checking
// ============================================================================

// IsInCPL_v<Derived, Target> - check at compile time whether Target is in
// the class precedence list of Derived.
template <template<typename, typename> class Derived, template<typename, typename> class Target>
struct IsInCPL {
private:
    using DerivedSpec = MakeSpecInternal_t<Derived>;
    using TargetSpec  = MakeSpecInternal_t<Target>;
    using CPL         = GetPrecedenceList_t<DerivedSpec>;
public:
    static constexpr bool value = meta::Contains_v<CPL, TargetSpec>;
};

template <template<typename, typename> class Derived, template<typename, typename> class Target>
inline constexpr bool IsInCPL_v = IsInCPL<Derived, Target>::value;

} // namespace c4

// ============================================================================
// Version Information
// ============================================================================

#define C4_VERSION_MAJOR 0
#define C4_VERSION_MINOR 3
#define C4_VERSION_PATCH 0
#define C4_VERSION "0.3.0"
