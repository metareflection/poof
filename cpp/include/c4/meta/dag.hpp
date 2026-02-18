#pragma once

#include "type_list.hpp"
#include "utils.hpp"
#include "../core/spec.hpp"

namespace c4 {
namespace meta {

// ============================================================================
// Ancestry Computation - Compute all ancestors of a specification
// ============================================================================

// Forward declaration
template <typename Spec, typename Visited = TypeList<>>
struct GetAllAncestors;

// Helper for computing when not visited
template <typename Spec, typename Visited>
struct GetAllAncestorsNotVisited {
private:
    using Parents = typename Spec::parents_type;
    using NewVisited = Cons_t<Spec, Visited>;

    // Recursively get ancestors of each parent
    template <typename ParentList, typename Acc, typename VisitedSoFar>
    struct GetParentAncestors;

    template <typename Acc, typename VisitedSoFar>
    struct GetParentAncestors<TypeList<>, Acc, VisitedSoFar> {
        using type = Acc;
        using visited = VisitedSoFar;
    };

    template <typename Parent, typename... RestParents, typename Acc, typename VisitedSoFar>
    struct GetParentAncestors<TypeList<Parent, RestParents...>, Acc, VisitedSoFar> {
    private:
        using ParentAncestry = GetAllAncestors<Parent, VisitedSoFar>;
        using ParentResult = typename ParentAncestry::type;
        using ParentVisited = typename ParentAncestry::visited;
        using Combined = Concat_t<Acc, ParentResult>;
        using Rest = GetParentAncestors<TypeList<RestParents...>, Combined, ParentVisited>;
    public:
        using type = typename Rest::type;
        using visited = typename Rest::visited;
    };

    using ParentResults = GetParentAncestors<Parents, TypeList<>, NewVisited>;

public:
    using type = Cons_t<Spec, typename ParentResults::type>;
    using visited = typename ParentResults::visited;
};

// Main implementation
template <typename Spec, typename... Visited>
struct GetAllAncestors<Spec, TypeList<Visited...>> {
private:
    static constexpr bool already_visited = Contains_v<TypeList<Visited...>, Spec>;

    using Computed = GetAllAncestorsNotVisited<Spec, TypeList<Visited...>>;

public:
    using type = std::conditional_t<already_visited, TypeList<>, typename Computed::type>;
    using visited = std::conditional_t<already_visited, TypeList<Visited...>, typename Computed::visited>;
};

template <typename Spec>
using GetAllAncestors_t = typename GetAllAncestors<Spec>::type;

// ============================================================================
// Unique Ancestors - Get ancestors with duplicates removed
// ============================================================================

template <typename Spec>
struct GetUniqueAncestors {
    using type = RemoveDuplicates_t<GetAllAncestors_t<Spec>>;
};

template <typename Spec>
using GetUniqueAncestors_t = typename GetUniqueAncestors<Spec>::type;

// ============================================================================
// Depth Computation - Maximum distance from base
// ============================================================================

// Helper for MaxParentDepth
template <typename ParentList>
struct MaxParentDepthImpl;

template <>
struct MaxParentDepthImpl<TypeList<>> {
    static constexpr size_t value = 0;
};

template <typename Parent, typename... Rest>
struct MaxParentDepthImpl<TypeList<Parent, Rest...>> {
private:
    template <typename Spec>
    struct GetDepthHelper;

    template <typename Spec>
    struct GetDepthHelper {
        static constexpr size_t value = GetDepthHelper<typename Spec::parents_type>::value + 1;
    };

    static constexpr size_t parent_depth = GetDepthHelper<Parent>::value;
    static constexpr size_t rest_depth = MaxParentDepthImpl<TypeList<Rest...>>::value;
public:
    static constexpr size_t value = Max_v<parent_depth, rest_depth>;
};

template <typename Spec>
struct GetDepth {
    static constexpr size_t value = 1 + MaxParentDepthImpl<typename Spec::parents_type>::value;
};

template <typename Spec>
inline constexpr size_t GetDepth_v = GetDepth<Spec>::value;

// ============================================================================
// Cycle Detection - Check for circular dependencies
// ============================================================================

// Helper for checking parents
template <typename Parents, typename NewPath>
struct AnyParentHasCycle;

template <typename NewPath>
struct AnyParentHasCycle<TypeList<>, NewPath> {
    static constexpr bool value = false;
};

template <typename Parent, typename... Rest, typename NewPath>
struct AnyParentHasCycle<TypeList<Parent, Rest...>, NewPath> {
    template <typename Spec, typename Path>
    struct HasCycleHelper;

    template <typename Spec, typename Path>
    struct HasCycleHelper {
        static constexpr bool in_path = Contains_v<Path, Spec>;
        static constexpr bool value = in_path || AnyParentHasCycle<typename Spec::parents_type, Cons_t<Spec, Path>>::value;
    };

    static constexpr bool value =
        HasCycleHelper<Parent, NewPath>::value ||
        AnyParentHasCycle<TypeList<Rest...>, NewPath>::value;
};

template <typename Spec, typename Path = TypeList<>>
struct HasCycle {
private:
    static constexpr bool in_path = Contains_v<Path, Spec>;
    using NewPath = Cons_t<Spec, Path>;
public:
    static constexpr bool value = in_path || AnyParentHasCycle<typename Spec::parents_type, NewPath>::value;
};

template <typename Spec>
inline constexpr bool HasCycle_v = HasCycle<Spec>::value;

// ============================================================================
// Ancestor Relationship - Check if one spec is ancestor of another
// ============================================================================

template <typename Spec, typename PotentialAncestor>
struct IsAncestorOf {
private:
    using Ancestors = GetUniqueAncestors_t<Spec>;
public:
    static constexpr bool value = Contains_v<Ancestors, PotentialAncestor>;
};

template <typename Spec, typename PotentialAncestor>
inline constexpr bool IsAncestorOf_v = IsAncestorOf<Spec, PotentialAncestor>::value;

// ============================================================================
// Find Most Specific Suffix Ancestor
// ============================================================================

// Helper to find first suffix in list
template <typename AncestorList>
struct FindFirstSuffix;

template <>
struct FindFirstSuffix<TypeList<>> {
    using type = void;
};

template <typename A, typename... Rest>
struct FindFirstSuffix<TypeList<A, Rest...>> {
    using type = std::conditional_t<IsInternalSuffixSpec_v<A>, A, typename FindFirstSuffix<TypeList<Rest...>>::type>;
};

template <typename Spec>
struct FindMostSpecificSuffix {
private:
    using Ancestors = GetUniqueAncestors_t<Spec>;
public:
    using type = typename FindFirstSuffix<Ancestors>::type;
};

template <typename Spec>
using FindMostSpecificSuffix_t = typename FindMostSpecificSuffix<Spec>::type;

// ============================================================================
// Validation - Check specification validity
// ============================================================================

template <typename Spec>
struct ValidateSpecification {
    // Check for cycles
    static_assert(!HasCycle_v<Spec>,
        "Circular dependency detected in specification hierarchy");

    // Note: Suffix property validation happens during linearization

    static constexpr bool value = true;
};

template <typename Spec>
inline constexpr bool ValidateSpecification_v = ValidateSpecification<Spec>::value;

} // namespace meta
} // namespace c4
