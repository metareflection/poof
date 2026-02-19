#pragma once

#include "type_list.hpp"

namespace c4 {
namespace meta {

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
    struct HasCycleHelper {
        static constexpr bool in_path = Contains_v<Path, Spec>;
        static constexpr bool value = in_path || AnyParentHasCycle<typename Spec::__c4__parents_type, Cons_t<Spec, Path>>::value;
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
    static constexpr bool value = in_path || AnyParentHasCycle<typename Spec::__c4__parents_type, NewPath>::value;
};

template <typename Spec>
inline constexpr bool HasCycle_v = HasCycle<Spec>::value;

} // namespace meta
} // namespace c4
