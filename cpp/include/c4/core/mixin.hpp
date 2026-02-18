#pragma once

#include <type_traits>
#include <utility>
#include <vector>
#include <string>

namespace c4 {

// ============================================================================
// Basic Mixin Pattern (from Smaragdakis & Batory 2000)
// ============================================================================

// EmptyBase - Base case for mixin composition
struct EmptyBase {
    // Minimal interface - can be extended as needed
    EmptyBase() = default;
    virtual ~EmptyBase() = default;

    // Base method for collecting mixin names in MRO order
    // Each mixin should override this to append its name and call Super::collectNames
    virtual void collectNames(std::vector<std::string>& names) const {
        // Base case - do nothing
    }
};

// ============================================================================
// Mixin Template Pattern
// ============================================================================

// Example mixin - not used directly, just shows the pattern:
//
// template <typename Super>
// class MyMixin : public Super {
// public:
//     // Mixin functionality here
//     void my_method() { /* ... */ }
//
//     // Can call Super methods
//     void combined() {
//         Super::some_method();
//         my_method();
//     }
// };

// ============================================================================
// CRTP Helper - Curiously Recurring Template Pattern
// Used for type propagation in mixin layers
// ============================================================================

template <typename Derived>
struct CRTPBase {
    // Helper to get the derived type
    using derived_type = Derived;

    Derived& derived() {
        return static_cast<Derived&>(*this);
    }

    const Derived& derived() const {
        return static_cast<const Derived&>(*this);
    }
};

// ============================================================================
// Mixin Composition Helpers
// ============================================================================

// Compose2 - Compose two mixins
template <template<typename> class Mixin1, template<typename> class Mixin2>
struct Compose2 {
    template <typename Base>
    using type = Mixin1<Mixin2<Base>>;
};

// Compose3 - Compose three mixins
template <
    template<typename> class Mixin1,
    template<typename> class Mixin2,
    template<typename> class Mixin3
>
struct Compose3 {
    template <typename Base>
    using type = Mixin1<Mixin2<Mixin3<Base>>>;
};

// Note: More general composition is handled by the C4 linearization algorithm

// ============================================================================
// Constructor Forwarding Helper
// ============================================================================

// Mixins often need to forward constructors to their superclass.
// This macro helps with that:

#define C4_FORWARD_CONSTRUCTORS(MixinClass, SuperClass) \
    template <typename... Args> \
    MixinClass(Args&&... args) : SuperClass(std::forward<Args>(args)...) {}

// ============================================================================
// Method Override Helper (for explicit qualification)
// ============================================================================

// Per the S&B paper, it's best practice to explicitly qualify superclass
// method calls to avoid name lookup issues. This helper documents that:

#define C4_SUPER_CALL(SuperClass, method, ...) \
    SuperClass::method(__VA_ARGS__)

// ============================================================================
// Virtual Method Designation
// ============================================================================

// From S&B paper: A mixin can make a method virtual or not depending on
// whether its superclass declared it virtual. No special handling needed
// in C++ - it's automatic. Just be aware of this behavior.

// ============================================================================
// Example: Simple Counting Mixin (from S&B paper)
// ============================================================================

namespace examples {

template <typename Super>
class Counting : public Super {
    int nodes_visited = 0;
    int edges_visited = 0;

public:
    C4_FORWARD_CONSTRUCTORS(Counting, Super)

    // Getters
    int get_nodes_visited() const { return nodes_visited; }
    int get_edges_visited() const { return edges_visited; }

    // Increment counters
    void count_node() { ++nodes_visited; }
    void count_edge() { ++edges_visited; }

    // Reset
    void reset_counts() {
        nodes_visited = 0;
        edges_visited = 0;
    }
};

} // namespace examples

} // namespace c4
