#pragma once

// Example: Simple Counting Mixin (from Smaragdakis & Batory 2000)
// Counts nodes and edges visited during graph traversal.
//
// Usage:
//   #include <c4/c4.hpp>
//   #include "counting.hpp"
//   using MyClass = c4::examples::Counting<c4::Mixin>;

#include <c4/c4.hpp>

namespace c4 {
namespace examples {

template <typename Super>
class Counting : public Super {
    int nodes_visited = 0;
    int edges_visited = 0;

public:
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;

    template <typename... Args>
    Counting(Args&&... args) : Super(std::forward<Args>(args)...) {}

    virtual int get_nodes_visited() const { return nodes_visited; }
    virtual int get_edges_visited() const { return edges_visited; }

    virtual void count_node() { ++nodes_visited; }
    virtual void count_edge() { ++edges_visited; }

    virtual void reset_counts() {
        nodes_visited = 0;
        edges_visited = 0;
    }

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Counting");
        Super::collectNames(names);
    }
};

} // namespace examples
} // namespace c4
