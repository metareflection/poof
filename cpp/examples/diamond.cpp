// Diamond inheritance example — the canonical C4 usage.
// Demonstrates multiple inheritance with C3/C4 linearization.
//
// Build:
//   g++ -std=c++20 -I../include diamond.cpp -o diamond && ./diamond

#include "mixin_names.hpp"
#include <iostream>
#include <vector>
#include <string>

using namespace c4;
using c4::examples::C4N;

// Base spec - no parents.
// Super is Mixin when composed; Super::collectNames() is the empty base case.
template <typename Super>
struct O : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("O");
        Super::collectNames(names);
    }
};

// A and B each inherit from O
template <typename Super>
struct A : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("A");
        Super::collectNames(names);
    }
};

template <typename Super>
struct B : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("B");
        Super::collectNames(names);
    }
};

// Diamond inherits from both A and B
template <typename Super>
struct Diamond : public Super {
    using __c4__parents = TypeList<SpecList<A, B>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Diamond");
        Super::collectNames(names);
    }
};

// Compose Diamond into a concrete class using C4 linearization.
// MRO is computed at compile time: [Diamond, A, B, O]
using Diamond_Class = C4N<Diamond>;

// Compile-time MRO membership checks
static_assert(IsInMRO_v<Diamond, A>);    // A is an ancestor
static_assert(IsInMRO_v<Diamond, B>);    // B is an ancestor
static_assert(IsInMRO_v<Diamond, O>);    // O is an ancestor (shared base)

int main() {
    Diamond_Class d;

    std::vector<std::string> names;
    d.collectNames(names);

    std::cout << "MRO: ";
    for (const auto& n : names) std::cout << n << " ";
    std::cout << "\n";
    // Output: MRO: Diamond A B O
    // O appears once despite being a shared ancestor — C4 linearization handles it.

    return 0;
}
