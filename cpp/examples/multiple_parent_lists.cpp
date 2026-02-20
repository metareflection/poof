// Multiple parent lists example — demonstrates TypeList<SpecList<...>, SpecList<...>>.
//
// A spec's __c4__parents can list multiple independent SpecLists.
// Each SpecList constrains the relative order of its members in the CPL,
// but parent lists are independent of each other — no ordering is imposed between
// members of different lists.  C3 finds the unique linearization consistent
// with all constraints.
//
// Compare with a single parent list TypeList<SpecList<A, B, C>>:
//   - that constrains A before B before C.
// With two parent lists TypeList<SpecList<A, B>, SpecList<C>>:
//   - A must precede B, and C has no position constraint relative to {A, B}.
//   - C3 still produces a deterministic CPL given the full inheritance graph.
//
// Build:
//   g++ -std=c++20 -I../include multiple_parent_lists.cpp -o multiple_parent_lists && ./multiple_parent_lists

#include "mixin_names.hpp"
#include <iostream>
#include <vector>
#include <string>

using namespace c4;
using c4::examples::C4N;

template <typename Super>
struct O : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("O"); Super::collectNames(names);
    }
};

template <typename Super>
struct A : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("A"); Super::collectNames(names);
    }
};

template <typename Super>
struct B : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("B"); Super::collectNames(names);
    }
};

template <typename Super>
struct C : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("C"); Super::collectNames(names);
    }
};

// Two independent parent lists:
//   List 1: SpecList<A, B> — A must precede B in the CPL
//   List 2: SpecList<C>    — no positional constraint relative to A or B
//
// C3 determines the unique valid linearization: MultiParentList A B C O
template <typename Super>
struct MultiParentList : public Super {
    using __c4__parents = TypeList<SpecList<A, B>, SpecList<C>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("MultiParentList"); Super::collectNames(names);
    }
};

using MultiParentList_Class = C4N<MultiParentList>;

int main() {
    MultiParentList_Class mg;
    std::vector<std::string> names;
    mg.collectNames(names);

    std::cout << "CPL: ";
    for (const auto& n : names) std::cout << n << " ";
    std::cout << "\n";
    // CPL: MultiParentList A B C O

    return 0;
}
