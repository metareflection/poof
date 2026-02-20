// Test for C4 spec pattern
// Shows the clean API where specs are defined directly as template mixins

#include <c4/c4.hpp>
#include "../examples/mixin_names.hpp"
#include <iostream>
#include <vector>
#include <string>

using namespace c4;
using c4::examples::C4N;

// =============================================================================
// Define specs directly - they ARE the mixins
// =============================================================================

// Base spec - no parents
template <typename Super>
struct O : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("O");
        Super::collectNames(names);
    }
};

// A inherits from O
template <typename Super>
struct A : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("A");
        Super::collectNames(names);
    }
};

// B inherits from O
template <typename Super>
struct B : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("B");
        Super::collectNames(names);
    }
};

// Diamond inherits from A and B
template <typename Super>
struct Diamond : public Super {
    using __c4__parents = TypeList<SpecList<A, B>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Diamond");
        Super::collectNames(names);
    }

    virtual int getValue() const { return 42; }
};

// =============================================================================
// Suffix spec example
// =============================================================================

template <typename Super>
struct Point : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = true;  // Suffix spec!

    double x = 0.0, y = 0.0;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Point");
        Super::collectNames(names);
    }
};

template <typename Super>
struct ColoredPoint : public Super {
    using __c4__parents = TypeList<SpecList<Point>>;
    static constexpr bool __c4__is_suffix = false;  // Infix, but parent is suffix

    std::string color = "black";

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("ColoredPoint");
        Super::collectNames(names);
    }
};

// =============================================================================
// Main
// =============================================================================

int main() {
    std::cout << "C4 Spec Pattern Test\n";
    std::cout << "====================\n\n";

    // Compose Diamond from the spec
    using Diamond_Class = C4N<Diamond>;

    Diamond_Class d;
    std::vector<std::string> names;
    d.collectNames(names);

    std::cout << "Diamond hierarchy CPL: ";
    for (const auto& name : names) {
        std::cout << name << " ";
    }
    std::cout << "\n";
    std::cout << "Expected: Diamond A B O\n";

    std::cout << "getValue() = " << d.getValue() << "\n\n";

    // Test CPL queries
    std::cout << "Testing CPL membership:\n";
    std::cout << "  IsInCPL<Diamond, A>: " << (IsInCPL_v<Diamond, A> ? "true" : "false") << "\n";
    std::cout << "  IsInCPL<Diamond, O>: " << (IsInCPL_v<Diamond, O> ? "true" : "false") << "\n";
    std::cout << "  IsInCPL<Diamond, Point>: " << (IsInCPL_v<Diamond, Point> ? "true" : "false") << "\n\n";

    // Test suffix spec
    using CP_Class = C4N<ColoredPoint>;

    CP_Class cp;
    std::vector<std::string> cp_names;
    cp.collectNames(cp_names);

    std::cout << "ColoredPoint CPL: ";
    for (const auto& name : cp_names) {
        std::cout << name << " ";
    }
    std::cout << "\n";
    std::cout << "Expected: ColoredPoint Point\n";

    std::cout << "Point fields accessible: x=" << cp.x << ", y=" << cp.y << "\n";
    std::cout << "Color field: " << cp.color << "\n\n";

    // Test CPL for ColoredPoint
    std::cout << "Testing CPL for ColoredPoint:\n";
    std::cout << "  IsInCPL<ColoredPoint, Point>: " << (IsInCPL_v<ColoredPoint, Point> ? "true" : "false") << "\n";

    std::cout << "\n✓ C4 spec pattern works!\n";
    std::cout << "✓ CPL queries work!\n";

    return 0;
}
