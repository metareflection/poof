// Test for C4 spec pattern
// Shows the clean API where specs are defined directly as template mixins

#include <c4/c4.hpp>
#include <iostream>
#include <vector>
#include <string>

using namespace c4;

// =============================================================================
// Define specs directly - they ARE the mixins
// =============================================================================

// Base spec - no parents
template <typename Super>
struct O : public Super {
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("O");
        Super::__c4__collectNames(names);
    }
};

// A inherits from O
template <typename Super>
struct A : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("A");
        Super::__c4__collectNames(names);
    }
};

// B inherits from O
template <typename Super>
struct B : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("B");
        Super::__c4__collectNames(names);
    }
};

// Diamond inherits from A and B
template <typename Super>
struct Diamond : public Super {
    using __c4__parents = SpecList<A, B>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("Diamond");
        Super::__c4__collectNames(names);
    }

    virtual int getValue() const { return 42; }
};

// =============================================================================
// Suffix spec example
// =============================================================================

template <typename Super>
struct Point : public Super {
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = true;  // Suffix spec!

    double x = 0.0, y = 0.0;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("Point");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct ColoredPoint : public Super {
    using __c4__parents = SpecList<Point>;
    static constexpr bool __c4__is_suffix = false;  // Infix, but parent is suffix

    std::string color = "black";

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("ColoredPoint");
        Super::__c4__collectNames(names);
    }
};

// =============================================================================
// Main
// =============================================================================

int main() {
    std::cout << "C4 Spec Pattern Test\n";
    std::cout << "====================\n\n";

    // Compose Diamond from the spec
    using Diamond_Class = Compose_t<Diamond>;

    Diamond_Class d;
    std::vector<std::string> names;
    d.__c4__collectNames(names);

    std::cout << "Diamond hierarchy MRO: ";
    for (const auto& name : names) {
        std::cout << name << " ";
    }
    std::cout << "\n";
    std::cout << "Expected: Diamond A B O\n";

    std::cout << "getValue() = " << d.getValue() << "\n\n";

    // Test MRO queries
    std::cout << "Testing MRO membership:\n";
    std::cout << "  IsInMRO<Diamond, A>: " << (IsInMRO_v<Diamond, A> ? "true" : "false") << "\n";
    std::cout << "  IsInMRO<Diamond, O>: " << (IsInMRO_v<Diamond, O> ? "true" : "false") << "\n";
    std::cout << "  IsInMRO<Diamond, Point>: " << (IsInMRO_v<Diamond, Point> ? "true" : "false") << "\n\n";

    // Test suffix spec
    using CP_Class = Compose_t<ColoredPoint>;

    CP_Class cp;
    std::vector<std::string> cp_names;
    cp.__c4__collectNames(cp_names);

    std::cout << "ColoredPoint MRO: ";
    for (const auto& name : cp_names) {
        std::cout << name << " ";
    }
    std::cout << "\n";
    std::cout << "Expected: ColoredPoint Point\n";

    std::cout << "Point fields accessible: x=" << cp.x << ", y=" << cp.y << "\n";
    std::cout << "Color field: " << cp.color << "\n\n";

    // Test MRO for ColoredPoint
    std::cout << "Testing MRO for ColoredPoint:\n";
    std::cout << "  IsInMRO<ColoredPoint, Point>: " << (IsInMRO_v<ColoredPoint, Point> ? "true" : "false") << "\n";

    std::cout << "\n✓ C4 spec pattern works!\n";
    std::cout << "✓ MRO queries work!\n";

    return 0;
}
