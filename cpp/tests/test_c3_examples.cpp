// Test cases ported from gerbil/src/gerbil/test/c3-test.ss
// These test the C3 linearization algorithm with various hierarchies
// All mixins are templates that chain properly and implement __c4__collectNames

#include <c4/c4.hpp>
#include <type_traits>
#include <iostream>
#include <vector>
#include <string>
#include <cassert>

using namespace c4;
using namespace c4::meta;

// =============================================================================
// Test Helpers
// =============================================================================

template <typename Actual, typename Expected>
struct AssertSame {
    static_assert(std::is_same_v<Actual, Expected>,
        "Precedence list mismatch");
    static constexpr bool value = true;
};

#define CHECK_MRO(Spec, ...) \
    static_assert(AssertSame< \
        GetPrecedenceList_t<Spec>, \
        TypeList<__VA_ARGS__> \
    >::value, "MRO check failed for " #Spec)

// Helper to verify runtime MRO matches expected
bool verifyMRO(const std::vector<std::string>& actual,
               const std::vector<std::string>& expected,
               const std::string& testName) {
    // Always print actual MRO for debugging
    std::cout << testName << ":\n";
    std::cout << "  Actual:   ";
    for (const auto& name : actual) {
        std::cout << name << " ";
    }
    std::cout << "\n  Expected: ";
    for (const auto& name : expected) {
        std::cout << name << " ";
    }
    std::cout << "\n";

    if (actual.size() != expected.size()) {
        std::cout << "✗ Size mismatch (got "
                  << actual.size() << ", expected " << expected.size() << ")\n\n";
        return false;
    }

    for (size_t i = 0; i < actual.size(); ++i) {
        if (actual[i] != expected[i]) {
            std::cout << "✗ Mismatch at position " << i
                      << " (got '" << actual[i] << "', expected '"
                      << expected[i] << "')\n\n";
            return false;
        }
    }

    std::cout << "✓ Passed\n\n";
    return true;
}

// =============================================================================
// Wikipedia 2021 Example
// https://en.wikipedia.org/wiki/C3_linearization
// =============================================================================

namespace wiki2021 {

// Unified spec pattern: spec IS the mixin
template <typename Super>
struct O : public Super {
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("O");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct A : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("A");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct B : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("B");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct C : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("C");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct D : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("D");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct E : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("E");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct K1 : public Super {
    using __c4__parents = SpecList<A, B, C>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("K1");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct K2 : public Super {
    using __c4__parents = SpecList<D, B, E>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("K2");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct K3 : public Super {
    using __c4__parents = SpecList<D, A>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("K3");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct Z : public Super {
    using __c4__parents = SpecList<K1, K2, K3>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("Z");
        Super::__c4__collectNames(names);
    }
};

// Compose into final class
using Z_Class = Compose_t<Z>;

// Runtime verification
bool runRuntimeTests() {
    Z_Class z;
    std::vector<std::string> names;
    z.__c4__collectNames(names);

    std::vector<std::string> expected = {
        "Z", "K1", "K2", "K3", "D", "A", "B", "C", "E", "O"
    };

    return verifyMRO(names, expected, "Wikipedia 2021 (Z hierarchy)");
}

} // namespace wiki2021

// =============================================================================
// Wikipedia 2023 Example (with K->J, Z->Y)
// https://en.wikipedia.org/wiki/C3_linearization
// =============================================================================

namespace wiki2023 {

template <typename Super>
struct O : public Super {
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("O");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct A : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("A");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct B : public Super {
    using __c4__parents = SpecList<O>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("B");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct J1 : public Super {
    using __c4__parents = SpecList<A>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("J1");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct J2 : public Super {
    using __c4__parents = SpecList<A, B>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("J2");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct J3 : public Super {
    using __c4__parents = SpecList<B>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("J3");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct Y : public Super {
    using __c4__parents = SpecList<J1, J2, J3>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("Y");
        Super::__c4__collectNames(names);
    }
};

// Compose and test
using Y_Class = Compose_t<Y>;

bool runRuntimeTests() {
    Y_Class y;
    std::vector<std::string> names;
    y.__c4__collectNames(names);

    std::vector<std::string> expected = {
        "Y", "J1", "J2", "A", "J3", "B", "O"
    };

    return verifyMRO(names, expected, "Wikipedia 2023 (Y hierarchy)");
}

} // namespace wiki2023

// =============================================================================
// Boat Example from Gerbil c3-test.ss
// =============================================================================

namespace boat {

template <typename Super>
struct DB : public Super {  // DayBoat
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("DB");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct WB : public Super {  // WheelBoat
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("WB");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct EL : public Super {  // EngineLess
    using __c4__parents = SpecList<DB>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("EL");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct SM : public Super {  // SmallMultihull
    using __c4__parents = SpecList<DB>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("SM");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct PWB : public Super {  // PowerWheelBoat
    using __c4__parents = SpecList<WB>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("PWB");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct SC : public Super {  // SmallCatamaran
    using __c4__parents = SpecList<SM>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("SC");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct P : public Super {  // Pedalo
    using __c4__parents = SpecList<EL, PWB, SC>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("P");
        Super::__c4__collectNames(names);
    }
};

// Compose and test
using P_Class = Compose_t<P>;

bool runRuntimeTests() {
    P_Class p;
    std::vector<std::string> names;
    p.__c4__collectNames(names);

    std::vector<std::string> expected = {
        "P", "EL", "PWB", "WB", "SC", "SM", "DB"
    };

    return verifyMRO(names, expected, "Boat (Pedalo hierarchy)");
}

} // namespace boat

// =============================================================================
// StackOverflow Complex Example
// =============================================================================

namespace stackoverflow {

template <typename Super>
struct HH : public Super {
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("HH");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct GG : public Super {
    using __c4__parents = SpecList<>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("GG");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct II : public Super {
    using __c4__parents = SpecList<HH>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("II");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct FF : public Super {
    using __c4__parents = SpecList<HH, GG>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("FF");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct EE : public Super {
    using __c4__parents = SpecList<II, FF>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("EE");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct DD : public Super {
    using __c4__parents = SpecList<FF>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("DD");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct CC : public Super {
    using __c4__parents = SpecList<EE, DD>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("CC");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct BB : public Super {
    using __c4__parents = SpecList<EE>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("BB");
        Super::__c4__collectNames(names);
    }
};

template <typename Super>
struct AA : public Super {
    using __c4__parents = SpecList<CC, BB>;
    static constexpr bool __c4__is_suffix = false;

    void __c4__collectNames(std::vector<std::string>& names) const override {
        names.push_back("AA");
        Super::__c4__collectNames(names);
    }
};

// Compose and test
using AA_Class = Compose_t<AA>;

bool runRuntimeTests() {
    AA_Class aa;
    std::vector<std::string> names;
    aa.__c4__collectNames(names);

    std::vector<std::string> expected = {
        "AA", "CC", "BB", "EE", "II", "DD", "FF", "HH", "GG"
    };

    return verifyMRO(names, expected, "StackOverflow (AA hierarchy)");
}

} // namespace stackoverflow

// =============================================================================
// Main Test Runner
// =============================================================================

int main() {
    std::cout << "C3 Linearization Tests (with Template Mixins)\n";
    std::cout << "==============================================\n\n";

    bool all_passed = true;

    all_passed &= wiki2021::runRuntimeTests();
    all_passed &= wiki2023::runRuntimeTests();
    all_passed &= boat::runRuntimeTests();
    all_passed &= stackoverflow::runRuntimeTests();

    std::cout << "\n";
    if (all_passed) {
        std::cout << "✓ All C3 tests passed!\n";
        return 0;
    } else {
        std::cout << "✗ Some tests failed\n";
        return 1;
    }
}
