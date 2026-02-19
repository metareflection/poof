// Test cases for C4 suffix property
// Suffix specifications have special linearization requirements
// All mixins are templates that chain properly and implement collectNames

#include <c4/c4.hpp>
#include "../examples/mixin_names.hpp"
#include <type_traits>
#include <iostream>
#include <vector>
#include <string>

using namespace c4;
using namespace c4::meta;
using c4::examples::C4N;

// =============================================================================
// Test Helpers
// =============================================================================

bool verifyMRO(const std::vector<std::string>& actual,
               const std::vector<std::string>& expected,
               const std::string& testName) {
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
        std::cout << "✗ Size mismatch\n\n";
        return false;
    }

    for (size_t i = 0; i < actual.size(); ++i) {
        if (actual[i] != expected[i]) {
            std::cout << "✗ Mismatch at position " << i << "\n\n";
            return false;
        }
    }

    std::cout << "✓ Passed\n\n";
    return true;
}

// =============================================================================
// C4 Suffix Tests - Compatible Suffix Specifications
// =============================================================================

namespace suffix_compatible {

// Base suffix specs
template <typename Super>
struct SBA : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("SBA");
        Super::collectNames(names);
    }
};

template <typename Super>
struct SBB : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("SBB");
        Super::collectNames(names);
    }
};

// SBS inherits from SBA (both suffix)
template <typename Super>
struct SBS : public Super {
    using __c4__parents = TypeList<SpecList<SBA>>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("SBS");
        Super::collectNames(names);
    }
};

// sBs inherits from SBA (both suffix)
template <typename Super>
struct sBs : public Super {
    using __c4__parents = TypeList<SpecList<SBA>>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("sBs");
        Super::collectNames(names);
    }
};

// SBC inherits from SBS and SBB (suffix)
template <typename Super>
struct SBC : public Super {
    using __c4__parents = TypeList<SpecList<SBS, SBB>>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("SBC");
        Super::collectNames(names);
    }
};

// Compose and test
using SBC_Class = C4N<SBC>;

bool runTests() {
    SBC_Class sbc;
    std::vector<std::string> names;
    sbc.collectNames(names);

    std::vector<std::string> expected = {"SBC", "SBS", "SBA", "SBB"};
    return verifyMRO(names, expected, "Compatible suffix specs (SBC)");
}

} // namespace suffix_compatible

// =============================================================================
// Mixed Infix/Suffix Tests
// =============================================================================

namespace mixed_infix_suffix {

// O is a suffix spec (base)
template <typename Super>
struct O : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("O");
        Super::collectNames(names);
    }
};

// A is infix, inherits from suffix O
template <typename Super>
struct A : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("A");
        Super::collectNames(names);
    }
};

// B is infix, inherits from infix A (which has suffix O in MRO)
template <typename Super>
struct B : public Super {
    using __c4__parents = TypeList<SpecList<A>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("B");
        Super::collectNames(names);
    }
};

// Compose and test
using B_Class = C4N<B>;

bool runTests() {
    B_Class b;
    std::vector<std::string> names;
    b.collectNames(names);

    std::vector<std::string> expected = {"B", "A", "O"};
    return verifyMRO(names, expected, "Mixed infix/suffix (B)");
}

} // namespace mixed_infix_suffix

// =============================================================================
// Complex Suffix Chain
// =============================================================================

namespace complex_suffix {

// Linear suffix chain
template <typename Super>
struct S1 : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("S1");
        Super::collectNames(names);
    }
};

template <typename Super>
struct S2 : public Super {
    using __c4__parents = TypeList<SpecList<S1>>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("S2");
        Super::collectNames(names);
    }
};

template <typename Super>
struct S3 : public Super {
    using __c4__parents = TypeList<SpecList<S2>>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("S3");
        Super::collectNames(names);
    }
};

template <typename Super>
struct S4 : public Super {
    using __c4__parents = TypeList<SpecList<S3>>;
    static constexpr bool __c4__is_suffix = true;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("S4");
        Super::collectNames(names);
    }
};

// Infix spec inheriting from middle of chain
template <typename Super>
struct I : public Super {
    using __c4__parents = TypeList<SpecList<S2>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("I");
        Super::collectNames(names);
    }
};

// Compose and test
using S4_Class = C4N<S4>;
using I_Class = C4N<I>;

bool runTests() {
    bool passed = true;

    S4_Class s4;
    std::vector<std::string> s4_names;
    s4.collectNames(s4_names);
    std::vector<std::string> s4_expected = {"S4", "S3", "S2", "S1"};
    passed &= verifyMRO(s4_names, s4_expected, "Suffix chain (S4)");

    I_Class i;
    std::vector<std::string> i_names;
    i.collectNames(i_names);
    std::vector<std::string> i_expected = {"I", "S2", "S1"};
    passed &= verifyMRO(i_names, i_expected, "Infix from middle of chain (I)");

    return passed;
}

} // namespace complex_suffix

// =============================================================================
// Multiple Parent Groups Test
// =============================================================================
// Demonstrates TypeList<SpecList<A,B>, SpecList<C>> — two independent ordering
// groups.  Relative ordering between C and {A,B} is not locally constrained,
// so C3 may place C anywhere after its own parents are satisfied.
// The expected MRO here matches TypeList<SpecList<A,B,C>> because the only
// valid C3 linearisation of the three groups is A B C O.

namespace multiple_groups {

template <typename Super>
struct O : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("O");
        Super::collectNames(names);
    }
};

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

template <typename Super>
struct C : public Super {
    using __c4__parents = TypeList<SpecList<O>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("C");
        Super::collectNames(names);
    }
};

// Two independent ordering groups: [A, B] and [C].
// Equivalent semantics to TypeList<SpecList<A, B, C>> for this hierarchy,
// but the relative ordering between C and {A,B} is not locally constrained.
template <typename Super>
struct MultiGroup : public Super {
    using __c4__parents = TypeList<SpecList<A, B>, SpecList<C>>;
    static constexpr bool __c4__is_suffix = false;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("MultiGroup");
        Super::collectNames(names);
    }
};

using MultiGroup_Class = C4N<MultiGroup>;

bool runTests() {
    MultiGroup_Class mg;
    std::vector<std::string> names;
    mg.collectNames(names);

    // C3 merge of [A,B,O], [C,O], [A,B], [C] yields A B C O
    std::vector<std::string> expected = {"MultiGroup", "A", "B", "C", "O"};
    return verifyMRO(names, expected, "Multiple parent groups (MultiGroup)");
}

} // namespace multiple_groups

// =============================================================================
// Suffix Subtyping Property Test
// =============================================================================
// Demonstrates that C4<X> is a C++ subtype of C4<Z> whenever Z is a suffix
// ancestor of X.  This holds by construction: the mixin chain places suffix
// specs at the tail so every suffix ancestor becomes a literal base class of
// the composed type.

namespace suffix_subtyping {

// SBase: root suffix spec (no parents)
template <typename Super>
struct SBase : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = true;
};

// SChild: suffix spec that extends SBase
template <typename Super>
struct SChild : public Super {
    using __c4__parents = TypeList<SpecList<SBase>>;
    static constexpr bool __c4__is_suffix = true;
};

// InfixM: infix mixin whose suffix ancestor chain is SChild -> SBase
template <typename Super>
struct InfixM : public Super {
    using __c4__parents = TypeList<SpecList<SChild>>;
    static constexpr bool __c4__is_suffix = false;
};

// Compile-time subtyping checks:
// C4<InfixM> is composed as InfixM<SChild<SBase<Mixin>>>, so C4<SChild> and
// C4<SBase> are literal base classes of C4<InfixM>.
static_assert(std::is_base_of_v<C4<SBase>,  C4<InfixM>>, "C4<InfixM> should be subtype of C4<SBase>");
static_assert(std::is_base_of_v<C4<SChild>, C4<InfixM>>, "C4<InfixM> should be subtype of C4<SChild>");

bool test_suffix_subtyping() {
    C4<InfixM> obj;
    // Can upcast to suffix ancestor types
    C4<SChild>* as_schild = &obj;
    C4<SBase>*  as_sbase  = &obj;
    std::cout << "Suffix subtyping:\n";
    std::cout << "  C4<InfixM> IS-A C4<SChild>: " << (as_schild != nullptr ? "true" : "false") << "\n";
    std::cout << "  C4<InfixM> IS-A C4<SBase>:  " << (as_sbase  != nullptr ? "true" : "false") << "\n";
    std::cout << "  (static_asserts above already proved these at compile time)\n";
    return true;
}

} // namespace suffix_subtyping

// =============================================================================
// Main
// =============================================================================

int main() {
    std::cout << "C4 Suffix Property Tests (with Template Mixins)\n";
    std::cout << "================================================\n\n";

    bool all_passed = true;

    all_passed &= suffix_compatible::runTests();
    all_passed &= mixed_infix_suffix::runTests();
    all_passed &= complex_suffix::runTests();
    all_passed &= multiple_groups::runTests();

    all_passed &= suffix_subtyping::test_suffix_subtyping();
    std::cout << "Suffix subtyping: passed\n\n";

    if (all_passed) {
        std::cout << "All C4 suffix tests passed!\n";
        return 0;
    } else {
        std::cout << "Some tests failed\n";
        return 1;
    }
}
