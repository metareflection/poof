// Suffix specification example — demonstrates the C4 suffix property.
//
// Suffix specs (__c4__is_suffix = true) are always placed at the end of any
// descendant's CPL, enabling fixed-offset field layout and single-inheritance-
// style optimizations across the whole class hierarchy.
//
// Key property: C4<X> is a C++ subtype of C4<Z> whenever Z is a suffix
// ancestor of X — safe upcasting without virtual bases.
//
// Build:
//   g++ -std=c++20 -I../include suffix.cpp -o suffix && ./suffix

#include "mixin_names.hpp"
#include <iostream>
#include <string>
#include <vector>
#include <type_traits>

using namespace c4;
using c4::examples::C4N;

// ── Suffix specs (stable "base layer", always at the tail of any CPL) ────────

// Universal root — no parents.
template <typename Super>
struct Object : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = true;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Object"); Super::collectNames(names);
    }
};

// Named: extends Object, adds a name field.
template <typename Super>
struct Named : public Super {
    using __c4__parents = TypeList<SpecList<Object>>;
    static constexpr bool __c4__is_suffix = true;
    std::string name;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Named"); Super::collectNames(names);
    }
};

// ── Infix specs (domain logic; suffix ancestors stay at the tail) ─────────────

// Printable: infix mixin that uses the name field from the suffix ancestor Named.
template <typename Super>
struct Printable : public Super {
    using __c4__parents = TypeList<SpecList<Named>>;
    static constexpr bool __c4__is_suffix = false;
    void print() const { std::cout << "Printable: " << this->name << "\n"; }
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Printable"); Super::collectNames(names);
    }
};

// Loggable: infix mixin, also uses Named.
template <typename Super>
struct Loggable : public Super {
    using __c4__parents = TypeList<SpecList<Named>>;
    static constexpr bool __c4__is_suffix = false;
    void log(const std::string& msg) const {
        std::cout << "[" << this->name << "] " << msg << "\n";
    }
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Loggable"); Super::collectNames(names);
    }
};

// Service: combines Printable and Loggable.
// CPL: [Service, Printable, Loggable, Named, Object]
// Named and Object stay at the end because they are suffix specs.
template <typename Super>
struct Service : public Super {
    using __c4__parents = TypeList<SpecList<Printable, Loggable>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Service"); Super::collectNames(names);
    }
};

using Service_Class = C4N<Service>;

// ── Suffix subtyping checks (compile-time) ────────────────────────────────────
// Suffix ancestors appear as literal base classes in the mixin chain, so
// C4<X> is-a C4<Z> for any suffix ancestor Z — no virtual bases needed.

static_assert(std::is_base_of_v<C4<Named>,    C4<Service>>,   "C4<Service> is-a C4<Named>");
static_assert(std::is_base_of_v<C4<Object>,   C4<Service>>,   "C4<Service> is-a C4<Object>");
static_assert(std::is_base_of_v<C4<Named>,    C4<Printable>>, "C4<Printable> is-a C4<Named>");
static_assert(std::is_base_of_v<C4<Named>,    C4<Loggable>>,  "C4<Loggable> is-a C4<Named>");
static_assert(std::is_base_of_v<C4<Object>,   C4<Named>>,     "C4<Named> is-a C4<Object>");

int main() {
    Service_Class svc;
    svc.name = "my-service";

    std::vector<std::string> names;
    svc.collectNames(names);

    std::cout << "CPL: ";
    for (const auto& n : names) std::cout << n << " ";
    std::cout << "\n";
    // CPL: Service Printable Loggable Named Object
    // Named and Object are always at the end — they are suffix specs.

    svc.print();
    svc.log("started");

    // Safe upcast to suffix ancestor types.
    // Service_Class = C4N<Service> = C4<Service, MixinNames>, so upcast targets
    // must use the same base — C4N<Named> = C4<Named, MixinNames>.
    C4N<Named>*  as_named  = &svc;
    C4N<Object>* as_object = &svc;
    std::cout << "Upcast to C4N<Named>:  " << (as_named  ? "ok" : "fail") << "\n";
    std::cout << "Upcast to C4N<Object>: " << (as_object ? "ok" : "fail") << "\n";

    return 0;
}
