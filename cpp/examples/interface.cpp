// Interface example — modular programming against an interface with C4.
//
// A spec may inherit from Interface<Self> or a plain Interface alongside Super.
// The Interface has no <Super> template parameter: it defines only the virtual
// members that clients need, independently of the mixin chain.
//
// Two patterns:
//   1. Interface<Self>   — parameterized by the concrete class, enabling
//                          covariant return types (clone, factory, etc.)
//   2. Interface         — plain abstract base, for straightforward virtual APIs
//
// Clients program against Interface* or Interface<ConcreteType>*, completely
// independently of the mixin hierarchy.
//
// Build:
//   g++ -std=c++20 -Iinclude examples/interface.cpp -o build/interface && build/interface

#include "mixin_names.hpp"
#include <iostream>
#include <memory>
#include <string>
#include <vector>

using namespace c4;
using c4::examples::C4N;

// ── Pattern 1: Interface<Self> ────────────────────────────────────────────────
// The interface is parameterized by the concrete class.
// This lets it declare methods that return or accept the concrete type,
// something impossible with a plain unparameterized interface.

template <typename Self>
struct Cloneable {
    virtual std::unique_ptr<Self> clone() const = 0;
    virtual ~Cloneable() = default;
};

// A spec that satisfies Cloneable<Self>.
// It inherits from Cloneable<Self> and Super; Self is the final composed class.
template <typename Self, typename Super>
struct CloneableMixin : public Cloneable<Self>, public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;

    std::unique_ptr<Self> clone() const override {
        return std::make_unique<Self>(static_cast<const Self&>(*this));
    }

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("CloneableMixin");
        Super::collectNames(names);
    }
};

// ── Pattern 2: Plain Interface (no Self) ─────────────────────────────────────
// A conventional abstract base class. Clients only need to #include this
// interface; they are fully decoupled from the mixin implementation.

struct Serializable {
    virtual std::string serialize() const = 0;
    virtual ~Serializable() = default;
};

// A spec that satisfies Serializable.
template <typename Self, typename Super>
struct SerializableMixin : public Serializable, public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;

    std::string tag = "node";

    std::string serialize() const override {
        return "{\"type\":\"" + tag + "\"}";
    }

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("SerializableMixin");
        Super::collectNames(names);
    }
};

// ── Combining both interfaces in one class ────────────────────────────────────

template <typename Self, typename Super>
struct Node : public Super {
    using __c4__parents = TypeList<SpecList<CloneableMixin, SerializableMixin>>;
    static constexpr bool __c4__is_suffix = false;

    int value = 0;

    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Node");
        Super::collectNames(names);
    }
};

using NodeClass = C4N<Node>;

// NodeClass is-a Cloneable<NodeClass> and Serializable
static_assert(std::is_base_of_v<Cloneable<NodeClass>, NodeClass>);
static_assert(std::is_base_of_v<Serializable,         NodeClass>);

int main() {
    NodeClass n;
    n.value = 42;
    n.tag   = "answer";

    // ── Collect CPL ───────────────────────────────────────────────────────────
    std::vector<std::string> names;
    n.collectNames(names);
    std::cout << "CPL: ";
    for (const auto& name : names) std::cout << name << " ";
    std::cout << "\n";
    // CPL: Node CloneableMixin SerializableMixin

    // ── Use via Serializable* — client knows nothing about mixins ─────────────
    Serializable* s = &n;
    std::cout << "serialize(): " << s->serialize() << "\n";

    // ── Use via Cloneable<NodeClass>* — covariant clone ───────────────────────
    Cloneable<NodeClass>* c = &n;
    std::unique_ptr<NodeClass> copy = c->clone();
    copy->tag = "copy";
    std::cout << "clone serialize(): " << copy->serialize() << "\n";

    return 0;
}
