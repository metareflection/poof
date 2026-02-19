#pragma once

// Mixin helper utilities (optional, not part of the core C4 API)
// Reference: Smaragdakis & Batory, "Mixin-Based Programming in C++" (2000)

#include <c4/c4.hpp>
#include <utility>

namespace c4 {

// Backward-compatibility alias
using EmptyBase = Mixin;

// ============================================================================
// CRTP Helper - Curiously Recurring Template Pattern
// ============================================================================

template <typename Derived>
struct CRTPBase {
    using derived_type = Derived;

    virtual ~CRTPBase() = default;

    virtual Derived& derived() {
        return static_cast<Derived&>(*this);
    }

    virtual const Derived& derived() const {
        return static_cast<const Derived&>(*this);
    }
};

// ============================================================================
// Compose2 - Compose two mixins into one
// ============================================================================

template <template<typename> class Mixin1, template<typename> class Mixin2>
struct Compose2 {
    template <typename Base>
    using type = Mixin1<Mixin2<Base>>;
};

// ============================================================================
// Constructor Forwarding Helper
// ============================================================================

// Mixins often need to forward constructors to their superclass.
#define C4_FORWARD_CONSTRUCTORS(MixinClass, SuperClass) \
    template <typename... Args> \
    MixinClass(Args&&... args) : SuperClass(std::forward<Args>(args)...) {}

// ============================================================================
// Method Override Helper (for explicit qualification)
// ============================================================================

// Per the S&B paper, it is best practice to explicitly qualify superclass
// method calls to avoid name lookup issues.
#define C4_SUPER_CALL(SuperClass, method, ...) \
    SuperClass::method(__VA_ARGS__)

} // namespace c4
