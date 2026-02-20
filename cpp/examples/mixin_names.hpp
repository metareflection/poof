#pragma once

// MixinNames: a base that provides the collectNames traversal protocol.
//
// Use C4N<Spec> instead of C4<Spec> when your specs implement collectNames
// for runtime CPL introspection.  This keeps the collectNames protocol
// entirely out of the core library.

#include <c4/c4.hpp>
#include <vector>
#include <string>

namespace c4 {
namespace examples {

// Root base providing the collectNames protocol.
// Each mixin in the chain calls Super::collectNames then adds its own name.
struct MixinNames : public Mixin {
    virtual void collectNames(std::vector<std::string>& names) const {}
};

// C4N<Spec>: compose Spec with MixinNames as the chain root.
template <template<typename> class Spec>
using C4N = C4<Spec, MixinNames>;

} // namespace examples
} // namespace c4
