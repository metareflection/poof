# C4: Optimal Inheritance in C++

A C++20 implementation of the C4 linearization algorithm, enabling "optimal inheritance" —
combining the expressiveness of flavorful multiple inheritance
with the performance of single inheritance.

## Overview

This project implements **Optimal Inheritance** using C++ template metaprogramming, based on:

1. **Flavorful Multiple Inheritance**:
   Multiple parent methods are not lose-lose conflict, but win-win cooperation.
   They can each call the next along a linearized class precedence list. No information loss.
2. **Performance of Single Inheritance**:
   Classes that declare "static constexpr bool __c4__is_suffix = true;" are *suffix classes*,
   whose class precedence list is guaranteed to be the suffix of that of any descendent,
   enabling all the usual optimizations of single inheritance (e.g. fixed-offset fields, etc.)
3. **Linearization Consistency**: respect user-provided local precedence order
   (a DAG specified as zero, one or more lists of mixins),
   monotonicity of precedence lists, ensuring consistency of behavior across methods and across
   subclasses, so that, e.g. you could have multiple-inheritance methods handle locking
   or memory allocation for you without risking deadlocks or use-after-free.
4. **Compile-time resolution of inheritance**:
   using C++ templates, we ensure that all inheritance computations happen at compile-time;
   there is zero runtime overhead to using this library.

## Project Status

It works. Tests pass. We're still working on making it simpler, better and more usable.

## Building and Testing

```bash
cd poof/cpp
g++ -std=c++20 -Iinclude tests/test_spec_pattern.cpp   -o build/test_spec_pattern   && build/test_spec_pattern
g++ -std=c++20 -Iinclude tests/test_c3_examples.cpp    -o build/test_c3_examples    && build/test_c3_examples
g++ -std=c++20 -Iinclude tests/test_c4_suffix.cpp      -o build/test_c4_suffix      && build/test_c4_suffix
g++ -std=c++20 -Iinclude tests/test_error_detection.cpp -o build/test_error_detection && build/test_error_detection
```

## Example Usage

`collectNames` is not part of the core library — it lives in `examples/mixin_names.hpp`,
which provides `MixinNames` (a base with the `collectNames` protocol) and `C4N<Spec>`
(shorthand for `C4<Spec, MixinNames>`).

```cpp
#include "mixin_names.hpp"   // provides MixinNames, C4N
using namespace c4;
using c4::examples::C4N;

// Base spec - no parents
template <typename Super>
struct O : public Super {
    using __c4__parents = TypeList<>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("O"); Super::collectNames(names);
    }
};

// A and B each inherit from O
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

// Diamond inherits from both A and B
template <typename Super>
struct Diamond : public Super {
    using __c4__parents = TypeList<SpecList<A, B>>;
    static constexpr bool __c4__is_suffix = false;
    void collectNames(std::vector<std::string>& names) const {
        names.push_back("Diamond"); Super::collectNames(names);
    }
};

// Compose Diamond; CPL computed at compile time: [Diamond, A, B, O]
using Diamond_Class = C4N<Diamond>;

// Compile-time CPL membership checks (no collectNames needed)
static_assert(IsInCPL_v<Diamond, A>);
static_assert(IsInCPL_v<Diamond, B>);
static_assert(IsInCPL_v<Diamond, O>);

int main() {
    Diamond_Class d;
    std::vector<std::string> names;
    d.collectNames(names);
    // names == {"Diamond", "A", "B", "O"}
    // O appears once despite being a shared ancestor — C4 handles it.
}
```

“Suffix property”: suffix specs (marked `__c4__is_suffix = true`) are guaranteed to have
their class precedence list as the suffix of any descendent’s class precedence list,
enabling fixed-offset field access. Suffix specs in a given class’s ancestry
are always in a total order, though some classes in between them might not.

## Examples

All runnable examples live in `examples/` and build with:

```bash
cd poof/cpp
g++ -std=c++20 -Iinclude examples/<name>.cpp -o build/<name> && build/<name>
```

| File | Demonstrates |
|------|-------------|
| `examples/diamond.cpp` | Basic diamond inheritance; canonical C4 usage with `C4N<>` |
| `examples/suffix.cpp` | Suffix specs (`__c4__is_suffix = true`), fixed-tail CPL placement, and suffix subtyping: `C4<X>` is-a `C4<Y>` for every suffix ancestor `Y` |
| `examples/multiple_parent_lists.cpp` | Multiple independent parent lists: `TypeList<SpecList<A,B>, SpecList<C>>` |
| `examples/mixin_names.hpp` | `MixinNames` base and `C4N<Spec>` alias — used by examples and tests that need `collectNames` |
| `examples/counting.hpp` | `Counting` mixin tracking nodes/edges visited; illustrates stateful mixins |

## Architecture

```
include/c4/
├── c4.hpp            # Main header (include this)
├── type_list.hpp     # Compile-time list operations
├── type_map.hpp      # Compile-time associative map (ancestor counting)
├── dag.hpp           # Cycle detection
└── c4_linearize.hpp  # C4 algorithm (included by c4.hpp)
```

### Type-Level Infrastructure

**TypeList** - Compile-time list with operations:
- Basic: Head, Tail, Cons, Append, Concat
- Transform: Map, Reverse
- Query: Contains
- Special: RemoveNulls, AppendReverseUntil (for C4), FoldLeft

**TypeMap** - Compile-time associative map:
- Get, Insert, Increment, Decrement
- Used for O(dn) ancestor counting in C3/C4

**SpecHelper** - Internal wrapper for a spec in the inheritance DAG:
```cpp
template <template<typename> class Spec, typename ParentsTypeList,
          bool IsSuffix, size_t UniqueId>
struct SpecHelper { /* ... */ };
```

## The C4 Algorithm

C4 extends C3 with support for **suffix specifications**. It enforces five constraints:

1. **Linearization**: Total order extending the DAG partial order
2. **Local Order**: Parent order in definitions preserved in precedence list
3. **Monotonicity**: Parent's precedence list is subsequence of child's
4. **Shape Determinism**: Isomorphic DAGs yield isomorphic precedence lists
5. **Suffix Property** (C4): Suffix spec's precedence list is suffix to all descendents' precedence lists.

### Complexity

- **Optimized C4**: O(dn) using hash-table ancestor counting
  (modulo our map implementation not being a hash-table but a linear map in C++,
  which in practice makes it O(dn²)). Contrast with the original C3 being O(d²n²).

## Contributors

Code largely coded by Claude Opus 4.5 (Anthropic) as guided by François-René Rideau.

## Bibliography

**Yannis Smaragdakis and Don Batory**. "Mixin-based programming in C++". 2000. In Proc. International Symposium on Generative and Component-Based Software Engineering, pp. 164–178. doi:10.1007/3-540-44815-2_12 . Explains the basic approach to implementing Mixin inheritance on top of C++ templates.

**François-René Rideau**. "Lambda, the Ultimate Object". 2026. (Not yet published) http://fare.tunes.org/files/cs/poof/ltuo.html . Includes a complete theory of OO. The C4 algorithm is explained in chapter 7.

**François-René Rideau**. "Gerbil Scheme C4 implementation". 2025.
Latest copy in branch c3-doc. Source files src/gerbil/runtime/c3.ss,
tests in src/gerbil/test/c3-test.ss.
