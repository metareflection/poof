# C4: Flavorful Multiple Inheritance in C++

A C++20 implementation of the C4 linearization algorithm, enabling "optimal inheritance" - combining the expressiveness of multiple inheritance with the performance of single inheritance.

## Overview

This project implements **flavorful multiple inheritance** using C++ template metaprogramming, based on:

1. **Mixin-based programming** patterns from Smaragdakis & Batory (2000)
2. **C4 linearization algorithm** - an extension of C3 with suffix property support
3. **Compile-time DAG resolution** - zero runtime overhead

### Key Innovation

C4 allows mixing **suffix specifications** (like Lisp structs) with **infix specifications** in the same inheritance hierarchy:

- **Suffix specs** get single-inheritance performance (fixed-offset field access, 10-100x faster)
- **Infix specs** provide full multiple-inheritance flexibility
- **Both** can inherit from each other in the same DAG

## Project Status

✅ **Phase 1 Complete** - Foundation implemented and tested
- Type-level infrastructure (TypeList, TypeMap, utils)
- Specification representation
- Basic mixin patterns
- All infrastructure tests passing

🚧 **Phase 2 In Progress** - DAG and ancestry computation
- Implementing transitive closure computation
- Cycle detection
- Precedence list infrastructure

📋 **Remaining**:
- Phase 3: C3 merge algorithm
- Phase 4: C4 extensions (suffix support)
- Phase 5: High-level API and examples

## Building and Testing

```bash
cd /workspace/poof/cpp
g++ -std=c++20 -I. tests/test_basic_infrastructure.cpp -o tests/test_infrastructure
./tests/test_infrastructure
```

Expected output:
```
=== C4 Infrastructure Tests ===

Testing TypeList operations...
  ✓ All TypeList tests passed!
Testing TypeMap operations...
  ✓ All TypeMap tests passed!
Testing Specification...
  ✓ All Specification tests passed!
Testing basic mixin usage...
  ✓ Basic mixin usage tests passed!

=== All Infrastructure Tests Passed! ===
```

## Architecture

### Core Components

```
include/c4/
├── meta/                    # Compile-time metaprogramming
│   ├── type_list.hpp       # Fundamental type-level list operations
│   ├── type_map.hpp        # Type-to-value associative map
│   └── utils.hpp           # Utility metafunctions
├── core/                    # Core abstractions
│   ├── specification.hpp   # Specification template
│   └── mixin.hpp           # Basic mixin patterns
└── algorithm/               # C4 linearization (coming soon)
    ├── c4_linearize.hpp    # Main C4 algorithm
    ├── c3_merge.hpp        # C3 merge with O(dn) optimization
    └── merge_suffix.hpp    # Suffix merging logic
```

### Type-Level Infrastructure

**TypeList** - Compile-time list with operations:
- Basic: Head, Tail, Cons, Append, Concat
- Transform: Map, Filter, Reverse
- Query: Contains, IndexOf, At
- Special: RemoveNulls, AppendReverseUntil (for C4)

**TypeMap** - Compile-time associative map:
- Get, Insert, Increment, Decrement
- Used for O(dn) ancestor counting in C3/C4

**Specification** - Represents a class in the inheritance DAG:
```cpp
template <typename Mixin, typename Parents, bool IsSuffix, size_t UniqueId>
struct Specification { /* ... */ };

// Convenience aliases
template <typename M, typename P>
using SuffixSpec = Specification<M, P, true, __COUNTER__>;

template <typename M, typename P>
using InfixSpec = Specification<M, P, false, __COUNTER__>;
```

## Example Usage (Planned)

```cpp
#include <c4/c4.hpp>

// Define base
struct Point {
    double x, y;
};

// Define mixins
template <typename Super>
struct Colored : public Super {
    std::string color;
};

template <typename Super>
struct Named : public Super {
    std::string name;
};

// Create specifications
using PointSpec = c4::SuffixSpec<Point, c4::TypeList<>>;
using ColoredPointSpec = c4::InfixSpec<Colored, c4::TypeList<PointSpec>>;
using NamedColoredPointSpec = c4::InfixSpec<Named, c4::TypeList<ColoredPointSpec>>;

// Compose with automatic C4 linearization
using MyPoint = c4::Compose<NamedColoredPointSpec>;

// Use
MyPoint p;
p.x = 1.0;          // Fast fixed-offset access (suffix property)
p.y = 2.0;          // Fast fixed-offset access (suffix property)
p.color = "red";    // Color from infix mixin
p.name = "origin";  // Name from infix mixin
```

## The C4 Algorithm

C4 extends C3 with support for **suffix specifications**. It enforces five constraints:

1. **Linearization**: Total order extending the DAG partial order
2. **Local Order**: Parent order in definitions preserved in precedence list
3. **Monotonicity**: Parent's precedence list is subsequence of child's
4. **Shape Determinism**: Isomorphic DAGs yield isomorphic precedence lists
5. **Suffix Property** (C4): Suffix spec's precedence list is suffix of all descendants

### Algorithm Steps

1. Extract parent precedence lists
2. **Split** each into prefix (infix specs) and suffix (suffix specs)
3. **Merge** suffix lists (must form total order)
4. Append local order to candidate lists
5. **Clean** redundant suffix elements
6. **C3 merge** on cleaned prefixes (O(dn) with ancestor counting)
7. **Join** prefix and suffix

### Complexity

- **Naive C3**: O(d²n²) where d = parents, n = ancestors
- **Optimized C4**: O(dn) using hash-table ancestor counting

## References

1. **Smaragdakis & Batory (2000)**: "Mixin-Based Programming in C++"
   - `/workspace/2000__Smaragdakis_Batory__Mixin-based_programming_in_C++.pdf`

2. **C4 Algorithm Description**: `/workspace/poof/ltuo_07_inheritance_mixin_single_multiple_or_optimal.scrbl`
   - Section 7.4: C4 algorithm (lines 1445-1656)

3. **Reference Implementation**: `/workspace/gerbil/src/gerbil/runtime/c3.ss`
   - Working C4 in Scheme (256 lines)

4. **Test Cases**: `/workspace/gerbil/src/gerbil/test/c3-test.ss`
   - Comprehensive test hierarchies

## Implementation Plan

Full implementation plan available at: `/workspace/.claude/plans/pure-greeting-micali.md`

**Timeline**: 12 days (estimated)
**Lines of Code**: ~3700 total
- Core implementation: ~1500 lines
- Tests: ~800 lines
- Examples: ~400 lines
- Documentation: ~1000 lines

## Current Files

### Implemented (Phase 1 ✅)
- `include/c4/meta/type_list.hpp` (400+ lines)
- `include/c4/meta/type_map.hpp` (200+ lines)
- `include/c4/meta/utils.hpp` (100+ lines)
- `include/c4/core/specification.hpp` (140+ lines)
- `include/c4/core/mixin.hpp` (120+ lines)
- `tests/test_basic_infrastructure.cpp` (170+ lines)

### In Progress (Phase 2 🚧)
- `include/c4/meta/dag.hpp` - Next up

### Planned (Phases 3-5 📋)
- C3/C4 algorithm implementation
- Test suite (Wikipedia, boat, grid examples)
- High-level Compose API
- Comprehensive examples
- API documentation

## License

Part of the Proof of Optimal Ownership Foundation (poof) project.

## Contributors

Implementation following the plan by Claude (Anthropic).
