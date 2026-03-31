#!/bin/sh -ex

# Check that the examples compile and run
for i in interface suffix diamond multiple_parent_lists ; do
  g++ -std=c++20 -Iinclude examples/${i}.cpp   -o build/${i}   && build/${i} || return 1
done

# Check that the tests pass
for i in test_c3_examples test_c4_suffix test_error_detection test_spec_pattern ; do
  g++ -std=c++20 -Iinclude tests/${i}.cpp    -o build/${i}    && build/${i} || return 1
done
