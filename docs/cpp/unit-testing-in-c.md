---
metaTitle: "Unit Testing in C++"
description: "Google Test, Catch"
---

# Unit Testing in C++


Unit testing is a level in software testing that validates the behavior and correctness of units of code.

In C++, "units of code" often refer to either classes, functions, or groups of either. Unit testing is often performed using specialized "testing frameworks" or "testing libraries" that often use non-trivial syntax or usage patterns.

This topic will review different strategies and unit testing libraries or frameworks.



## Google Test


[Google Test is a C++ testing framework maintained by Google.](https://github.com/google/googletest) It requires building the `gtest` library and linking it to your testing framework when building a test case file.

### Minimal Example

```cpp
// main.cpp

#include <gtest/gtest.h>
#include <iostream>

// Google Test test cases are created using a C++ preprocessor macro
// Here, a "test suite" name and a specific "test name" are provided.
TEST(module_name, test_name) {
    std::cout << "Hello world!" << std::endl;
    // Google Test will also provide macros for assertions.
    ASSERT_EQ(1+1, 2);
}

// Google Test can be run manually from the main() function
// or, it can be linked to the gtest_main library for an already
// set-up main() function primed to accept Google Test test cases.
int main(int argc, char** argv) {
    ::testing::InitGoogleTest(&argc, argv);

    return RUN_ALL_TESTS();
}

// Build command: g++ main.cpp -lgtest

```



## Catch


[Catch](https://github.com/philsquared/Catch) is a header only library that allows you to use both [TDD](https://en.wikipedia.org/wiki/Test-driven_development) and [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) unit test style.

The following snippet is from the Catch documentation page at [this link](https://github.com/philsquared/Catch/blob/master/docs/tutorial.md):

```


SCENARIO( "vectors can be sized and resized", "[vector]" ) {
    GIVEN( "A vector with some items" ) {
        std::vector v( 5 );
        
        REQUIRE( v.size() == 5 );
        REQUIRE( v.capacity() >= 5 );
        
        WHEN( "the size is increased" ) {
            v.resize( 10 );
            
            THEN( "the size and capacity change" ) {
                REQUIRE( v.size() == 10 );
                REQUIRE( v.capacity() >= 10 );
            }
        }
        WHEN( "the size is reduced" ) {
            v.resize( 0 );
            
            THEN( "the size changes but not capacity" ) {
                REQUIRE( v.size() == 0 );
                REQUIRE( v.capacity() >= 5 );
            }
        }
        WHEN( "more capacity is reserved" ) {
            v.reserve( 10 );
            
            THEN( "the capacity changes but not the size" ) {
                REQUIRE( v.size() == 5 );
                REQUIRE( v.capacity() >= 10 );
            }
        }
        WHEN( "less capacity is reserved" ) {
            v.reserve( 0 );
            
            THEN( "neither size nor capacity are changed" ) {
                REQUIRE( v.size() == 5 );
                REQUIRE( v.capacity() >= 5 );
            }
        }
    }
}

```

Conveniently, these tests will be reported as follows when run:

