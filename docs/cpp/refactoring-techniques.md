---
metaTitle: "C++ | Refactoring Techniques"
description: "Goto Cleanup, Refactoring walk through"
---

# Refactoring Techniques


**Refactoring** refers to the modification of existing code into an improved version.  Although refactoring is often done while changing code to add features or fix bugs, the term particularly refers improving code without necessarily adding features or fixing bugs.



## Goto Cleanup


In C++ code bases which used to be C, one can find the pattern `goto cleanup`. As the `goto` command makes the workflow of a function harder to understand, this is often avoided. Often, it can be replaced by return statements, loops, functions. Though, with the `goto cleanup` one needs to get rid of cleanup logic.

```cpp
short calculate(VectorStr **data) {
    short result = FALSE;
    VectorStr *vec = NULL;
    if (!data)
       goto cleanup;  //< Could become return false

    // ... Calculation which 'new's VectorStr

    result = TRUE;
cleanup:
    delete [] vec;
    return result;
}

```

In C++ one could use [RAII](https://stackoverflow.com/documentation/c%2b%2b/1320/raii-resource-acquisition-is-initialization) to fix this issue:

```cpp
struct VectorRAII final {
    VectorStr *data{nullptr};
    VectorRAII() = default;
    ~VectorRAII() {
        delete [] data;
    }
    VectorRAII(const VectorRAII &) = delete;
};

short calculate(VectorStr **data) {
    VectorRAII vec{};
    if (!data)
       return FALSE;  //< Could become return false

    // ... Calculation which 'new's VectorStr and stores it in vec.data

    return TRUE;
}

```

From this point on, one could continue refactoring the actual code. For example by replacing the `VectorRAII` by `std::unique_ptr` or `std::vector`.



## Refactoring walk through


Here's a program which might benefit from refactoring.  It's a simple program using C++11 which is intended to calculate and print all prime numbers from 1 to 100 and is based on a program that was posted on [CodeReview](https://codereview.stackexchange.com/) for review.

```cpp
#include <iostream>
#include <vector>
#include <cmath>

int main()
{
    int l = 100;
    bool isprime;
    std::vector<int> primes;
    primes.push_back(2);
    for (int no = 3; no < l; no += 2) {
        isprime = true;
        for (int primecount=0; primes[primecount] <= std::sqrt(no); ++primecount) {
            if (no % primes[primecount] == 0) {
                isprime = false;
                break;
            } else if (primes[primecount] * primes[primecount] > no) {
                std::cout << no << "\n";
                break;
            }
        }
        if (isprime) {
            std::cout << no << " ";
            primes.push_back(no);
        }
    }
    std::cout << "\n";
}

```

The output from this program looks like this:

> 
3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97


The first thing we notice is that the program fails to print `2` which is a prime number.  We could simply add a line of code to simply print that one constant without modifying the rest of the program, but it might be neater to **refactor** the program to split it into two parts - one that creates the prime number list an another that prints them.  Here's how that might look:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

std::vector<int> prime_list(int limit)
{
    bool isprime;
    std::vector<int> primes;
    primes.push_back(2);
    for (int no = 3; no < limit; no += 2) {
        isprime = true;
        for (int primecount=0; primes[primecount] <= std::sqrt(no); ++primecount) {
            if (no % primes[primecount] == 0) {
                isprime = false;
                break;
            } else if (primes[primecount] * primes[primecount] > no) {
                break;
            }
        }
        if (isprime) {
            primes.push_back(no);
        }
    }
    return primes;
}

int main() 
{
    std::vector<int> primes = prime_list(100);
    for (std::size_t i = 0; i < primes.size(); ++i) {
        std::cout << primes[i] << ' ';
    }
    std::cout << '\n';
}

```

Trying this version, we see that it does indeed work correctly now:

> 
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97


The next step is to notice that the second `if` clause is not really needed.  The logic in the loop looks for prime factors of each given number up to the square root of that number.  This works because if there are any prime factors of a number at least one of them must be less than or equal to the square root of that number.  Reworking just that function (the rest of the program remains the same) we get this result:

```cpp
std::vector<int> prime_list(int limit)
{
    bool isprime;
    std::vector<int> primes;
    primes.push_back(2);
    for (int no = 3; no < limit; no += 2) {
        isprime = true;
        for (int primecount=0; primes[primecount] <= std::sqrt(no); ++primecount) {
            if (no % primes[primecount] == 0) {
                isprime = false;
                break;
            }
        }
        if (isprime) {
            primes.push_back(no);
        }
    }
    return primes;
}

```

We can go further, changing variable names to be a bit more descriptive.  For example `primecount` isn't really a count of primes.  Instead it's an index variable into the vector of known primes.  Also, while `no` is sometimes used as an abbreviation for "number", in mathematical writing, it's more common to use `n`.  We can also make some modifications by eliminating the `break`, and by declaring variables closer to where they're used.

```cpp
std::vector<int> prime_list(int limit)
{
    std::vector<int> primes{2};
    for (int n = 3; n < limit; n += 2) {
        bool isprime = true;
        for (int i=0; isprime && primes[i] <= std::sqrt(n); ++i) {
            isprime &= (n % primes[i] != 0);
        }
        if (isprime) {
            primes.push_back(n);
        }
    }
    return primes;
}

```

We can also refactor `main` to use a "range-for" to make it a bit neater:

```cpp
int main() 
{
    std::vector<int> primes = prime_list(100);
    for (auto p : primes) {
        std::cout << p << ' ';
    }
    std::cout << '\n';
}

```

This is just one way refactoring might be done.  Others might make different choices.  However, the purpose for refactoring remains the same, which is to improve the readability and possibly the performance of the code without necessarily adding features.

