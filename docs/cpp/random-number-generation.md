---
metaTitle: "C++ | Random number generation"
description: "True random value generator, Generating a pseudo-random number, Using the generator for multiple distributions"
---

# Random number generation



## True random value generator


To generate true random values that can be used for cryptography `std::random_device` has to be used as generator.

```cpp
#include <iostream>
#include <random>

int main()
{
   std::random_device crypto_random_generator;
   std::uniform_int_distribution<int> int_distribution(0,9);
   
   int actual_distribution[10] = {0,0,0,0,0,0,0,0,0,0};
   
   for(int i = 0; i < 10000; i++) {
       int result = int_distribution(crypto_random_generator);
       actual_distribution[result]++;
   }

   for(int i = 0; i < 10; i++) {
       std::cout << actual_distribution[i] << " ";
   }
   
   return 0;
}

```

`std::random_device` is used in the same way as a pseudo random value generator is used.

However `std::random_device` **may be implemented in terms of an implementation-defined pseudo-random number engine** if a non-deterministic source (e.g. a hardware device) isn't available to the implementation.

Detecting such implementations should be possible via the [`entropy` member function](http://en.cppreference.com/w/cpp/numeric/random/random_device/entropy) (which return zero when the generator is completely deterministic), but many popular libraries (both GCC's libstdc++ and LLVM's libc++) always return zero, even when they're using high-quality external randomness.



## Generating a pseudo-random number


A pseudo-random number generator generates values that can be guessed based on previously generated values. In other words: it is deterministic. Do not use a pseudo-random number generator in situations where a true random number is required.

```cpp
#include <iostream>
#include <random>

int main()
{
   std::default_random_engine pseudo_random_generator;
   std::uniform_int_distribution<int> int_distribution(0, 9);
   
   int actual_distribution[10] = {0,0,0,0,0,0,0,0,0,0};
   
   for(int i = 0; i < 10000; i++) {
       int result = int_distribution(pseudo_random_generator);
       actual_distribution[result]++;
   }

   for(int i = 0; i <= 9; i++) {
       std::cout << actual_distribution[i] << " ";
   }
   
   return 0;
}

```

This code creates a random number generator, and a distribution that generates integers in the range [0,9] with equal likelihood. It then counts how many times each result was generated.

The template parameter of [`std::uniform_int_distribution<T>`](http://en.cppreference.com/w/cpp/numeric/random/uniform_int_distribution) specifies the type of integer that should be generated. Use [`std::uniform_real_distribution<T>`](http://en.cppreference.com/w/cpp/numeric/random/uniform_real_distribution) to generate floats or doubles.



## Using the generator for multiple distributions


The random number generator can (and should) be used for multiple distributions.

```cpp
#include <iostream>
#include <random>

int main()
{
   std::default_random_engine pseudo_random_generator;
   std::uniform_int_distribution<int> int_distribution(0, 9);
   std::uniform_real_distribution<float> float_distribution(0.0, 1.0);
   std::discrete_distribution<int> rigged_dice({1,1,1,1,1,100});
   
   std::cout << int_distribution(pseudo_random_generator) << std::endl;
   std::cout << float_distribution(pseudo_random_generator) << std::endl;
   std::cout << (rigged_dice(pseudo_random_generator) + 1) << std::endl;
   
   return 0;
}

```

In this example, only one generator is defined. It is subsequently used to generate a random value in three different distributions. The `rigged_dice` distribution will generate a value between 0 and 5, but almost always generates a `5`, because the chance to generate a `5` is `100 / 105`.



#### Remarks


Random number generation in C++ is provided by the `<random>` header. This header defines random devices, pseudo-random generators and distributions.

Random devices return random numbers provided by operating system. They should either be used for initialization of pseudo-random generators or directly for cryptographic purposes.

Pseudo-random generators return integer pseudo-random numbers based on their initial seed. The pseudo-random number range typically spans all values of an unsigned type. All pseudo-random generators in the standard library will return the same numbers for the same initial seed for all platforms.

Distributions consume random numbers from pseudo-random generators or random devices and produce random numbers with necessary distribution. Distributions are not platform-independent and can produce different numbers for the same generators with the same initial seeds on different platforms.

