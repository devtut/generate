---
metaTitle: "Objective C - Random Integer"
description: "Basic Random Integer, Random Integer within a Range"
---

# Random Integer



## Basic Random Integer


The `arc4random_uniform()` function is the simplest way to get high-quality random integers. As per the manual:

> 
arc4random_uniform(upper_bound) will return a uniformly distributed random number less than upper_bound.
arc4random_uniform() is recommended over constructions like ''arc4random() % upper_bound'' as it avoids "modulo bias" when the upper bound is not a power of two.


```objectivec
uint32_t randomInteger = arc4random_uniform(5); // A random integer between 0 and 4

```



## Random Integer within a Range


The following code demonstrates usage of `arc4random_uniform()` to generate a random integer between 3 and 12:

```objectivec
uint32_t randomIntegerWithinRange = arc4random_uniform(10) + 3; // A random integer between 3 and 12

```

This works to create a range because `arc4random_uniform(10)` returns an integer between 0 and 9. Adding 3 to this random integer produces a range between `0 + 3` and `9 + 3`.

