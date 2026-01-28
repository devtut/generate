---
metaTitle: "Ruby - Generate a random number"
description: "6 Sided die, Generate a random number from a range (inclusive)"
---

# Generate a random number


How to generate a random number in Ruby.



## 6 Sided die


```

  # Roll a 6 sided die, rand(6) returns a number from 0 to 5 inclusive
   dice_roll_result = 1 + rand(6)

```



## Generate a random number from a range (inclusive)


```ruby
# ruby 1.92
lower_limit = 1
upper_limit = 6
Random.new.rand(lower_limit..upper_limit) # Change your range operator to suit your needs

```



#### Remarks


Alias of Random::DEFAULT.rand. This uses a pseudo-random number generator which approximates true randomness

