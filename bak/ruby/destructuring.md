---
metaTitle: "Ruby - Destructuring"
description: "Overview, Destructuring Block Arguments"
---

# Destructuring




## Overview


Most of the magic of destructuring uses the splat (`*`) operator.

|Example|Result / **comment**
|---|---|---|---|---|---|---|---|---|---
|`a, b = [0,1]`|`a=0, b=1`
|`a, *rest = [0,1,2,3]`|`a=0, rest=[1,2,3]`
|`a, * = [0,1,2,3]`|`a=0`   **Equivalent to `.first`**
|`*, z = [0,1,2,3]`|`z=3`   **Equivalent to `.last`**



## Destructuring Block Arguments


```ruby
triples = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

triples.each { |(first, second, third)| puts second }
# 2
# 5
# 8

triples.map { |(first, *rest)| rest.join(' ') } # => ["2 3", "5 6", "8 9"]

```

