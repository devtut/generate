---
metaTitle: "Ruby on Rails - Safe Constantize"
description: "Successful safe_constantize, Unsuccessful safe_constantize "
---

# Safe Constantize



## Successful safe_constantize


`User` is an `ActiveRecord` or `Mongoid` class. Replace `User` with any `Rails` class in your project (even something like `Integer` or `Array`)

```ruby
my_string = "User" # Capitalized string
  # => 'User'
my_constant = my_string.safe_constantize
  # => User
my_constant.all.count
  # => 18

my_string = "Array"
  # => 'Array'
my_constant = my_string.safe_constantize
  # => Array
my_constant.new(4)
  # => [nil, nil, nil, nil]

```



## Unsuccessful safe_constantize 


This example will not work because the string passed in isn't recognized as a constant in the project. Even if you pass in `"array"`, it won't work as it isn't capitalized.

```ruby
my_string = "not_a_constant" 
  # => 'not_a_constant'
my_string.safe_constantize
  # => nil

my_string = "array" #Not capitalized!
  # => 'array'
my_string.safe_constantize
  # => nil

```

