---
metaTitle: "Ruby - Truthiness"
description: "All objects may be converted to booleans in Ruby, Truthiness of a value can be used in if-else constructs"
---

# Truthiness



## All objects may be converted to booleans in Ruby


Use the double negation syntax to check for truthiness of values. All values correspond to a boolean, irrespective of their type.

```ruby
irb(main):001:0> !!1234
=> true
irb(main):002:0> !!"Hello, world!"
(irb):2: warning: string literal in condition
=> true
irb(main):003:0> !!true
=> true
irb(main):005:0> !!{a:'b'}
=> true

```

All values except `nil` and `false` are truthy.

```ruby
irb(main):006:0> !!nil
=> false
irb(main):007:0> !!false
=> false

```



## Truthiness of a value can be used in if-else constructs


You do not need to use double negation in if-else statements.

```ruby
if 'hello'
    puts 'hey!'
else
    puts 'bye!'
end

```

The above code prints 'hey!' on the screen.



#### Remarks


As a rule of thumb, avoid using double-negations in code. [Rubocop says](http://www.rubydoc.info/github/bbatsov/rubocop/Rubocop/Cop/Style/DoubleNegation) that double negations are unnecessarily complex and can often be replaced with something more readable.

Instead of writing

```ruby
def user_exists?
    !!user
end

```

use

```ruby
def user_exists?
    !user.nil?
end

```

