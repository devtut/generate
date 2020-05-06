---
metaTitle: "Ruby - Monkey Patching in Ruby"
description: "Adding Functionality"
---

# Monkey Patching in Ruby



## Adding Functionality


You can add a method to any class in Ruby, whether it's a builtin or not. The calling object is referenced using `self`.

```ruby
class Fixnum
  def plus_one
    self + 1
  end

  def plus(num)
    self + num
  end

  def concat_one
    self.to_s + '1'
  end
end

1.plus_one # => 2
3.plus(5) # => 8
6.concat_one # => '61'

```



#### Remarks


Monkey patching, while convenient, has some pitfalls that aren't immediately obvious. Most notably, a patch like that in the example pollutes the global scope. If two modules both add `Hash#symbolize`, only the last module required actually applies its change; the rest are erased.

Furthermore, if there's an error in a patched method, the stacktrace simply points to the patched class. This implies that there's a bug in the `Hash` class itself (which there is now).

Lastly, because Ruby is very flexible with what containers to hold, a method that seems very straightforward when you write it has lots of undefined functionality. For instance, creating `Array#sum` is good for an array of numbers, but breaks when given an array of a custom class.

A safer alternative is refinements, available in Ruby >= 2.0.

