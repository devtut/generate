---
metaTitle: "Ruby - Enumerators"
description: "Custom enumerators, Existing methods, Rewinding"
---

# Enumerators


An [`Enumerator`](http://ruby-doc.org/core/Enumerator.html) is an object that implements iteration in a controlled fashion.

Instead of looping until some condition is satisfied, the object **enumerates** values as needed. Execution of the loop is paused until the next value is requested by the owner of the object.

Enumerators make infinite streams of values possible.



## Custom enumerators


Let's create an [`Enumerator`](http://ruby-doc.org/core/Enumerator.html) for [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number).

```ruby
fibonacci = Enumerator.new do |yielder|
  a = b = 1
  loop do
    yielder << a
    a, b = b, a + b
  end
end

```

We can now use any [`Enumerable`](http://ruby-doc.org/core/Enumerable.html) method with `fibonacci`:

```ruby
fibonacci.take 10
# => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

```



## Existing methods


If an iteration method such as `each` is called without a block, an [`Enumerator`](http://ruby-doc.org/core/Enumerator.html) should be returned.

This can be done using the [`enum_for`](http://ruby-doc.org/core/Object.html#method-i-enum_for) method:

```ruby
def each
  return enum_for :each unless block_given?

  yield :x
  yield :y
  yield :z
end

```

This enables the programmer to compose [`Enumerable`](http://ruby-doc.org/core/Enumerable.html) operations:

```ruby
each.drop(2).map(&:upcase).first
# => :Z

```



## Rewinding


Use [`rewind`](http://ruby-doc.org/core-2.3.1/Enumerator.html#method-i-rewind) to restart the enumerator.

```ruby
ℕ = Enumerator.new do |yielder|
  x = 0
  loop do
    yielder << x
    x += 1
  end
end

ℕ.next
# => 0

ℕ.next
# => 1

ℕ.next
# => 2

ℕ.rewind

ℕ.next
# => 0

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`yield`|Responds to `yield`, which is aliased as `<<`. Yielding to this object implements iteration.

