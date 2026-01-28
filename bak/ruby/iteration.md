---
metaTitle: "Ruby - Iteration"
description: "Each, Implementation in a class, Iterating over complex objects, For  iterator, Iteration with index, Map"
---

# Iteration



## Each


Ruby has many types of enumerators but the first and most simple type of enumerator to start with is `each`. We will print out `even` or `odd` for each number between `1` and `10` to show how `each` works.

Basically there are two ways to pass so called `blocks`. A `block` is a piece of code being passed which will be executed by the method which is called. The `each` method takes a `block` which it calls for every element of the collection of objects it was called on.

There are two ways to pass a block to a method:

### Method 1: Inline

```ruby
(1..10)</code> is a range from `1` to `10` inclusive. If we wanted it to be `1` to `10` exclusive, we would write `(1...10)`.</li>
- `.each` is an enumerator that enumerates over `each` element in the object it is acting on. In this case, it acts on `each` number in the range.
<li>`{ |i| puts i.even? ? 'even' : 'odd' }` is the block for the `each` statement, which itself can be broken down further.
<ol>
- `|i|` this means that each element in the range is represented within the block by the identifier `i`.
- `puts` is an output method in Ruby that has an automatic line break after each time it prints. (We can use `print` if we don't want the automatic line break)
- `i.even?` checks if `i` is even. We could have also used `i % 2 == 0`; however, it is preferable to use built in methods.
- `? "even" : "odd"` this is ruby's ternary operator. The way a ternary operator is constructed is `expression ? a : b`. This is short for
</ol>
<pre>if expression
  a
else
  b
end
</pre>
</li>
</ol>
For code longer than one line the `block` should be passed as a `multiline block`.
<h3>Method 2: Multiline</h3>
<pre>(1..10).each do |i|
  if i.even?
    puts 'even'
  else
    puts 'odd'
  end
end
</pre>
In a `multiline block` the `do` replaces the opening bracket and `end` replaces the closing bracket from the `inline` style.
Ruby supports reverse_each as well. It will iterate the array backwards.

```ruby
@arr = [1,2,3,4]
puts @arr.inspect # output is [1,2,3,4]

print "Reversed array elements["
@arr.reverse_each do |val|
        print " #{val} " # output is 4 3 2 1
end
print "]\n"

```


1. `(1..10)` is a range from `1` to `10` inclusive. If we wanted it to be `1` to `10` exclusive, we would write `(1...10)`.
1. `.each` is an enumerator that enumerates over `each` element in the object it is acting on. In this case, it acts on `each` number in the range.
<li>`{ |i| puts i.even? ? 'even' : 'odd' }` is the block for the `each` statement, which itself can be broken down further.
<ol>
1. `|i|` this means that each element in the range is represented within the block by the identifier `i`.
1. `puts` is an output method in Ruby that has an automatic line break after each time it prints. (We can use `print` if we don't want the automatic line break)
1. `i.even?` checks if `i` is even. We could have also used `i % 2 == 0`; however, it is preferable to use built in methods.
1. `? "even" : "odd"` this is ruby's ternary operator. The way a ternary operator is constructed is `expression ? a : b`. This is short for



## Implementation in a class


`Enumerable` is the most popular module in Ruby. Its purpose is to provide you with iterable methods like `map`, `select`, `reduce`, etc. Classes that use `Enumerable` include `Array`, `Hash`, `Range`.
To use it, you have to `include Enumerable` and implement `each`.

```ruby
class NaturalNumbers
  include Enumerable

  def initialize(upper_limit)
    @upper_limit = upper_limit
  end

  def each(&block)
    0.upto(@upper_limit).each(&block)
  end
end

n = NaturalNumbers.new(6)

n.reduce(:+)                   # => 21
n.select(&:even?)              # => [0, 2, 4, 6]
n.map { |number| number ** 2 } # => [0, 1, 4, 9, 16, 25, 36]

```



## Iterating over complex objects


**Arrays**

You can iterate over nested arrays:

```ruby
[[1, 2], [3, 4]].each { |(a, b)| p "a: #{ a }", "b: #{ b }" }

```

The following syntax is allowed too:

```ruby
[[1, 2], [3, 4]].each { |a, b| "a: #{ a }", "b: #{ b }" }

```

Will produce:

```ruby
"a: 1"
"b: 2"
"a: 3"
"b: 4"

```

**Hashes**

You can iterate over key-value pairs:

```ruby
{a: 1, b: 2, c: 3}.each { |pair| p "pair: #{ pair }" }

```

Will produce:

```ruby
"pair: [:a, 1]"
"pair: [:b, 2]"
"pair: [:c, 3]"

```

You can iterate over keys and values simultaneously:

```ruby
{a: 1, b: 2, c: 3}.each { |(k, v)| p "k: #{ k }", "v: #{ k }" }

```

Will produce:

```ruby
"k: a"
"v: a"
"k: b"
"v: b"
"k: c"
"v: c"

```



## For  iterator


This iterates from 4 to 13 (inclusive).

```ruby
for i in 4..13
    puts "this is #{i}.th number"
end

```

We can also iterate over arrays using for

```ruby
names = ['Siva', 'Charan', 'Naresh', 'Manish']

for name in names
    puts name
end

```



## Iteration with index


Sometimes you want to know the position (**index**) of the current element while iterating over an enumerator. For such purpose, Ruby provides the `with_index` method. It can be applied to all the enumerators. Basically, by adding `with_index` to an enumeration, you can enumerate that enumeration. Index is passed to a block as the second argument.

```ruby
[2,3,4].map.with_index { |e, i| puts "Element of array number #{i} => #{e}" }
#Element of array number 0 => 2
#Element of array number 1 => 3
#Element of array number 2 => 4
#=> [nil, nil, nil]

```

`with_index` has an optional argument – the first index which is `0` by default:

```ruby
[2,3,4].map.with_index(1) { |e, i| puts "Element of array number #{i} => #{e}" }
#Element of array number 1 => 2
#Element of array number 2 => 3
#Element of array number 3 => 4
#=> [nil, nil, nil]

```

There is a specific method `each_with_index`. The only difference between it and `each.with_index` is that you can't pass an argument to that, so the first index is `0` all the time.

```ruby
[2,3,4].each_with_index { |e, i| puts "Element of array number #{i} => #{e}" }
#Element of array number 0 => 2
#Element of array number 1 => 3
#Element of array number 2 => 4
#=> [2, 3, 4]

```



## Map


Returns the changed object, but the original object remains as it was. For example:

```ruby
arr = [1, 2, 3]
arr.map { |i| i + 1 } # => [2, 3, 4]
arr # => [1, 2, 3]

```

`map!` changes the original object:

```ruby
arr = [1, 2, 3]
arr.map! { |i| i + 1 } # => [2, 3, 4]
arr # => [2, 3, 4]

```

Note: you can also use `collect` to do the same thing.

