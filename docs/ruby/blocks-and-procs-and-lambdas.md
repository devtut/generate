---
metaTitle: "Ruby - Blocks and Procs and Lambdas"
description: "Lambdas, Partial Application and Currying, Objects as block arguments to methods, Converting to Proc, Blocks, Proc"
---

# Blocks and Procs and Lambdas



## Lambdas


```ruby
# lambda using the arrow syntax
hello_world = -> { 'Hello World!' }
hello_world[]
# 'Hello World!'

# lambda using the arrow syntax accepting 1 argument
hello_world = ->(name) { "Hello #{name}!" }
hello_world['Sven']
# "Hello Sven!"

the_thing = lambda do |magic, ohai, dere|
  puts "magic! #{magic}"
  puts "ohai #{dere}"
  puts "#{ohai} means hello"
end

the_thing.call(1, 2, 3)
# magic! 1
# ohai 3
# 2 means hello

the_thing.call(1, 2)
# ArgumentError: wrong number of arguments (2 for 3)

the_thing[1, 2, 3, 4]
# ArgumentError: wrong number of arguments (4 for 3)

```

You can also use `->` to create and `.()` to call lambda

```ruby
the_thing = ->(magic, ohai, dere) {
  puts "magic! #{magic}"
  puts "ohai #{dere}"
  puts "#{ohai} means hello"
}

the_thing.(1, 2, 3)
# => magic! 1
# => ohai 3
# => 2 means hello

```

Here you can see that a lambda is almost the same as a proc. However, there are several caveats:

<li>
The arity of a lambda's arguments are enforced; passing the wrong number of arguments to a lambda, will raise an `ArgumentError`. They can still have default parameters, splat parameters, etc.
</li>
<li>
`return`ing from within a lambda returns from the lambda, while `return`ing from a proc returns out of the enclosing scope:

```ruby
def try_proc
  x = Proc.new {
    return # Return from try_proc
  }
  x.call
  puts "After x.call" # this line is never reached
end

def try_lambda
  y = -> {
    return # return from y
  }
  y.call
  puts "After y.call" # this line is not skipped
end

try_proc # No output
try_lambda # Outputs "After y.call"

```


</li>



## Partial Application and Currying


Technically, Ruby doesn't have functions, but methods. However, a Ruby method behaves almost identically to functions in other language:

```ruby
def double(n)
  n * 2
end

```

This normal method/function takes a parameter `n`, doubles it and returns the value. Now let's define a higher order function (or method):

```ruby
def triple(n)
  lambda {3 * n}
end

```

Instead of returning a number, `triple` returns a method. You can test it using the [Interactive Ruby Shell](https://en.wikipedia.org/wiki/Interactive_Ruby_Shell):

```ruby
$ irb --simple-prompt
>> def double(n)
>>   n * 2
>> end
=> :double
>> def triple(n)
>>   lambda {3 * n}
>> end
=> :triple
>> double(2)
=> 4
>> triple(2)
=> #<Proc:0x007fd07f07bdc0@(irb):7 (lambda)>

```

If you want to actually get the tripled number, you need to call (or "reduce") the lambda:

```ruby
triple_two = triple(2)
triple_two.call # => 6

```

Or more concisely:

```ruby
triple(2).call

```

### Currying and Partial Applications

This is not useful in terms of defining very basic functionality, but it is useful if you want to have methods/functions that are not instantly called or reduced. For example, let's say you want to define methods that add a number by a specific number (for example `add_one(2) = 3`). If you had to define a ton of these you could do:

```ruby
def add_one(n)
  n + 1
end 

def add_two(n)
  n + 2
end

```

However, you could also do this:

```ruby
add = -> (a, b) { a + b }
add_one = add.curry.(1)
add_two = add.curry.(2)

```

Using lambda calculus we can say that `add` is `(λa.(λb.(a+b)))`. Currying is a way of **partially applying** `add`. So `add.curry.(1)`, is `(λa.(λb.(a+b)))(1)` which can be reduced to `(λb.(1+b))`. Partial application means that we passed one argument to `add` but left the other argument to be supplied later. The output is a specialized method.

### More useful examples of currying

Let's say we have really big general formula, that if we specify certain arguments to it, we can get specific formulae from it. Consider this formula:

```ruby
f(x, y, z) = sin(x\*y)*sin(y\*z)*sin(z\*x)

```

This formula is made for working in three dimensions, but let's say we only want this formula with regards to y and z. Let's also say that to ignore x, we want to set it's value to pi/2. Let's first make the general formula:

```ruby
f = ->(x, y, z) {Math.sin(x*y) * Math.sin(y*z) * Math.sin(z*x)}

```

Now, let's use currying to get our `yz` formula:

```ruby
f_yz = f.curry.(Math::PI/2)

```

Then to call the lambda stored in `f_yz`:

```ruby
f_xy.call(some_value_x, some_value_y)

```

This is pretty simple, but let's say we want to get the formula for `xz`. How can we set `y` to `Math::PI/2` if it's not the last argument? Well, it's a bit more complicated:

```ruby
f_xz = -> (x,z) {f.curry.(x, Math::PI/2, z)}

```

In this case, we need to provide placeholders for the parameter we aren't pre-filling. For consistency we could write `f_xy` like this:

```ruby
f_xy = -> (x,y) {f.curry.(x, y, Math::PI/2)}

```

Here's how the lambda calculus works for `f_yz`:

```ruby
f = (λx.(λy.(λz.(sin(x*y) * sin(y*z) * sin(z*x))))
f_yz = (λx.(λy.(λz.(sin(x*y) * sin(y*z) * sin(z*x)))) (π/2) # Reduce => 
f_yz = (λy.(λz.(sin((π/2)*y) * sin(y*z) * sin(z*(π/2))))

```

Now let's look at `f_xz`

```ruby
f = (λx.(λy.(λz.(sin(x*y) * sin(y*z) * sin(z*x))))
f_xz = (λx.(λy.(λz.(sin(x*y) * sin(y*z) * sin(z*x)))) (λt.t) (π/2)  # Reduce =>
f_xz = (λt.(λz.(sin(t*(π/2)) * sin((π/2)*z) * sin(z*t))))

```

For more reading about lambda calculus try [this](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf).



## Objects as block arguments to methods


Putting a `&` (ampersand) in front of an argument will pass it as the method's block. Objects will be converted to a `Proc` using the `to_proc` method.

```ruby
class Greeter
  def to_proc
    Proc.new do |item|
      puts "Hello, #{item}"
    end
  end
end

greet = Greeter.new

%w(world life).each(&greet)

```

This is a common pattern in Ruby and many standard classes provide it.

For example, [`Symbol`](http://ruby-doc.org/core/Symbol.html)s implement `to_proc` by sending themselves to the argument:

```ruby
# Example implementation
class Symbol
  def to_proc
    Proc.new do |receiver|
      receiver.send self
    end
  end
end

```

This enables the useful `&:symbol` idiom, commonly used with [`Enumerable`](http://ruby-doc.org/core/Enumerable.html) objects:

```ruby
letter_counts = %w(just some words).map(&:length)  # [4, 4, 5]

```



## Converting to Proc


Objects that respond to `to_proc` can be converted to procs with the `&` operator (which will also allow them to be passed as blocks).

The class Symbol defines `#to_proc` so it tries to call the corresponding method on the object it receives as parameter.

```ruby
p [ 'rabbit', 'grass' ].map( &:upcase ) # => ["RABBIT", "GRASS"]

```

Method objects also define `#to_proc`.

```ruby
output = method( :p )

[ 'rabbit', 'grass' ].map( &output ) # => "rabbit\ngrass"

```



## Blocks


Blocks are chunks of code enclosed between braces `{}` (usually for single-line blocks) or `do..end` (used for multi-line blocks).

```ruby
5.times { puts "Hello world" } # recommended style for single line blocks

5.times do
    print "Hello "
    puts "world"
end   # recommended style for multi-line blocks

5.times {
    print "hello "
    puts "world" } # does not throw an error but is not recommended

```

Note: braces have higher precedence than `do..end`

### Yielding

Blocks can be used inside methods and functions using the word `yield`:

```ruby
def block_caller
    puts "some code"
    yield
    puts "other code"
end
block_caller { puts "My own block" } # the block is passed as an argument to the method.
#some code
#My own block
#other code

```

Be careful though if `yield` is called without a block it will raise a `LocalJumpError`. For this purpose ruby provides another method called `block_given?` this allows you to check if a block was passed before calling yield

```ruby
def block_caller
  puts "some code" 
  if block_given? 
    yield
  else
    puts "default"
  end
  puts "other code"
end
block_caller 
# some code
# default
# other code
block_caller { puts "not defaulted"}
# some code
# not defaulted
# other code

```

`yield` can offer arguments to the block as well

```ruby
def yield_n(n)
  p = yield n if block_given?
  p || n 
end
yield_n(12) {|n| n + 7 } 
#=> 19 
yield_n(4) 
#=> 4

```

While this is a simple example `yield`ing can be very useful for allowing direct access to instance variables or evaluations inside the context of another object. For Example:

```ruby
class Application
  def configuration
    @configuration ||= Configuration.new
    block_given? ? yield(@configuration) : @configuration
  end
end
class Configuration; end

app = Application.new 
app.configuration do |config| 
  puts config.class.name
end
# Configuration
#=> nil 
app.configuration
#=> #<Configuration:0x2bf1d30>

```

As you can see using `yield` in this manner makes the code more readable than continually calling `app.configuration.#method_name`. Instead you can perform all the configuration inside the block keeping the code contained.

### Variables

Variables for blocks are local to the block (similar to the variables of functions), they die when the block is executed.

```ruby
my_variable = 8
3.times do |x|
    my_variable = x 
    puts my_variable
end
puts my_variable
#=> 0
# 1
# 2
# 8

```

Blocks can't be saved, they die once executed. In order to save blocks you need to use `procs` and `lambdas`.



## Proc


```ruby
def call_the_block(&calling); calling.call; end

its_a = proc do |*args|
  puts "It's a..." unless args.empty?
  "beautiful day"
end

puts its_a       #=> "beautiful day"
puts its_a.call  #=> "beautiful day"
puts its_a[1, 2] #=> "It's a..." "beautiful day"

```

We've copied the method `call_the_block` from the last example. Here, you can see that a proc is made by calling the `proc` method with a block. You can also see that blocks, like methods, have implicit returns, which means that procs (and lambdas) do too. In the definition of `its_a`, you can see that blocks can take splat arguments as well as normal ones; they're also capable of taking default arguments, but I couldn't think of a way to work that in. Lastly, you can see that it's possible to use multiple syntaxes to call a method -- either the `call` method, or the `[]` operator.



#### Syntax


- Proc.new(**block**)
- lambda { |args| code }
- ->(arg1, arg2) { code }
- object.to_proc
- { |single_arg| code }
<li>do |arg, (key, value)|
**code**
end</li>



#### Remarks


Be careful about operator precedence when you have a line with multiple methods chained, like:

```ruby
str = "abcdefg"
puts str.gsub(/./) do |match|
  rand(2).zero? ? match.upcase : match.downcase
end

```

Instead of printing something like `abCDeFg`, like you'd expect, it prints something like `#<Enumerator:0x00000000af42b28>` -- this is because `do ... end` has lower precedence than methods, which means that `gsub` only sees the `/./` argument, and not the block argument. It returns an enumerator. The block ends up passed to `puts`, which ignores it and just displays the result of `gsub(/./)`.

To fix this, either wrap the `gsub` call in parentheses or use `{ ... }` instead.

