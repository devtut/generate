---
metaTitle: "Ruby - Methods"
description: "Yielding to blocks, Default parameters, Optional parameter(s) (splat operator), Defining a method, Single required parameter, Required default optional parameter mix, Use a function as a block, Tuple Arguments, Multiple required parameters, Method Definitions are Expressions, Capturing undeclared keyword arguments (double splat)"
---

# Methods


Functions in Ruby provide organized, reusable code to preform a set of actions. Functions simplify the coding process, prevent redundant logic, and make code easier to follow. This topic describes the declaration and utilization of functions, arguments, parameters, yield statements and scope in Ruby.



## Yielding to blocks


You can send a block to your method and it can call that block multiple times. This can be done by sending a proc/lambda or such, but is easier and faster with `yield`:

```ruby
def simple(arg1,arg2)
  puts "First we are here:  #{arg1}"
  yield
  puts "Finally we are here:  #{arg2}"
  yield
end
simple('start','end') { puts "Now we are inside the yield" }

#> First we are here:  start
#> Now we are inside the yield
#> Finally we are here:  end
#> Now we are inside the yield

```

Note that the `{ puts ... }` is not inside the parentheses, it implicitly comes after. This also means we can only have one `yield` block. We can pass arguments to the `yield`:

```ruby
def simple(arg)
  puts "Before yield"
  yield(arg)
  puts "After yield"
end
simple('Dave') { |name| puts "My name is #{name}" }

#> Before yield
#> My name is Dave
#> After yield

```

With yield we can easily make iterators or any functions that work on other code:

```ruby
def countdown(num)
  num.times do |i|
    yield(num-i)
  end
end

```

`countdown(5) { |i| puts "Call number #{i}" }`

```ruby
#> Call number 5
#> Call number 4
#> Call number 3
#> Call number 2
#> Call number 1

```

In fact, it is with `yield` that things like `foreach`, `each` and `times` are generally implemented in classes.

If you want to find out if you have been given a block or not, use `block_given?`:

```ruby
class Employees
  def names
    ret = []
    @employees.each do |emp|
      if block_given?
        yield(emp.name)
      else
        ret.push(emp.name) 
      end
    end

    ret
  end
end

```

This example assumes that the `Employees` class has an `@employees` list that can be iterated with `each` to get objects that have employee names using the `name` method. If we are given a block, then we'll `yield` the name to the block, otherwise we just push it to an array that we return.



## Default parameters


```ruby
def make_animal_sound(sound = 'Cuack')
    puts sound
end

```

### 

```ruby
make_animal_sound('Mooo') # Mooo
make_animal_sound         # Cuack

```

It's possible to include defaults for multiple arguments:

```ruby
def make_animal_sound(sound = 'Cuack', volume = 11)
    play_sound(sound, volume)
end

make_animal_sound('Mooo') # Spinal Tap cow

```

However, it's not possible to [supply the second](http://stackoverflow.com/questions/695431/in-a-method-that-take-multiple-optional-parameters-how-can-any-but-the-first-be) without also supplying the first. Instead of using positional parameters, try keyword parameters:

```ruby
def make_animal_sound(sound: 'Cuack', volume: 11)
    play_sound(sound, volume)
end

make_animal_sound(volume: 1) # Duck whisper

```

Or a hash parameter that stores options:

```ruby
def make_animal_sound(options = {})
    options[:sound]  ||= 'Cuak'
    options[:volume] ||= 11
    play_sound(sound, volume)
end

make_animal_sound(:sound => 'Mooo') 

```

Default parameter values can be set by any ruby expression. The expression will run in the context of the method, so you can even declare local variables here. Note, won't get through code review. Courtesy of caius for [pointing this out](https://gist.github.com/caius/1528785).

```ruby
def make_animal_sound( sound = ( raise 'TUU-too-TUU-too...' ) ); p sound; end

make_animal_sound 'blaaaa' # => 'blaaaa'
make_animal_sound          # => TUU-too-TUU-too... (RuntimeError)

```



## Optional parameter(s) (splat operator)


```ruby
def welcome_guests(*guests)
    guests.each { |guest| puts "Welcome #{guest}!" }
end

```

### 

```ruby
welcome_guests('Tom')    # Welcome Tom!
welcome_guests('Rob', 'Sally', 'Lucas') # Welcome Rob!
                                        # Welcome Sally!
                                        # Welcome Lucas!

```

Note that `welcome_guests(['Rob', 'Sally', 'Lucas'])` will output `Welcome ["Rob", "Sally", "Lucas"]!`<br />
Instead, if you have a list, you can do `welcome_guests(*['Rob', 'Sally', 'Lucas'])` and that will work as `welcome_guests('Rob', 'Sally', 'Lucas')`.



## Defining a method


Methods are defined with the `def` keyword, followed by the **method name** and an optional list of **parameter names** in parentheses. The Ruby code between `def` and `end` represents the **body** of the method.

```ruby
def hello(name)
  "Hello, #{name}"
end

```

A method invocation specifies the method name, the object on which it is to be invoked (sometimes called the receiver), and zero or more argument values that are assigned to the named method parameters.

```ruby
hello("World")
# => "Hello, World"

```

When the receiver is not explicit, it is `self`.

Parameter names can be used as variables within the method body, and the values of these named parameters come from the arguments to a method invocation.

```ruby
hello("World")
# => "Hello, World"
hello("All")
# => "Hello, All"

```



## Single required parameter


```ruby
def say_hello_to(name)
    puts "Hello #{name}"
end

```

### 

```ruby
say_hello_to('Charles')    # Hello Charles

```



## Required default optional parameter mix


```ruby
def my_mix(name,valid=true, *opt)
    puts name
    puts valid
    puts opt
end

```

Call as follows:

```ruby
my_mix('me')
# 'me'
# true
# []

my_mix('me', false)
# 'me'
# false
# []

my_mix('me', true, 5, 7) 
# 'me'
# true
# [5,7]

```



## Use a function as a block


Many functions in Ruby accept a block as an argument. E.g.:

```ruby
[0, 1, 2].map {|i| i + 1}
 => [1, 2, 3]

```

If you already have a function that does what you want, you can turn it into a block using `&method(:fn)`:

```ruby
def inc(num)
   num + 1
end

[0, 1, 2].map &method(:inc)
 => [1, 2, 3]

```



## Tuple Arguments


A method can take an array parameter and destructure it immediately into named local variables. Found on [Mathias Meyer's blog](http://www.paperplanes.de/2012/2/16/fun-with-ruby-block-parameters.html).

```ruby
def feed( amount, (animal, food) )

    p "#{amount} #{animal}s chew some #{food}"

end

feed 3, [ 'rabbit', 'grass' ] # => "3 rabbits chew some grass"

```



## Multiple required parameters


```ruby
def greet(greeting, name)
    puts "#{greeting} #{name}"
end

```

### 

```ruby
greet('Hi', 'Sophie')    # Hi Sophie

```



## Method Definitions are Expressions


Defining a method in Ruby 2.x returns a symbol representing the name:

```ruby
class Example
  puts def hello
  end
end

#=> :hello

```

This allows for interesting metaprogramming techniques.  For instance, methods can be wrapped by other methods:

```ruby
class Class
  def logged(name)
    original_method = instance_method(name)
    define_method(name) do |*args|
      puts "Calling #{name} with #{args.inspect}."
      original_method.bind(self).call(*args)
      puts "Completed #{name}."
    end
  end
end

class Meal
  def initialize
    @food = []
  end
  
  logged def add(item)
    @food << item
  end
end

meal = Meal.new
meal.add "Coffee"
# Calling add with ["Coffee"].
# Completed add.

```



## Capturing undeclared keyword arguments (double splat)


The `**` operator works similarly to the `*` operator but it applies to keyword parameters.

```ruby
def options(required_key:, optional_key: nil, **other_options)
  other_options
end

options(required_key: 'Done!', foo: 'Foo!', bar: 'Bar!')
#> { :foo => "Foo!", :bar => "Bar!" }

```

In the above example, if the `**other_options` is not used, an `ArgumentError: unknown keyword: foo, bar` error would be raised.

```ruby
def without_double_splat(required_key:, optional_key: nil)
  # do nothing
end

without_double_splat(required_key: 'Done!', foo: 'Foo!', bar: 'Bar!')
#> ArgumentError: unknown keywords: foo, bar

```

This is handy when you have a hash of options that you want to pass to a method and you do not want to filter the keys.

```ruby
def options(required_key:, optional_key: nil, **other_options)
  other_options
end

my_hash = { required_key: true, foo: 'Foo!', bar: 'Bar!' }

options(my_hash)
#> { :foo => "Foo!", :bar => "Bar!" }

```

It is also possible to **unpack** a hash using the `**` operator.  This allows you to supply keyword directly to a method in addition to values from other hashes:

```ruby
my_hash = { foo: 'Foo!', bar: 'Bar!' }

options(required_key: true, **my_hash)
#> { :foo => "Foo!", :bar => "Bar!" }

```



#### Remarks


A **method** is a named block of code, associated with one or more objects and generally identified by a list of parameters in addition to the name.

```ruby
def hello(name)
  "Hello, #{name}"
end

```

A method invocation specifies the method name, the object on which it is to be invoked (sometimes called the receiver), and zero or more argument values that are assigned to the named method parameters. The value of the last expression evaluated in the method becomes the value of the method invocation expression.

```ruby
hello("World")
# => "Hello, World"

```

When the receiver is not explicit, it is `self`.

```ruby
self
# => main

self.hello("World")
# => "Hello, World"

```

As explained in the **Ruby Programming Language** book, many languages distinguish between functions, which have no associated object, and methods, which are invoked on a receiver object. Because Ruby is a purely object-oriented language, all methods are true methods and are associated with at least one object.

### Overview of Method Parameters

|Type|Method Signature|Call Example|Assignments
|---|---|---|---|---|---|---|---|---|---
|**R**equired|`def fn(a,b,c)`|`fn(2,3,5)`|`a=2, b=3, c=5`
|**V**ariadic|`def fn(*rest)`|`fn(2,3,5)`|`rest=[2, 3, 5]`
|**D**efault|`def fn(a=0,b=1)`|`fn(2,3)`|`a=2, b=3`
|**K**eyword|`def fn(a:0,b:1)`|`fn(a:2,b:3)`|`a=2, b=3`

These argument types can be combined in virtually any way you can imagine to create variadic functions. The minimum number of arguments to the function will equal the amount of required arguments in the signature. Extra arguments will be assigned to default parameters first, then to the `*rest` parameter.

|Type|Method Signature|Call Example|Assignments
|---|---|---|---|---|---|---|---|---|---
|R,D,V,R|`def fn(a,b=1,*mid,z)`|`fn(2,97)`|`a=2, b=1, mid=[], z=97`
|||`fn(2,3,97)`|`a=2, b=3, mid=[], z=97`
|||`fn(2,3,5,97)`|`a=2, b=3, mid=[5], z=97`
|||`fn(2,3,5,7,97)`|`a=2, b=3, mid=[5,7], z=97`
|R,K,K|`def fn(a,g:6,h:7)`|`fn(2)`|`a=2, g=6, h=7`
|||`fn(2,h:19)`|`a=2, g=6, h=19`
|||`fn(2,g:17,h:19)`|`a=2, g=17, h=19`
|[VK](http://stackoverflow.com/documentation/ruby/997/methods/6790/capturing-undeclared-keyword-arguments-double-splat)|`def fn(**ks)`|`fn(a:2,g:17,h:19)`|`ks={a:2, g:17, h:19}`
|||`fn(four:4,five:5)`|`ks={four:4, five:5}`

