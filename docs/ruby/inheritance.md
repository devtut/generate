---
metaTitle: "Ruby - Inheritance"
description: "Subclasses, Multiple Inheritance, What is inherited?, Mixins, Refactoring existing classes to use Inheritance"
---

# Inheritance



## Subclasses


Inheritance allows classes to define specific behaviour based on an existing class.

```ruby
class Animal
  def say_hello
    'Meep!'
  end

  def eat
    'Yumm!'
  end
end

class Dog < Animal
  def say_hello
    'Woof!'
  end
end

spot = Dog.new
spot.say_hello # 'Woof!'
spot.eat       # 'Yumm!'

```

In this example:

- `Dog` Inherits from `Animal`, making it a **Subclass**.
- `Dog` gains both the `say_hello` and `eat` methods from `Animal`.
- `Dog` overrides the `say_hello` method with different functionality.



## Multiple Inheritance


Multiple inheritance is a feature that allows one class to inherit from multiple classes(i.e., more than one parent). Ruby does not support multiple inheritance. It only supports single-inheritance (i.e. class can have only one parent), but you can use **composition** to build more complex classes using [Modules](http://stackoverflow.com/documentation/ruby/4039/modules).



## What is inherited?


**Methods are inherited**

```ruby
class A
  def boo; p 'boo' end
end

class B < A; end

b = B.new
b.boo # => 'boo'

```

**Class methods are inherited**

```ruby
class A
  def self.boo; p 'boo' end
end

class B < A; end

p B.boo # => 'boo'

```

**Constants are inherited**

```ruby
class A
  WOO = 1
end

class B < A; end

p B::WOO # => 1

```

But beware, they can be overridden:

```ruby
class B
  WOO = WOO + 1
end

p B::WOO # => 2

```

**Instance variables are inherited:**

```ruby
class A
  attr_accessor :ho
  def initialize
    @ho = 'haha'
  end
end

class B < A; end

b = B.new
p b.ho # => 'haha'

```

Beware, if you override the methods that initialize instance variables without calling `super`, they will be nil. Continuing from above:

```ruby
class C < A
  def initialize; end
 end

c = C.new
p c.ho    # => nil

```

**Class instance variables are not inherited:**

```ruby
class A
    @foo = 'foo'
    class << self
        attr_accessor :foo
    end
end

class B < A; end

p B.foo # => nil

# The accessor is inherited, since it is a class method
#
B.foo = 'fob' # possible

```

**Class variables aren't really inherited**

They are shared between the base class and all subclasses as 1 variable:

```ruby
class A
    @@foo = 0
    def initialize
        @@foo  += 1 
        p @@foo
    end
end

class B < A;end

a = A.new # => 1
b = B.new # => 2

```

So continuing from above:

```ruby
class C < A
  def initialize
    @@foo = -10
    p @@foo
  end
end

a = C.new # => -10
b = B.new # => -9

```



## Mixins


[Mixins](http://stackoverflow.com/documentation/ruby/4039/modules) are a beautiful way to achieve something similar to multiple inheritance. It allows us to inherit or rather include methods defined in a module into a class. These methods can be included as either instance or class methods. The below example depicts this design.

```ruby
module SampleModule

  def self.included(base)
    base.extend ClassMethods
  end

  module ClassMethods

    def method_static
      puts "This is a static method"
    end

  end

  def insta_method
    puts "This is an instance method"
  end

end

class SampleClass
  include SampleModule
end

sc = SampleClass.new

sc.insta_method

prints "This is an instance method"

sc.class.method_static

prints "This is a static method"

```



## Refactoring existing classes to use Inheritance


Let's say we have two classes, `Cat` and `Dog`.

```ruby
class Cat
  def eat
    die unless has_food?
    self.food_amount -= 1
    self.hungry = false
  end
  def sound
    puts "Meow"
  end
end

class Dog
  def eat
    die unless has_food?
    self.food_amount -= 1
    self.hungry = false
  end
  def sound
    puts "Woof"
  end
end

```

The `eat` method is exactly the same in these two classes. While this works, it is hard to maintain. The problem will get worse if there are more animals with the same `eat` method. Inheritance can solve this problem.

```ruby
class Animal
  def eat
    die unless has_food?
    self.food_amount -= 1
    self.hungry = false
  end
  # No sound method
end

class Cat < Animal
  def sound
    puts "Meow"
  end
end

class Dog < Animal
  def sound
    puts "Woof"
  end
end

```

We have created a new class, `Animal`, and moved our `eat` method to that class. Then, we  made `Cat` and `Dog` inherit from this new common superclass. This removes the need for repeating code



#### Syntax


- class SubClass < SuperClass

