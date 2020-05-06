---
metaTitle: "Ruby - Classes"
description: "Constructor, Creating a class, Access Levels, Accessing instance variables with getters and setters, Class Methods types, Dynamic class creation, New, allocate, and initialize, Class and instance variables"
---

# Classes



## Constructor


A class can have only one constructor, that is a method called `initialize`. The method is automatically invoked when a new instance of the class is created.

```ruby
class Customer
  def initialize(name)
     @name = name.capitalize 
  end
end

sarah = Customer.new('sarah')
sarah.name #=> 'Sarah'

```



## Creating a class


You can define a new class using the `class` keyword.

```ruby
class MyClass
end

```

Once defined, you can create a new instance using the `.new` method

```ruby
somevar = MyClass.new
# => #<MyClass:0x007fe2b8aa4a18>

```



## Access Levels


Ruby has three access levels. They are `public`, `private` and `protected`.

Methods that follow the `private` or `protected` keywords are defined as such. Methods that come before these are implicitly `public` methods.

### **Public Methods**

A public method should describe the behavior of the object being created. These methods can be called from outside the scope of the created object.

```ruby
class Cat
  def initialize(name)
    @name = name
  end

  def speak
    puts "I'm #{@name} and I'm 2 years old"
  end
  
  ...
end

new_cat = Cat.new("garfield")
#=> <Cat:0x2321868 @name="garfield">
 
new_cat.speak
#=> I'm garfield and I'm 2 years old

```

These methods are public ruby methods, they describe the behavior for initializing a new cat and the behavior of the speak method.

`public` keyword is unnecessary, but can be used to escape `private` or `protected`

```ruby
def MyClass
  def first_public_method
  end

  private

  def private_method
  end

  public

  def second_public_method
  end
end

```

### **Private Methods**

Private methods are not accessible from outside of the object. They are used internally by the object. Using the cat example again:

```ruby
class Cat
  def initialize(name)
    @name = name
  end

  def speak
    age = calculate_cat_age # here we call the private method 
    puts "I'm #{@name} and I'm #{age} years old"
  end

  private
     def calculate_cat_age
       2 * 3 - 4 
     end
end

my_cat = Cat.new("Bilbo")
my_cat.speak #=> I'm Bilbo and I'm 2 years old
my_cat.calculate_cat_age #=> NoMethodError: private method `calculate_cat_age' called for #<Cat:0x2321868 @name="Bilbo">

```

As you can see in the example above, the newly created Cat object has access to the `calculate_cat_age` method internally. We assign the variable `age` to the result of running the private `calculate_cat_age` method which prints the name and age of the cat to the console.

When we try and call the `calculate_cat_age` method from outside the `my_cat` object, we receive a `NoMethodError` because it's private. Get it?

### **Protected Methods**

Protected methods are very similar to private methods. They cannot be accessed outside the instance of object in the same way private methods can't be. However, using the `self` ruby method, protected methods can be called within the context of an object of the same type.

```ruby
class Cat
  def initialize(name, age)
    @name = name
    @age = age
  end

  def speak
    puts "I'm #{@name} and I'm #{@age} years old"
  end

  # this == method allows us to compare two objects own ages. 
  # if both Cat's have the same age they will be considered equal.
  def ==(other)
     self.own_age == other.own_age
  end

  protected
     def own_age
        self.age
     end
end

cat1 = Cat.new("ricky", 2)
=> #<Cat:0x007fe2b8aa4a18 @name="ricky", @age=2>

cat2 = Cat.new("lucy", 4)
=> #<Cat:0x008gfb7aa6v67 @name="lucy", @age=4>

cat3 = Cat.new("felix", 2)
=> #<Cat:0x009frbaa8V76 @name="felix", @age=2>

```

You can see we've added an age parameter to the cat class and created three new cat objects with the name and age. We are going to call the `own_age` protected method to compare the age's of our cat objects.

```ruby
cat1 == cat2
=> false

cat1 == cat3
=> true

```

Look at that, we were able to retrieve cat1's age using the `self.own_age` protected method and compare it against cat2's age by calling `cat2.own_age` inside of cat1.



## Accessing instance variables with getters and setters


We have three methods:

1. **`attr_reader`**: used to allow `read`ing the variable outside the class.
1. **`attr_writer`**: used to allow modifying the variable outside the class.
1. **`attr_accessor`**: combines both methods.

```ruby
class Cat
  attr_reader :age # you can read the age but you can never change it
  attr_writer :name # you can change name but you are not allowed to read
  attr_accessor :breed # you can both change the breed and read it

  def initialize(name, breed)
    @name = name
    @breed = breed
    @age = 2 
  end
  def speak
    puts "I'm #{@name} and I am a #{@breed} cat"
  end
end
 
my_cat = Cat.new("Banjo", "birman")
# reading values:

my_cat.age  #=> 2
my_cat.breed #=> "birman"
my_cat.name #=> Error

# changing values
 
my_cat.age = 3 #=> Error
my_cat.breed = "sphynx" 
my_cat.name = "Bilbo"

my_cat.speak #=> I'm Bilbo and I am a sphynx cat

```

Note that the parameters are symbols. this works by creating a method.

```ruby
class Cat
  attr_accessor :breed
end

```

Is basically the same as:

```ruby
class Cat
  def breed
    @breed
  end
  def breed= value
    @breed = value
  end
end

```



## Class Methods types


Classes have 3 types of methods: instance, singleton and class methods.

### Instance Methods

These are methods that can be called from an `instance` of the class.

```ruby
class Thing
  def somemethod
    puts "something"
  end
end

foo = Thing.new # create an instance of the class
foo.somemethod # => something

```

### Class Method

These are static methods, i.e,  they can be invoked on the class, and not on an instantiation of that class.

```ruby
class Thing
  def Thing.hello(name)
    puts "Hello, #{name}!"
  end
end

```

It is equivalent to use `self` in place of the class name. The following code is equivalent to the code above:

```ruby
class Thing
  def self.hello(name)
    puts "Hello, #{name}!"
  end
end

```

Invoke the method by writing

```ruby
Thing.hello("John Doe")  # prints: "Hello, John Doe!"

```

### Singleton Methods

These are only available to specific instances of the class, but not to all.

```ruby
# create an empty class
class Thing
end

# two instances of the class
thing1 = Thing.new
thing2 = Thing.new

# create a singleton method
def thing1.makestuff
  puts "I belong to thing one"
end

thing1.makestuff # => prints: I belong to thing one
thing2.makestuff # NoMethodError: undefined method `makestuff' for #<Thing>

```

Both the `singleton` and `class` methods are called `eigenclass`es. Basically, what ruby does is to create an anonymous class that holds such methods so that it won't interfere with the instances that are created.

Another way of doing this is by the `class <<` constructor. For example:

```ruby
# a class method (same as the above example)
class Thing
  class << self # the anonymous class
    def hello(name)
      puts "Hello, #{name}!"
    end
  end
end

Thing.hello("sarah") # => Hello, sarah!

# singleton method

class Thing
end

thing1 = Thing.new

class << thing1
  def makestuff
    puts "I belong to thing one"
  end
end

thing1.makestuff # => prints: "I belong to thing one"

```



## Dynamic class creation


Classes can be created dynamically through the use of `Class.new`.

```ruby
# create a new class dynamically
MyClass = Class.new

# instantiate an object of type MyClass
my_class = MyClass.new

```

In the above example, a new class is created and assigned to the constant `MyClass`.  This class can be instantiated and used just like any other class.

The `Class.new` method accepts a `Class` which will become the superclass of the dynamically created class.

```ruby
# dynamically create a class that subclasses another
Staffy = Class.new(Dog)

# instantiate an object of type Staffy
lucky = Staffy.new
lucky.is_a?(Staffy) # true
lucky.is_a?(Dog)    # true

```

The `Class.new` method also accepts a block.  The context of the block is the newly created class.  This allows methods to be defined.

```ruby
Duck = 
  Class.new do
    def quack
      'Quack!!'
    end
  end

# instantiate an object of type Duck
duck = Duck.new
duck.quack # 'Quack!!'

```



## New, allocate, and initialize


In many languages, new instances of a class are created using a special `new` keyword. In Ruby, `new` is also used to create instances of a class, but it isn't a keyword; instead, it's a static/class method, no different from any other static/class method. The definition is roughly this:

```ruby
class MyClass
   def self.new(*args)
     obj = allocate
     obj.initialize(*args) # oversimplied; initialize is actually private
     obj
   end
end

```

`allocate` performs the real 'magic' of creating an uninitialized instance of the class

Note also that the return value of `initialize` is discarded, and obj is returned instead. This makes it immediately clear why you can code your initialize method without worrying about returning `self` at the end.

The 'normal' `new` method that all classes get from `Class` works as above, but it's possible to redefine it however you like, or to define alternatives that work differently. For example:

```ruby
class MyClass
  def self.extraNew(*args)
    obj = allocate
    obj.pre_initialize(:foo)
    obj.initialize(*args)
    obj.post_initialize(:bar)
    obj
  end
end

```



## Class and instance variables


There are several special variable types that a class can use for more easily sharing data.

Instance variables, preceded by `@`. They are useful if you want to use the same variable in different methods.

```ruby
class Person
  def initialize(name, age)
    my_age = age # local variable, will be destroyed at end of constructor
    @name = name # instance variable, is only destroyed when the object is
  end

  def some_method
    puts "My name is #{@name}." # we can use @name with no problem
  end
   
  def another_method
    puts "My age is #{my_age}." # this will not work!
  end
end

mhmd = Person.new("Mark", 23)

mhmd.some_method #=> My name is Mark.
mhmd.another_method #=> throws an error

```

Class variable, preceded by `@@`. They contain the same values across all instances of a class.

```ruby
class Person
  @@persons_created = 0 # class variable, available to all objects of this class
  def initialize(name)
    @name = name

    # modification of class variable persists across all objects of this class
    @@persons_created += 1
  end  
      
  def how_many_persons
    puts "persons created so far: #{@@persons_created}"
  end 
end
    
mark = Person.new("Mark")
mark.how_many_persons #=> persons created so far: 1
helen = Person.new("Helen")

mark.how_many_persons #=> persons created so far: 2
helen.how_many_persons #=> persons created so far: 2
# you could either ask mark or helen

```

Global Variables, preceded by `$`. These are available anywhere to the program, so make sure to use them wisely.

```ruby
$total_animals = 0

class Cat
  def initialize
    $total_animals += 1
  end
end

class Dog
  def initialize
    $total_animals += 1
  end
end

bob = Cat.new()
puts $total_animals #=> 1
fred = Dog.new()
puts $total_animals #=> 2

```



#### Syntax


- class Name
- #some code describing the class behavior
- end



#### Remarks


Class names in Ruby are Constants, so the first letter should be a capital.

```ruby
class Cat # correct
end  

class dog # wrong, throws an error
end

```

