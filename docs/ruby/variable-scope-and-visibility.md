---
metaTitle: "Ruby - Variable Scope and Visibility"
description: "Class Variables, Local Variables, Global Variables, Instance Variables"
---

# Variable Scope and Visibility



## Class Variables


Class variables have a class wide scope, they can be declared anywhere in the class. A variable will be considered a class variable when prefixed with `@@`

```ruby
class Dinosaur
    @@classification = "Like a Reptile, but like a bird"
    
    def self.classification
        @@classification
    end

    def classification
        @@classification
    end
end

dino = Dinosaur.new
dino.classification
# => "Like a Reptile, but like a bird"
  
Dinosaur.classification
# => "Like a Reptile, but like a bird"

```

Class variables are shared between related classes and can be overwritten from a child class

```ruby
class TRex < Dinosaur
    @@classification = "Big teeth bird!"
end

TRex.classification
# => "Big teeth bird!"

Dinosaur.classification
# => "Big teeth bird!"

```

This behaviour is unwanted most of the time and can be circumvented by using class-level instance variables.

Class variables defined inside a module will not overwrite their including classes class variables:

```ruby
module SomethingStrange
    @@classification = "Something Strange"
end

class DuckDinosaur < Dinosaur
    include SomethingStrange
end

DuckDinosaur.class_variables
# => [:@@classification]
SomethingStrange.class_variables
# => [:@@classification]

DuckDinosaur.classification
# => "Big teeth bird!"

```



## Local Variables


Local variables (unlike the other variable classes) do not have any prefix

```ruby
local_variable = "local"
p local_variable
# => local

```

Its scope is dependent on where it has been declared, it can not be used outside the "declaration containers" scope. For example, if a local variable is declared in a method, it can only be used inside that method.

```ruby
def some_method
    method_scope_var = "hi there"
    p method_scope_var
end

some_method
# hi there
# => hi there

method_scope_var
# NameError: undefined local variable or method `method_scope_var'

```

Of course, local variables are not limited to methods, as a rule of thumb you could say that, as soon as you declare a variable inside a `do ... end` block or wrapped in curly braces `{}` it will be local and scoped to the block it has been declared in.

```ruby
2.times do |n|
    local_var = n + 1
    p local_var
end
# 1
# 2
# => 2

local_var
# NameError: undefined local variable or method `local_var'

```

However, local variables declared in `if` or `case` blocks can be used in the parent-scope:

```ruby
if true
    usable = "yay"
end

p usable
# yay
# => "yay"

```

While local variables can not be used outside of its block of declaration, it will be passed down to blocks:

```ruby
my_variable = "foo"

my_variable.split("").each_with_index do |char, i|
    puts "The character in string '#{my_variable}' at index #{i} is #{char}"
end
# The character in string 'foo' at index 0 is f
# The character in string 'foo' at index 1 is o
# The character in string 'foo' at index 2 is o
# => ["f", "o", "o"]

```

But not to method / class / module definitions

```ruby
my_variable = "foo"

def some_method
    puts "you can't use the local variable in here, see? #{my_variable}"
end

some_method
# NameError: undefined local variable or method `my_variable'

```

The variables used for block arguments are (of course) local to the block, but will overshadow previously defined variables, without overwriting them.

```ruby
overshadowed = "sunlight"

["darkness"].each do |overshadowed|
    p overshadowed
end
# darkness
# => ["darkness"]

p overshadowed
# "sunlight"
# => "sunlight"

```



## Global Variables


Global variables have a global scope and hence, can be used everywhere. Their scope is not dependent on where they are defined. A variable will be considered global, when prefixed with a `$` sign.

```ruby
$i_am_global = "omg"

class Dinosaur
    def instance_method
       p "global vars can be used everywhere. See? #{$i_am_global}, #{$another_global_var}" 
    end

    def self.class_method
       $another_global_var = "srsly?"
       p "global vars can be used everywhere. See? #{$i_am_global}"
    end
end

Dinosaur.class_method
# "global vars can be used everywhere. See? omg"
# => "global vars can be used everywhere. See? omg"

dinosaur = Dinosaur.new
dinosaur.instance_method
# "global vars can be used everywhere. See? omg, srsly?"
# => "global vars can be used everywhere. See? omg, srsly?"

```

Since a global variable can be defined everywhere and will be visible everywhere, calling an "undefined" global variable will return nil instead of raising an error.

```ruby
p $undefined_var
# nil
# => nil

```

Although global variables are easy to use its usage is strongly discouraged in favour of constants.



## Instance Variables


Instance variables have an object wide scope, they can be declared anywhere in the object, however an instance variable declared on class level, will only be visible in the class object. A variable will be considered an instance variable when prefixed with `@`. Instance variables are used to set and get an objects attributes and will return nil if not defined.

```ruby
class Dinosaur
    @base_sound = "rawrr"

    def initialize(sound = nil)
        @sound = sound || self.class.base_sound
    end

    def speak
        @sound
    end

    def try_to_speak
        @base_sound
    end

    def count_and_store_sound_length
        @sound.chars.each_with_index do |char, i|
            @sound_length = i + 1
            p "#{char}: #{sound_length}"
        end
    end
    
    def sound_length
        @sound_length
    end

    def self.base_sound
        @base_sound
    end
end

dino_1 = Dinosaur.new
dino_2 = Dinosaur.new "grrr"

Dinosaur.base_sound
# => "rawrr"
dino_2.speak
# => "grrr"

```

The instance variable declared on class level can not be accessed on object level:

```ruby
dino_1.try_to_speak
# => nil

```

However, we used the instance variable `@base_sound` to instantiate the sound when no sound is passed to the new method:

```ruby
dino_1.speak
# => "rawwr"

```

Instance variables can be declared anywhere in the object, even inside a block:

```ruby
dino_1.count_and_store_sound_length
# "r: 1"
# "a: 2"
# "w: 3"
# "r: 4"
# "r: 5"
# => ["r", "a", "w", "r", "r"]

dino_1.sound_length
# => 5

```

Instance variables are **not** shared between instances of the same class

```ruby
dino_2.sound_length
# => nil

```

This can be used to create class level variables, that will not be overwritten by a child-class, since classes are also objects in Ruby.

```ruby
class DuckDuckDinosaur < Dinosaur
    @base_sound = "quack quack"
end

duck_dino = DuckDuckDinosaur.new
duck_dino.speak
# => "quack quack"
DuckDuckDinosaur.base_sound
# => "quack quack"
Dinosaur.base_sound
# => "rawrr"

```



#### Syntax


- $global_variable
- @@class_variable
- @instance_variable
- local_variable



#### Remarks


Class variables are shared in the class hierarchy. This can result in surprising behavior.

```ruby
class A
  @@variable = :x

  def self.variable
    @@variable
  end
end

class B < A
  @@variable = :y
end

A.variable  # :y

```

Classes are objects, so instance variables can be used to provide state that is specific to each class.

```ruby
class A
  @variable = :x

  def self.variable
    @variable
  end
end

class B < A
  @variable = :y
end

A.variable  # :x

```

