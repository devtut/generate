---
metaTitle: "Ruby - Struct"
description: "Creating new structures for data, Customizing a structure class, Attribute lookup"
---

# Struct



## Creating new structures for data


[`Struct`](http://ruby-doc.org/core/Struct.html) defines new classes with the specified attributes and accessor methods.

```ruby
Person = Struct.new :first_name, :last_name

```

You can then instantiate objects and use them:

```ruby
person = Person.new 'John', 'Doe'
# => #<struct Person first_name="John", last_name="Doe">

person.first_name
# => "John"

person.last_name
# => "Doe"

```



## Customizing a structure class


```ruby
Person = Struct.new :name do
  def greet(someone)
    "Hello #{someone}! I am #{name}!"
  end
end

Person.new('Alice').greet 'Bob'
# => "Hello Bob! I am Alice!"

```



## Attribute lookup


Attributes can be accessed strings and symbols as keys. Numerical indexes also work.

```ruby
Person = Struct.new :name
alice = Person.new 'Alice'

alice['name']  # => "Alice"
alice[:name]   # => "Alice"
alice[0]       # => "Alice"

```



#### Syntax


- Structure = Struct.new :attribute

