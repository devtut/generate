---
metaTitle: "Ruby - Implicit Receivers and Understanding Self"
description: "There is always an implicit receiver, Keywords change the implicit receiver, When to use self?"
---

# Implicit Receivers and Understanding Self



## There is always an implicit receiver


In Ruby, there is always an implicit receiver for all method calls. The language keeps a reference to the current implicit receiver stored in the variable `self`. Certain language keywords like `class` and `module` will change what `self` points to. Understanding these behaviors is very helpful in mastering the language.

For example, when you first open `irb`

```ruby
irb(main):001:0> self
=> main

```

In this case the `main` object is the implicit receiver (see [http://stackoverflow.com/a/917842/417872](http://stackoverflow.com/a/917842/417872) for more about `main`).

You can define methods on the implicit receiver using the `def` keyword. For example:

```ruby
irb(main):001:0> def foo(arg)
irb(main):002:1> arg.to_s
irb(main):003:1> end
=> :foo
irb(main):004:0> foo 1
=> "1"

```

This has defined the method foo on the instance of main object running in your repl.

Note that local variables are looked up before method names, so that if you define a local variable with the same name, its reference will supersede the method reference. Continuing from the previous example:

```ruby
irb(main):005:0> defined? foo
=> "method"
irb(main):006:0> foo = 1
=> 1
irb(main):007:0> defined? foo
=> "local-variable"
irb(main):008:0> foo
=> 1
irb(main):009:0> method :foo
=> #<Method: Object#foo>

```

The `method` method can still find the `foo` method because it doesn't check for local variables, while the normal reference `foo` does.



## Keywords change the implicit receiver


When you define a class or module, the implicit receiver becomes a reference to the class itself. For example:

```ruby
puts "I am #{self}"
class Example
  puts "I am #{self}"
end

```

Executing the above code will print:

```ruby
"I am main"
"I am Example"

```



## When to use self?


Most Ruby code utilizes the implicit receiver, so programmers who are new to Ruby are often confused about when to use `self`. The practical answer is that `self` is used in two major ways:

**1. To change the receiver.**

Ordinarily the behavior of `def` inside a class or module is to create instance methods. Self can be used to define methods on the class instead.

```ruby
class Foo
  def bar
    1
  end

  def self.bar
    2
  end
end

Foo.new.bar #=> 1
Foo.bar #=> 2

```

**2. To disambiguate the receiver**

When local variables may have the same name as a method an explicit receiver may be required to disambiguate.

Examples:

```ruby
class Example
  def foo
    1
  end

  def bar
    foo + 1
  end

  def baz(foo)
    self.foo + foo # self.foo is the method, foo is the local variable
  end

  def qux
    bar = 2
    self.bar + bar # self.bar is the method, bar is the local variable
  end 
end

Example.new.foo    #=> 1
Example.new.bar    #=> 2
Example.new.baz(2) #=> 3
Example.new.qux    #=> 4

```

The other common case requiring disambiguation involves methods that end in the equals sign. For instance:

```ruby
class Example
  def foo=(input)
    @foo = input
  end

  def get_foo
    @foo
  end

  def bar(input)
    foo = input # will create a local variable
  end

  def baz(input)
    self.foo = input # will call the method
  end
end

e = Example.new
e.get_foo #=> nil
e.foo = 1
e.get_foo #=> 1
e.bar(2)
e.get_foo #=> 1
e.baz(2)
e.get_foo #=> 2

```

