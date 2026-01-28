---
metaTitle: "Ruby - method_missing"
description: "Catching calls to an undefined method, Use with block, Use with parameter, Using the missing method"
---

# method_missing



## Catching calls to an undefined method


```ruby
class Animal
  def method_missing(method, *args, &block)
    "Cannot call #{method} on Animal"
  end
end

=> Animal.new.say_moo 
> "Cannot call say_moo on Animal"

```



## Use with block


```ruby
class Animal
  def method_missing(method, *args, &block)
    if method.to_s == 'say'
      block.call
    else
      super
    end
  end
end

 => Animal.new.say{ 'moo' }
 => "moo" 

```



## Use with parameter


```ruby
class Animal
  def method_missing(method, *args, &block)
    say, speak = method.to_s.split("_")
    if say == "say" && speak
      return speak.upcase if args.first == "shout"
      speak
    else
      super
    end
  end
end

=> Animal.new.say_moo
=> "moo" 
=> Animal.new.say_moo("shout")
=> "MOO" 

```



## Using the missing method


```ruby
class Animal
  def method_missing(method, *args, &block)
    say, speak = method.to_s.split("_")
    if say == "say"
      speak
    else
      super
    end
  end
end

=> a = Animal.new
=> a.say_moo
=> "moo"
=> a.shout_moo
=> NoMethodError: undefined method `shout_moo'

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|method|The name of the method that has been called (in the above example this is `:say_moo`, note that this is a symbol.
|*args|The arguments passed in to this method. Can be any number, or none
|&block|The block of the method called, this can either be a `do` block, or a `{ }` enclosed block



#### Remarks


Always call super, at the bottom of this function. This saves silent failure when something is called and you don't get an error.

For example, this method_missing is going to cause problems:

```ruby
class Animal
  def method_missing(method, *args, &block)
    say, speak = method.to_s.split("_")
    if say == "say"
      speak
    end
  end
end

=> Animal.new.foobar
=> nil # This should really be raising an error

```

`method_missing` is a good tool to use when appropriate, but has two costs you should consider.  First, `method_missing` is less efficient -- ruby must search the class and all of its ancestors before it can fall back on this approach; this performance penalty may be trivial in a simple case, but can add up.  Second and more broadly, this is a form of meta-programming that has great power that comes with responsibility to ensure that the implementation is secure, properly handles malicious inputs, unexpected inputs, and so on.

You should also override `respond_to_missing?` like so:

```ruby
class Animal
  def respond_to_missing?(method, include_private = false)
    method.to_s.start_with?("say_") || super
  end
end

=> Animal.new.respond_to?(:say_moo) # => true

```

