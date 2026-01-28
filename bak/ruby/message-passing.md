---
metaTitle: "Ruby - Message Passing"
description: "Introduction, Message Passing Through Inheritance Chain, Message Passing Through Module Composition, Interrupting Messages"
---

# Message Passing



## Introduction


In **Object Oriented Design**, objects **receive** messages and **reply** to them. In Ruby, sending a message is **calling a method** and result of that method is the reply.

In Ruby message passing is dynamic. When a message arrives rather than knowing exactly how to reply to it Ruby uses a predefined set of rules to find a method that can reply to it. We can use these rules to interrupt and reply to the message, send it to another object or modify it among other actions.

Each time an object receives a message Ruby checks:

1. If this object has a singleton class and it can reply to this message.
1. Looks up this object's class then class' ancestors chain.
1. One by one checks if a method is available on this ancestor and moves up the chain.



## Message Passing Through Inheritance Chain


```ruby
class Example
  def example_method
    :example
  end

  def subexample_method
    :example
  end

  def not_missed_method
    :example
  end

  def method_missing name
    return :example if name == :missing_example_method
    return :example if name == :missing_subexample_method
    return :subexample if name == :not_missed_method
    super
  end
end

class SubExample < Example
  def subexample_method
    :subexample
  end

  def method_missing name
    return :subexample if name == :missing_subexample_method
    return :subexample if name == :not_missed_method
    super
  end
end

s = Subexample.new

```

To find a suitable method for `SubExample#subexample_method` Ruby first looks at ancestors chain of `SubExample`

```ruby
SubExample.ancestors # => [SubExample, Example, Object, Kernel, BasicObject]

```

It starts from `SubExample`. If we send `subexample_method` message Ruby chooses the one available one SubExample and ignores `Example#subexample_method`.

```ruby
s.subexample_method # => :subexample

```

After `SubExample` it checks `Example`. If we send `example_method` Ruby checks if `SubExample` can reply to it or not and since it can't Ruby goes up the chain and looks into `Example`.

```ruby
s.example_method # => :example

```

After Ruby checks all defined methods then it runs `method_missing` to see if it can reply or not. If we send `missing_subexample_method` Ruby won't be able to find a defined method on `SubExample` so it moves up to `Example`. It can't find a defined method on `Example` or any other class higher in chain either. Ruby starts over and runs `method_missing`. `method_missing` of `SubExample` can reply to `missing_subexample_method`.

```ruby
s.missing_subexample_method # => :subexample

```

However if a method is defined Ruby uses defined version even if it is higher in the chain. For example if we send `not_missed_method` even though `method_missing` of `SubExample` can reply to it Ruby walks up on `SubExample` because it doesn't have a defined method with that name and looks into `Example` which has one.

```ruby
s.not_missed_method # => :example

```



## Message Passing Through Module Composition


Ruby moves up on ancestors chain of an object. This chain can contain both modules and classes. Same rules about moving up the chain apply to modules as well.

```ruby
class Example
end

module Prepended
  def initialize *args
    return super :default if args.empty?
    super
  end
end

module FirstIncluded
  def foo
    :first
  end
end

module SecondIncluded
  def foo
    :second
  end
end

class SubExample < Example
  prepend Prepended
  include FirstIncluded
  include SecondIncluded

  def initialize data = :subexample
    puts data
  end
end

SubExample.ancestors # => [Prepended, SubExample, SecondIncluded, FirstIncluded, Example, Object, Kernel, BasicObject]

s = SubExample.new # => :default
s.foo # => :second

```



## Interrupting Messages


There are two ways to interrupt messages.

- Use `method_missing` to interrupt any non defined message.
- Define a method in middle of a chain to intercept the message

After interrupting messages, it is possible to:

- Reply to them.
- Send them somewhere else.
- Modify the message or its result.

Interrupting via `method_missing` and replying to message:

```ruby
class Example
  def foo
    @foo
  end

  def method_missing name, data
    return super unless name.to_s =~ /=$/
    name = name.to_s.sub(/=$/, "")
    instance_variable_set "@#{name}", data
  end
end

e = Example.new

e.foo = :foo
e.foo # => :foo

```

Intercepting message and modifying it:

```ruby
class Example
  def initialize title, body
  end
end

class SubExample < Example
end

```

Now let's imagine our data is "title:body" and we have to split them before calling `Example`. We can define `initialize` on `SubExample`.

```ruby
class SubExample < Example
  def initialize raw_data
    processed_data = raw_data.split ":"

    super processed_data[0], processed_data[1]
  end
end

```

Intercepting message and sending it to another object:

```ruby
class ObscureLogicProcessor
  def process data
    :ok
  end
end

class NormalLogicProcessor
  def process data
    :not_ok
  end
end

class WrapperProcessor < NormalLogicProcessor
  def process data
    return ObscureLogicProcessor.new.process data if data.obscure?

    super
  end
end

```

