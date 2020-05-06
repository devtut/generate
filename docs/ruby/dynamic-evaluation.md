---
metaTitle: "Ruby - Dynamic Evaluation"
description: "Instance evaluation, Evaluating a String, Evaluating Inside a Binding, Dynamically Creating Methods from Strings"
---

# Dynamic Evaluation



## Instance evaluation


The [`instance_eval`](http://ruby-doc.org/core/BasicObject.html#method-i-instance_eval) method is available on all objects. It evaluates code in the context of the receiver:

```ruby
object = Object.new

object.instance_eval do
  @variable = :value
end

object.instance_variable_get :@variable  # => :value

```

`instance_eval` sets `self` to `object` for the duration of the code block:

```ruby
object.instance_eval { self == object }  # => true

```

The receiver is also passed to the block as its only argument:

```ruby
object.instance_eval { |argument| argument == object }  # => true

```

The [`instance_exec`](http://ruby-doc.org/core/BasicObject.html#method-i-instance_exec) method differs in this regard: it passes its arguments to the block instead.

```ruby
object.instance_exec :@variable do |name|
  instance_variable_get name  # => :value
end

```



## Evaluating a String


Any `String` can be evaluated at runtime.

```ruby
class Example
  def self.foo
    :foo
  end
end

eval "Example.foo" #=> :foo

```



## Evaluating Inside a Binding


Ruby keeps track of local variables and `self` variable via an object called binding. We can get binding of a scope with calling `Kernel#binding` and evaluate string inside a binding via `Binding#eval`.

```ruby
b = proc do
  local_variable = :local
  binding
end.call

b.eval "local_variable" #=> :local

```

```ruby
def fake_class_eval klass, source = nil, &block
  class_binding = klass.send :eval, "binding"

  if block
    class_binding.local_variable_set :_fake_class_eval_block, block
    class_binding.eval "_fake_class_eval_block.call"
  else
    class_binding.eval source
  end
end

class Example
end

fake_class_eval Example, <<-BLOCK
  def self.foo
    :foo
  end
BLOCK

fake_class_eval Example do
  def bar
    :bar
  end
end

Example.foo #=> :foo
Example.new.bar #=> :bar

```



## Dynamically Creating Methods from Strings


Ruby offers [`define_method`](http://ruby-doc.org/core-2.3.1/Module.html#method-i-define_method) as a private method on modules and classes for defining new instance methods. However, the 'body' of the method must be a `Proc` or another existing method.

One way to create a method from raw string data is to use `eval` to create a Proc from the code:

```ruby
xml = <<ENDXML
<methods>
  <method name="go">puts "I'm going!"</method>
  <method name="stop">7*6</method>
</methods>
ENDXML

class Foo
  def self.add_method(name,code)
    body = eval( "Proc.new{ #{code} }" )
    define_method(name,body)
  end
end

require 'nokogiri' # gem install nokogiri
doc = Nokogiri.XML(xml)
doc.xpath('//method').each do |meth|
  Foo.add_method( meth['name'], meth.text )
end

f = Foo.new
p Foo.instance_methods(false)  #=> [:go, :stop]
p f.public_methods(false)      #=> [:go, :stop]
f.go                           #=> "I'm going!"
p f.stop                       #=> 42

```



#### Syntax


- eval "source"
- eval "source", binding
- eval "source", proc
- binding.eval "source" # equal to `eval "source", binding`



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`"source"`|Any Ruby source code
|`binding`|An instance of [`Binding`](http://ruby-doc.org/core-2.2.0/Binding.html) class
|`proc`|An instance of [`Proc`](http://ruby-doc.org/core-2.2.0/Proc.html) class

