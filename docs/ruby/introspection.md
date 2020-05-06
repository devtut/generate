---
metaTitle: "Ruby - Introspection"
description: "View an object's methods, View an object's Instance Variables, View Global and Local Variables, View Class Variables"
---

# Introspection



## View an object's methods


### Inspecting an Object

You can find the public methods an object can respond to using either the `methods` or `public_methods` methods, which return an array of symbols:

```ruby
class Foo
  def bar; 42; end
end
f = Foo.new
def f.yay; 17; end
p f.methods.sort
#=> [:!, :!=, :!~, :<=>, :==, :===, :=~, :__id__, :__send__, :bar, :class, :clone,
#=>  :define_singleton_method, :display, :dup, :enum_for, :eql?, :equal?, :extend,
#=>  :freeze, :frozen?, :hash, :inspect, :instance_eval, :instance_exec,
#=>  :instance_of?, :instance_variable_defined?, :instance_variable_get,
#=>  :instance_variable_set, :instance_variables, :is_a?, :itself, :kind_of?, 
#=>  :method, :methods, :nil?, :object_id, :private_methods, :protected_methods,
#=>  :public_method, :public_methods, :public_send, :remove_instance_variable,
#=>  :respond_to?, :send, :singleton_class, :singleton_method, :singleton_methods,
#=>  :taint, :tainted?, :tap, :to_enum, :to_s, :trust, :untaint, :untrust,
#=>  :untrusted?, :yay]

```

For a more targeted list, you can remove methods common to all objects, e.g.

```ruby
p (f.methods - Object.methods).sort
#=> [:bar,:yay]

```

Alternatively, you can pass `false` to `methods` or `public_methods`:

```ruby
p f.methods(false) # public and protected singleton methods of `f`
#=> [:yay]

p f.public_methods(false)
#=> [:yay, :bar]

```

You can find the private and protected methods of an object using `private_methods` and `protected_methods`:

```ruby
p f.private_methods.sort
#=> [:Array, :Complex, :DelegateClass, :Float, :Hash, :Integer, :Rational, :String,
#=>  :__callee__, :__dir__, :__method__, :`, :abort, :at_exit, :autoload, :autoload?,
#=>  :binding, :block_given?, :caller, :caller_locations, :catch,
#=>  :default_src_encoding, :eval, :exec, :exit, :exit!, :fail, :fork, :format, :gem,
#=>  :gem_original_require, :gets, :global_variables, :initialize, :initialize_clone,
#=>  :initialize_copy, :initialize_dup, :irb_binding, :iterator?, :lambda, :load,
#=>  :local_variables, :loop, :method_missing, :open, :p, :print, :printf, :proc,
#=>  :putc, :puts, :raise, :rand, :readline, :readlines, :require, :require_relative,
#=>  :respond_to_missing?, :select, :set_trace_func, :singleton_method_added,
#=>  :singleton_method_removed, :singleton_method_undefined, :sleep, :spawn,
#=>  :sprintf, :srand, :syscall, :system, :test, :throw, :trace_var, :trap,
#=>  :untrace_var, :warn]

p f.protected_methods
#=> []

```

As with `methods` and `public_methods`, you can pass `false` to `private_methods` and `protected_methods` to trim away inherited methods.

### Inspecting a Class or Module

In addition to `methods`, `public_methods`, `protected_methods`, and `private_methods`, classes and modules expose `instance_methods`, `public_instance_methods`, `protected_instance_methods`, and `private_instance_methods` to determine the methods exposed for objects that inherit from the class or module. As above, you can pass `false` to these methods to exclude inherited methods:

```ruby
p Foo.instance_methods.sort
#=> [:!, :!=, :!~, :<=>, :==, :===, :=~, :__id__, :__send__, :bar, :class,
#=>  :clone, :define_singleton_method, :display, :dup, :enum_for, :eql?,
#=>  :equal?, :extend, :freeze, :frozen?, :hash, :inspect, :instance_eval,
#=>  :instance_exec, :instance_of?, :instance_variable_defined?,
#=>  :instance_variable_get, :instance_variable_set, :instance_variables,
#=>  :is_a?, :itself, :kind_of?, :method, :methods, :nil?, :object_id,
#=>  :private_methods, :protected_methods, :public_method, :public_methods,
#=>  :public_send, :remove_instance_variable, :respond_to?, :send,
#=>  :singleton_class, :singleton_method, :singleton_methods, :taint,
#=>  :tainted?, :tap, :to_enum, :to_s, :trust, :untaint, :untrust, :untrusted?]

p Foo.instance_methods(false)
#=> [:bar]

```

Finally, if you forget the names of most of these in the future, you can find all of these methods using `methods`:

```ruby
p f.methods.grep(/methods/)
#=> [:private_methods, :methods, :protected_methods, :public_methods,
#=>  :singleton_methods]

p Foo.methods.grep(/methods/)
#=> [:public_instance_methods, :instance_methods, :private_instance_methods,
#=>  :protected_instance_methods, :private_methods, :methods,
#=>  :protected_methods, :public_methods, :singleton_methods]

```



## View an object's Instance Variables


It is possible to query an object about its instance variables using `instance_variables`, `instance_variable_defined?`, and `instance_variable_get`, and modify them using `instance_variable_set` and `remove_instance_variable`:

```ruby
class Foo
  attr_reader :bar
  def initialize
    @bar = 42
  end
end
f = Foo.new
f.instance_variables                #=> [:@bar]
f.instance_variable_defined?(:@baz) #=> false
f.instance_variable_defined?(:@bar) #=> true
f.instance_variable_get(:@bar)      #=> 42
f.instance_variable_set(:@bar, 17)   #=> 17
f.bar                               #=> 17
f.remove_instance_variable(:@bar)   #=> 17
f.bar                               #=> nil
f.instance_variables                #=> []

```

The names of instance variables include the `@` symbol. You will get an error if you omit it:

```ruby
f.instance_variable_defined?(:jim)
#=> NameError: `jim' is not allowed as an instance variable name

```



## View Global and Local Variables


The `Kernel` exposes methods for getting the list of [`global_variables`](http://ruby-doc.org/core-2.3.1/Kernel.html#method-i-global_variables) and [`local_variables`](http://ruby-doc.org/core-2.3.1/Kernel.html#method-i-local_variables):

```ruby
cats  = 42
$demo = "in progress"
p global_variables.sort
#=> [:$!, :$", :$$, :$&, :$', :$*, :$+, :$,, :$-0, :$-F, :$-I, :$-K, :$-W, :$-a,
#=>  :$-d, :$-i, :$-l, :$-p, :$-v, :$-w, :$., :$/, :$0, :$1, :$2, :$3, :$4, :$5,
#=>  :$6, :$7, :$8, :$9, :$:, :$;, :$<, :$=, :$>, :$?, :$@, :$DEBUG, :$FILENAME,
#=>  :$KCODE, :$LOADED_FEATURES, :$LOAD_PATH, :$PROGRAM_NAME, :$SAFE, :$VERBOSE,
#=>  :$\, :$_, :$`, :$binding, :$demo, :$stderr, :$stdin, :$stdout, :$~]

p local_variables
#=> [:cats]

```

Unlike instance variables there are no methods specifically for getting, setting, or removing global or local variables. Looking for such functionality is usually a sign that your code should be rewritten to use a Hash to store the values. However, if you must modify global or local variables by name, you can use `eval` with a string:

```ruby
var = "$demo"
eval(var)           #=> "in progress"
eval("#{var} = 17")
p $demo             #=> 17

```

By default, `eval` will evaluate your variables in the current scope. To evaluate local variables in a different scope, you must capture the **binding** where the local variables exist.

```ruby
def local_variable_get(name, bound=nil)
  foo = :inside
  eval(name,bound)
end

def test_1
  foo = :outside
  p local_variable_get("foo")
end

def test_2
  foo = :outside
  p local_variable_get("foo",binding)
end
  
test_1 #=> :inside
test_2 #=> :outside

```

In the above, `test_1` did not pass a binding to `local_variable_get`, and so the `eval` was executed within the context of that method, where a local variable named `foo` was set to `:inside`.



## View Class Variables


Classes and modules have the same methods for introspecting instance variables as any other object. Class and modules also have similar methods for querying the class variables (`@@these_things`):

```ruby
p Module.methods.grep(/class_variable/)
#=> [:class_variables, :class_variable_get, :remove_class_variable,
#=>  :class_variable_defined?, :class_variable_set]

class Foo
  @@instances = 0
  def initialize
    @@instances += 1
  end
end

class Bar < Foo; end

5.times{ Foo.new }
3.times{ Bar.new }
p Foo.class_variables                   #=> [:@@instances]
p Bar.class_variables                   #=> [:@@instances]
p Foo.class_variable_get(:@@instances)  #=> 8
p Bar.class_variable_get(:@@instances)  #=> 8

```

Similar to instance variables, the name of class variables must begin with `@@`, or you will get an error:

```ruby
p Bar.class_variable_defined?( :instances )
#=> NameError: `instances' is not allowed as a class variable name

```

