---
metaTitle: "Ruby - Introspection in Ruby"
description: "Lets see some examples, Introspection of class"
---

# Introspection in Ruby


**What is introspection?**

Introspection is looking inward to know about the inside. That is a simple definition of introspection.

In programming and Ruby in general…introspection is the ability to look at object, class… at run time to know about that one.



## Lets see some examples


Example:

```ruby
s = "Hello"  # s is a string

```

Then we find out something about s. Lets begin:

So you want to know what is the class of s at run time?

```ruby
irb(main):055:0* s.class
=> String

```

Ohh, good. But what are the methods of s?

```ruby
irb(main):002:0> s.methods
=> [:unicode_normalize, :include?, :to_c, :unicode_normalize!, :unicode_normalized?, :%, :*, :+, :count, :partition, :unpack, :encode, :encode!, :next, :casecmp, :insert, :bytesize, :match, :succ!, :next!, :upto, :index, :rindex, :replace, :clear, :chr, :+@, :-@, :setbyte, :getbyte, :<=>, :<<, :scrub, :scrub!, :byteslice, :==, :===, :dump, :=~, :downcase, :[], :[]=, :upcase, :downcase!, :capitalize, :swapcase, :upcase!, :oct, :empty?, :eql?, :hex, :chars, :split, :capitalize!, :swapcase!, :concat, :codepoints, :reverse, :lines, :bytes, :prepend, :scan, :ord, :reverse!, :center, :sub, :freeze, :inspect, :intern, :end_with?, :gsub, :chop, :crypt, :gsub!, :start_with?, :rstrip, :sub!, :ljust, :length, :size, :strip!, :succ, :rstrip!, :chomp, :strip, :rjust, :lstrip!, :tr!, :chomp!, :squeeze, :lstrip, :tr_s!, :to_str, :to_sym, :chop!, :each_byte, :each_char, :each_codepoint, :to_s, :to_i, :tr_s, :delete, :encoding, :force_encoding, :sum, :delete!, :squeeze!, :tr, :to_f, :valid_encoding?, :slice, :slice!, :rpartition, :each_line, :b, :ascii_only?, :hash, :to_r, :<, :>, :<=, :>=, :between?, :instance_of?, :public_send, :instance_variable_get, :instance_variable_set, :instance_variable_defined?, :remove_instance_variable, :private_methods, :kind_of?, :instance_variables, :tap, :is_a?, :extend, :to_enum, :enum_for, :!~, :respond_to?, :display, :object_id, :send, :method, :public_method, :singleton_method, :define_singleton_method, :nil?, :class, :singleton_class, :clone, :dup, :itself, :taint, :tainted?, :untaint, :untrust, :trust, :untrusted?, :methods, :protected_methods, :frozen?, :public_methods, :singleton_methods, :!, :!=, :__send__, :equal?, :instance_eval, :instance_exec, :__id__]

```

You want to know if s is an instance of String?

```ruby
irb(main):017:0*
irb(main):018:0* s.instance_of?(String)
=> true

```

What are the public methods of s?

```ruby
irb(main):026:0* s.public_methods
=> [:unicode_normalize, :include?, :to_c, :unicode_normalize!, :unicode_normalized?, :%, :*, :+, :count, :partition, :unpack, :encode, :encode!, :next, :casecmp, :insert, :bytesize, :match, :succ!, :next!, :upto, :index, :rindex, :replace, :clear, :chr, :+@, :-@, :setbyte, :getbyte, :<=>, :<<, :scrub, :scrub!, :byteslice, :==, :===, :dump, :=~, :downcase, :[], :[]=, :upcase, :downcase!, :capitalize, :swapcase, :upcase!, :oct, :empty?, :eql?, :hex, :chars, :split, :capitalize!, :swapcase!, :concat, :codepoints, :reverse, :lines, :bytes, :prepend, :scan, :ord, :reverse!, :center, :sub, :freeze, :inspect, :intern, :end_with?, :gsub, :chop, :crypt, :gsub!, :start_with?, :rstrip, :sub!, :ljust, :length, :size, :strip!, :succ, :rstrip!, :chomp, :strip, :rjust, :lstrip!, :tr!, :chomp!, :squeeze, :lstrip, :tr_s!, :to_str, :to_sym, :chop!, :each_byte, :each_char, :each_codepoint, :to_s, :to_i, :tr_s, :delete, :encoding, :force_encoding, :sum, :delete!, :squeeze!, :tr, :to_f, :valid_encoding?, :slice, :slice!, :rpartition, :each_line, :b, :ascii_only?, :hash, :to_r, :<, :>, :<=, :>=, :between?, :pretty_print, :pretty_print_cycle, :pretty_print_instance_variables, :pretty_print_inspect, :instance_of?, :public_send, :instance_variable_get, :instance_variable_set, :instance_variable_defined?, :remove_instance_variable, :private_methods, :kind_of?, :instance_variables, :tap, :pretty_inspect, :is_a?, :extend, :to_enum, :enum_for, :!~, :respond_to?, :display, :object_id, :send, :method, :public_method, :singleton_method, :define_singleton_method, :nil?, :class, :singleton_class, :clone, :dup, :itself, :taint, :tainted?, :untaint, :untrust, :trust, :untrusted?, :methods, :protected_methods, :frozen?, :public_methods, :singleton_methods, :!, :!=, :__send__, :equal?, :instance_eval, :instance_exec, :__id__]

```

and private methods....

```ruby
irb(main):030:0* s.private_methods
=> [:initialize, :initialize_copy, :DelegateClass, :default_src_encoding, :irb_binding, :sprintf, :format, :Integer, :Float, :String, :Array, :Hash, :catch, :throw, :loop, :block_given?, :Complex, :set_trace_func, :trace_var, :untrace_var, :at_exit, :Rational, :caller, :caller_locations, :select, :test, :fork, :exit, :`, :gem_original_require, :sleep, :pp, :respond_to_missing?, :load, :exec, :exit!, :system, :spawn, :abort, :syscall, :printf, :open, :putc, :print, :readline, :puts, :p, :srand, :readlines, :gets, :rand, :proc, :lambda, :trap, :initialize_clone, :initialize_dup, :gem, :require, :require_relative, :autoload, :autoload?, :binding, :local_variables, :warn, :raise, :fail, :global_variables, :__method__, :__callee__, :__dir__, :eval, :iterator?, :method_missing, :singleton_method_added, :singleton_method_removed, :singleton_method_undefined]

```

Yes, do s have a method name upper. You want to get the upper case version of s?
Lets try:

```ruby
irb(main):044:0> s.respond_to?(:upper)
=> false

```

Look like not, the correct method is upcase lets check:

```ruby
irb(main):047:0*
irb(main):048:0* s.respond_to?(:upcase)
=> true

```



## Introspection of class


Lets following are the class definition

What are the instance methods of `C`?

`C.instance_methods # [:c, :b, :a, :to_json, :instance_of?...]`

What are the instance methods that declare only on `C`?

`C.instance_methods(false) # [:c]`

What are the ancestors of class `C`?

`C.ancestors # [C, B, A, Object,...]`

Superclass of `C`?

`C.superclass # A`

