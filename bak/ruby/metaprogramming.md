---
metaTitle: "Ruby - Metaprogramming"
description: "Implementing with using instance evaluation, send() method, Defining methods dynamically, Defining methods on instances"
---

# Metaprogramming


**Metaprogramming** can be described in two ways:

“Computer programs that write or manipulate other programs (or themselves) as their data, or that do part of the work at compile time that would otherwise be done at runtime”.

More simply put: **Metaprogramming is writing code that writes code during runtime to make your life easier**.



## Implementing "with" using instance evaluation


Many languages feature a `with` statement that allows programmers to omit the receiver of method calls.

`with` can be easily emulated in Ruby using [`instance_eval`](http://ruby-doc.org/core/BasicObject.html#method-i-instance_eval):

```ruby
def with(object, &block)
  object.instance_eval &block
end

```

The `with` method can be used to seamlessly execute methods on objects:

```ruby
hash = Hash.new

with hash do
  store :key, :value
  has_key? :key       # => true
  values              # => [:value]
end

```



## send() method


`send()` is used to pass message to `object`. `send()` is an instance method of the `Object` class.
The first argument in `send()` is the message that you're sending to the object - that is, the name of a method. It could be `string` or `symbol` but **symbols** are preferred. Then arguments those need to pass in method, those will be the remaining arguments in `send()`.

```ruby
class Hello
  def hello(*args)
    puts 'Hello ' + args.join(' ')
  end
end
h = Hello.new
h.send :hello, 'gentle', 'readers'   #=> "Hello gentle readers"
# h.send(:hello, 'gentle', 'readers') #=> Here :hello is method and rest are the arguments to method.

```

### Here is the more descriptive example

```ruby
class Account
  attr_accessor :name, :email, :notes, :address

  def assign_values(values)
    values.each_key do |k, v|
      # How send method would look a like
      # self.name = value[k]
      self.send("#{k}=", values[k])
    end
  end
end

user_info = {
  name: 'Matt',
  email: 'test@gms.com',
  address: '132 random st.',
  notes: "annoying customer"
}

account = Account.new
If attributes gets increase then we would messup the code
#--------- Bad way --------------
account.name = user_info[:name]
account.address = user_info[:address]
account.email = user_info[:email]
account.notes = user_info[:notes]

# --------- Meta Programing way --------------
account.assign_values(user_info) # With single line we can assign n number of attributes

puts account.inspect

```

Note: `send()` itself is not recommended anymore. Use `__send__()` which has the power to call private methods, or (recommended) `public_send()`



## Defining methods dynamically


With Ruby you can modify the structure of the program in execution time. One way to do it, is by defining methods dynamically using the method `method_missing`.

Let's say that we want to be able to test if a number is greater than other number with the syntax `777.is_greater_than_123?`.

```ruby
# open Numeric class
class Numeric
  # override `method_missing`
  def method_missing(method_name,*args)
    # test if the method_name matches the syntax we want
    if method_name.to_s.match /^is_greater_than_(\d+)\?$/
      # capture the number in the method_name
      the_other_number = $1.to_i
      # return whether the number is greater than the other number or not
      self > the_other_number
    else
      # if the method_name doesn't match what we want, let the previous definition of `method_missing` handle it
      super
    end
  end
end

```

One important thing to remember when using `method_missing` that one should also override `respond_to?` method:

```ruby
class Numeric
   def respond_to?(method_name, include_all = false) 
     method_name.to_s.match(/^is_greater_than_(\d+)\?$/) || super
   end
end

```

Forgetting to do so leads to a inconsistent situation, when you can successfully call `600.is_greater_than_123`, but `600.respond_to(:is_greater_than_123)` returns false.



## Defining methods on instances


In ruby you can add methods to existing instances of any class. This allows you to add behavior to and instance of a class without changing the behavior of the rest of the instances of that class.

```ruby
class Example
  def method1(foo)
    puts foo
  end
end

#defines method2 on object exp
exp = Example.new
exp.define_method(:method2) {puts "Method2"}

#with method parameters
exp.define_method(:method3) {|name| puts name}

```

