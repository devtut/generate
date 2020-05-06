---
metaTitle: "Ruby - Recursion in Ruby"
description: "Tail recursion, Recursive function"
---

# Recursion in Ruby




## Tail recursion


Many recursive algorithms can be expressed using iteration. For instance, the greatest common denominator function can be [written recursively](https://en.wikipedia.org/wiki/Recursion_(computer_science)#Recursive_procedures):

```ruby
def gdc (x, y)
  return x if y == 0
  return gdc(y, x%y)
end

```

or iteratively:

```ruby
def gdc_iter (x, y)
  while y != 0 do
    x, y = y, x%y
  end

  return x
end

```

The two algorithms are equivalent in theory, but the recursive version risks a [SystemStackError](https://ruby-doc.org/core/SystemStackError.html). However, since the recursive method ends with a call to itself, it could be optimized to avoid a stack overflow. Another way to put it: the recursive algorithm can result in the same machine code as the iterative **if** the compiler knows to look for the recursive method call at the end of the method. Ruby doesn't do tail call optimization by default, but you can [turn it on with](https://ruby-doc.org/core/RubyVM/InstructionSequence.html#method-c-compile_option):

```ruby
RubyVM::InstructionSequence.compile_option = {
  tailcall_optimization: true,
  trace_instruction: false
}

```

In addition to turning on tail-call optimization, you also need to turn off instruction tracing. Unfortunately, these options only apply at compile time, so you either need to `require` the recursive method from another file or `eval` the method definition:

```ruby
RubyVM::InstructionSequence.new(<<-EOF).eval
  def me_myself_and_i
    me_myself_and_i
  end
EOF
me_myself_and_i # Infinite loop, not stack overflow

```

Finally, the final return call must return the method and **only the method**. That means you'll need to re-write the standard factorial function:

```ruby
def fact(x)
  return 1 if x <= 1
  return x*fact(x-1)
end

```

To something like:

```

def fact(x, acc=1)
   return acc if x <= 1
   return fact(x-1, x*acc)
 end

```

This version passes the accumulated sum via a second (optional) argument that [defaults](http://stackoverflow.com/documentation/ruby/997/methods/3243/default-parameters#t=201705072356272378973) to 1.

Further reading: [Tail Call Optimization in Ruby](http://nithinbekal.com/posts/ruby-tco/) and [Tailin' Ruby](http://timelessrepo.com/tailin-ruby).



## Recursive function


Let's start with a simple algorithm to see how recursion could be implemented in Ruby.

A bakery has products to sell. Products are in packs. It services orders in packs only. Packaging starts from the largest pack size and then the remaining quantities are filled by next pack sizes available.

For e.g. If an order of 16 is received, bakery allocates 2 from 5 pack and 2 from 3 pack. 2**5+2**3 = 16. Let's see how this is implemented in recursion. "allocate" is the recursive function here.

```ruby
#!/usr/bin/ruby

class Bakery
  attr_accessor :selected_packs

  def initialize
    @packs = [5,3] # pack sizes 5 and 3
    @selected_packs = []
  end

  def allocate(qty)
    remaining_qty = nil

    # ==============================================
    # packs are allocated in large packs first order
    # to minimize the packaging space
    # ==============================================
    @packs.each do |pack|
      remaining_qty = qty - pack

      if remaining_qty > 0
        ret_val = allocate(remaining_qty)
        if ret_val == 0
          @selected_packs << pack
          remaining_qty = 0
          break
        end
      elsif remaining_qty == 0
        @selected_packs << pack
        break
      end
    end

    remaining_qty
  end
end

bakery = Bakery.new
bakery.allocate(16)
puts "Pack combination is: #{bakery.selected_packs.inspect}"

```

Output is:

> 
Pack combination is: [3, 3, 5, 5]


