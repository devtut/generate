---
metaTitle: "Ruby - Control Flow"
description: "if, elsif, else and end, Case statement, Truthy and Falsy values, Inline if/unless, while, until, Or-Equals/Conditional assignment operator (||=), Flip-Flop operator, unless, throw, catch, Ternary operator, Loop control with break, next, and redo, return vs. next: non-local return in a block, Control flow with logic statements, begin, end"
---

# Control Flow



## if, elsif, else and end


Ruby offers the expected `if` and `else` expressions for branching logic, terminated by the `end` keyword:

```ruby
# Simulate flipping a coin
result = [:heads, :tails].sample

if result == :heads
  puts 'The coin-toss came up "heads"'
else
  puts 'The coin-toss came up "tails"'
end

```

In Ruby, `if` statements are expressions that evaluate to a value, and the result can be assigned to a variable:

```ruby
status = if age < 18
           :minor
         else
           :adult
         end

```

Ruby also offers C-style ternary operators ([see here for details](http://stackoverflow.com/documentation/ruby/640/control-flow/10907/ternary-operator#t=201607270911323570258)) that can be expressed as:

```ruby
some_statement ? if_true : if_false  

```

This means the above example using if-else can also be written as

```ruby
status = age < 18 ? :minor : :adult

```

Additionally, Ruby offers the `elsif` keyword which accepts an expression to enables additional branching logic:

```ruby
label = if shirt_size == :s
          'small'
        elsif shirt_size == :m
          'medium'
        elsif shirt_size == :l
          'large'
        else
          'unknown size'
        end

```

If none of the conditions in an `if`/`elsif` chain are true, and there is no `else` clause, then the expression evaluates to nil.  This can be useful inside string interpolation, since `nil.to_s` is the empty string:

```ruby
"user#{'s' if @users.size != 1}"

```



## Case statement


Ruby uses the `case` keyword for switch statements.

As per the [Ruby Docs](http://ruby-doc.org/docs/keywords/1.9/Object.html#method-i-case):

> 
<p>Case statements consist of an optional condition, which is in the
position of an argument to `case`, and zero or more `when` clauses.
The first `when` clause to match the condition (or to evaluate to
Boolean truth, if the condition is null) “wins”, and its code stanza
is executed. The value of the case statement is the value of the
successful `when` clause, or `nil` if there is no such clause.</p>
<p>A case statement can end with an `else` clause. Each `when` a
statement can have multiple candidate values, separated by commas.</p>


Example:

```ruby
case x
when 1,2,3
  puts "1, 2, or 3"
when 10
  puts "10"
else
  puts "Some other number"
end

```

Shorter version:

```ruby
case x
when 1,2,3 then puts "1, 2, or 3"
when 10 then puts "10"
else puts "Some other number"
end

```

The value of the `case` clause is matched with each `when` clause using the `===` method (not `==`). Therefore it can be used with a variety of different types of objects.

A `case` statement can be used with [Ranges](http://ruby-doc.org/core/Range.html):

```ruby
case 17
when 13..19
  puts "teenager"
end

```

A `case` statement can be used with a [Regexp](http://ruby-doc.org/core/Regexp.html):

```ruby
case "google"
when /oo/
  puts "word contains oo"
end

```

A `case` statement can be used with a [Proc](http://ruby-doc.org/core/Proc.html) or lambda:

```ruby
case 44
when -> (n) { n.even? or n < 0 }
  puts "even or less than zero"
end

```

A `case` statement can be used with [Classes](http://ruby-doc.org/core/Class.html):

```ruby
case x
when Integer
  puts "It's an integer"
when String
  puts "It's a string"
end

```

By implementing the `===` method you can create your own match classes:

```ruby
class Empty
  def self.===(object)
    !object or "" == object
  end
end

case ""
when Empty
  puts "name was empty"
else
  puts "name is not empty"
end

```

A `case` statement can be used without a value to match against:

```ruby
case
when ENV['A'] == 'Y'
  puts 'A'
when ENV['B'] == 'Y'
  puts 'B'
else
  puts 'Neither A nor B'
end

```

A `case` statement has a value, so you can use it as a method argument or in an assignment:

```ruby
description = case 16
              when 13..19 then "teenager"
              else ""
              end

```



## Truthy and Falsy values


In Ruby, there are exactly two values which are considered "falsy", and will return false when tested as a condition for an `if` expression. They are:

- `nil`
- boolean `false`

**All** other values are considered "truthy", including:

- `0` - numeric zero (Integer or otherwise)
- `""` - Empty strings
- `"\n"` - Strings containing only whitespace
- `[]` - Empty arrays
- `{}` - Empty hashes

Take, for example, the following code:

```ruby
def check_truthy(var_name, var)
  is_truthy = var ? "truthy" : "falsy"
  puts "#{var_name} is #{is_truthy}"
end

check_truthy("false", false)
check_truthy("nil", nil)
check_truthy("0", 0)
check_truthy("empty string", "")
check_truthy("\\n", "\n")
check_truthy("empty array", [])
check_truthy("empty hash", {})

```

Will output:

```ruby
false is falsy
nil is falsy
0 is truthy
empty string is truthy
\n is truthy
empty array is truthy
empty hash is truthy

```



## Inline if/unless


A common pattern is to use an inline, or trailing, `if` or `unless`:

```ruby
puts "x is less than 5" if x < 5

```

This is known as a conditional **modifier**, and is a handy way of adding simple guard code and early returns:

```ruby
def save_to_file(data, filename)
  raise "no filename given" if filename.empty?
  return false unless data.valid?

  File.write(filename, data)
end

```

It is not possible to add an `else` clause to these modifiers. Also it is generally not recommended to use conditional modifiers inside the main logic -- For complex code one should use normal `if`, `elsif`, `else` instead.



## while, until


A `while` loop executes the block while the given condition is met:

```ruby
i = 0
while i < 5
  puts "Iteration ##{i}"
  i +=1
end

```

An `until` loop executes the block while the conditional is false:

```ruby
i = 0
until i == 5
  puts "Iteration ##{i}"
  i +=1
end

```



## Or-Equals/Conditional assignment operator (||=)


Ruby has an or-equals operator that allows a value to be assigned to a variable if and only if that variable evaluates to either `nil` or `false`.

```

||= # this is the operator that achieves this. 

```

this operator with the double pipes representing or and the equals sign representing assigning of a value. You may think it represents something like this:

```

x = x || y

```

this above example is not correct. The or-equals operator actually represents this:

```

x || x = y

```

If `x` evaluates to `nil` or `false` then `x` is assigned the value of `y`, and left unchanged otherwise.

Here is a practical use-case of the or-equals operator. Imagine you have a portion of your code that is expected to send an email to a user. What do you do if for what ever reason there is no email for this user. You might write something like this:

```

if user_email.nil?
    user_email = "error@yourapp.com"
 end

```

Using the or-equals operator we can cut this entire chunk of code, providing clean, clear control and functionality.

```

user_email ||= "error@yourapp.com"

```

In cases where `false` is a valid value, care must be taken to not override it accidentally:

```ruby
has_been_run = false
has_been_run ||= true
#=> true

has_been_run = false
has_been_run = true if has_been_run.nil?
#=> false

```



## Flip-Flop operator


The flip flop operator `..` is used between two conditions in a conditional statement:

```ruby
(1..5).select do |e|
  e if (e == 2) .. (e == 4)
end
# => [2, 3, 4]

```

The condition evaluates to `false` **until** the first part becomes `true`. Then it evaluates to `true` **until** the second part becomes `true`. After that it switches to `false` again.

This example illustrates what is being selected:

```ruby
[1, 2, 2, 3, 4, 4, 5].select do |e|
  e if (e == 2) .. (e == 4)
end
# => [2, 2, 3, 4]

```

The flip-flop operator only works inside ifs (including `unless`) and ternary operator. Otherwise it is being considered as the range operator.

```ruby
(1..5).select do |e|
  (e == 2) .. (e == 4)
end
# => ArgumentError: bad value for range

```

It can switch from `false` to `true` and backwards multiple times:

```ruby
((1..5).to_a * 2).select do |e|
  e if (e == 2) .. (e == 4)
end
# => [2, 3, 4, 2, 3, 4] 

```



## unless


A common statement is `if !(some condition)`. Ruby offers the alternative of the `unless` statement.

The structure is exactly the same as an `if` statement, except the condition is negative. Also, the `unless` statement does not support `elsif`, but it does support `else`:

```ruby
# Prints not inclusive
unless 'hellow'.include?('all')
  puts 'not inclusive'
end

```



## throw, catch


Unlike many other programming languages, the `throw` and `catch` keywords are not related to exception handling in Ruby.

In Ruby, `throw` and `catch` act a bit like labels in other languages. They are used to change the control flow, but are not related to a concept of "error" like Exceptions are.

```ruby
catch(:out) do
  catch(:nested) do
    puts "nested"
  end

  puts "before"
  throw :out
  puts "will not be executed"
end
puts "after"
# prints "nested", "before", "after"

```



## Ternary operator


Ruby has a ternary operator (`?:`), which returns one of two value based on if a condition evaluates as truthy:

```ruby
conditional ? value_if_truthy : value_if_falsy

value = true
value ? "true" : "false"
#=> "true"

value = false
value ? "true" : "false"
#=> "false"

```

it is the same as writing `if a then b else c end`, though the ternary is preferred

Examples:

```ruby
puts (if 1 then 2 else 3 end) # => 2

puts 1 ? 2 : 3                # => 2

x = if 1 then 2 else 3 end
puts x                        # => 2

```



## Loop control with break, next, and redo


The flow of execution of a Ruby block may be controlled with the `break`, `next`, and `redo` statements.

### `break`

The `break` statement will exit the block immediately.  Any remaining instructions in the block will be skipped, and the iteration will end:

```ruby
actions = %w(run jump swim exit macarena)
index = 0

while index < actions.length
  action = actions[index]

  break if action == "exit"

  index += 1
  puts "Currently doing this action: #{action}"
end

# Currently doing this action: run
# Currently doing this action: jump
# Currently doing this action: swim

```

### `next`

The `next` statement will return to the top of the block immediately, and proceed with the next iteration.  Any remaining instructions in the block will be skipped:

```ruby
actions = %w(run jump swim rest macarena)
index = 0

while index < actions.length
  action = actions[index]
  index += 1

  next if action == "rest"

  puts "Currently doing this action: #{action}"
end

# Currently doing this action: run
# Currently doing this action: jump
# Currently doing this action: swim
# Currently doing this action: macarena

```

### `redo`

The `redo` statement will return to the top of the block immediately, and retry the same iteration.  Any remaining instructions in the block will be skipped:

```ruby
actions = %w(run jump swim sleep macarena)
index = 0
repeat_count = 0

while index < actions.length
  action = actions[index]
  puts "Currently doing this action: #{action}"

  if action == "sleep"
    repeat_count += 1
    redo if repeat_count < 3
  end

  index += 1
end

# Currently doing this action: run
# Currently doing this action: jump
# Currently doing this action: swim
# Currently doing this action: sleep
# Currently doing this action: sleep
# Currently doing this action: sleep
# Currently doing this action: macarena

```

### `Enumerable` iteration

In addition to loops, these statements work with Enumerable iteration methods, such as `each` and `map`:

```ruby
[1, 2, 3].each do |item|
  next if item.even?
  puts "Item: #{item}"
end

# Item: 1
# Item: 3

```

### Block result values

In both the `break` and `next` statements, a value may be provided, and will be used as a block result value:

```ruby
even_value = for value in [1, 2, 3]
  break value if value.even?
end

puts "The first even value is: #{even_value}"

# The first even value is: 2

```



## return vs. next: non-local return in a block


Consider this **broken** snippet:

```ruby
def foo
  bar = [1, 2, 3, 4].map do |x|
    return 0 if x.even?
    x
  end
  puts 'baz'
  bar
end
foo # => 0

```

One might expect `return` to yield a value for `map`'s array of block results. So the return value of `foo` would be `[1, 0, 3, 0]`. Instead, **`return` returns a value from the method `foo`**. Notice that `baz` isn't printed, which means execution never reached that line.

`next` with a value does the trick. It acts as a block-level `return`.

```ruby
def foo
  bar = [1, 2, 3, 4].map do |x|
    next 0 if x.even?
    x
  end
  puts 'baz'
  bar
end
foo # baz
    # => [1, 0, 3, 0]

```

In the absence of a `return`, the value returned by the block is the value of its last expression.



## Control flow with logic statements


While it might seem counterintuitive, you can use logical operators to determine whether or not a statement is run. For instance:

```ruby
File.exist?(filename) or STDERR.puts "#{filename} does not exist!"

```

This will check to see if the file exists and only print the error message if it doesn't. The `or` statement is lazy, which means it'll stop executing once it's sure which whether it's value is true or false. As soon as the first term is found to be true, there's no need to check the value of the other term. But if the first term is false, it must check the second term.

A common use is to set a default value:

```ruby
glass = glass or 'full' # Optimist! 

```

That sets the value of `glass` to 'full' if it's not already set. More concisely, you can use the symbolic version of `or`:

```ruby
glass ||= 'empty' # Pessimist. 

```

It's also possible to run the second statement only if the first one is false:

```ruby
File.exist?(filename) and puts "#{filename} found!"

```

Again, `and` is lazy so it will only execute the second statement if necessary to arrive at a value.

The `or` operator has lower precedence than `and`. Similarly, `||` has lower precedence than `&&`. The symbol forms have higher precedence than the word forms. This is handy to know when you want to mix this technique with assignment:

```ruby
a = 1 and b = 2
#=> a==1
#=> b==2

```

```ruby
a = 1 && b = 2; puts a, b
#=> a==2
#=> b==2

```

Note that the Ruby Style Guide [recommends](https://github.com/bbatsov/ruby-style-guide#no-and-or-or):

> 
The `and` and `or` keywords are banned. The minimal added readability is just not worth the high probability of introducing subtle bugs. For boolean expressions, always use `&&` and `||` instead. For flow control, use `if` and `unless`; `&&` and `||` are also acceptable but less clear.




## begin, end


The `begin` block is a control structure that groups together multiple statements.

```ruby
begin
  a = 7
  b = 6
  a * b
end

```

A `begin` block will return the value of the last statement in the block.  The following example will return `3`.

```ruby
begin
  1
  2
  3
end

```

The `begin` block is useful for conditional assignment using the `||=` operator where multiple statements may be required to return a result.

```ruby
circumference ||=
  begin
    radius = 7
    tau = Math::PI * 2
    tau * radius
  end

```

It can also be combined with other block structures such as `rescue`, `ensure`, `while`, `if`, `unless`, etc to provide greater control of program flow.

`Begin` blocks are not code blocks, like `{ ... }` or `do ... end`; they cannot be passed to functions.

