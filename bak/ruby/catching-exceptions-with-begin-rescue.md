---
metaTitle: "Ruby - Catching Exceptions with Begin / Rescue"
description: "A Basic Error Handling Block, Saving the Error, Checking for Different Errors, Retrying, Checking Whether No Error Was Raised, Code That Should Always Run"
---

# Catching Exceptions with Begin / Rescue



## A Basic Error Handling Block


Let's make a function to divide two numbers, that's very trusting about its input:

```ruby
def divide(x, y)
  return x/y
end

```

This will work fine for a lot of inputs:

```ruby
> puts divide(10, 2)
5

```

But not all

```ruby
> puts divide(10, 0)
ZeroDivisionError: divided by 0

> puts divide(10, 'a')
TypeError: String can't be coerced into Fixnum

```

We can rewrite the function by wrapping the risky division operation in a `begin... end` block to check for errors, and use a `rescue` clause to output a message and return `nil` if there is a problem.

```ruby
def divide(x, y)
  begin
    return x/y
  rescue
    puts "There was an error"
    return nil
  end
end

> puts divide(10, 0)
There was an error

> puts divide(10, 'a')
There was an error

```



## Saving the Error


You can save the error if you want to use it in the `rescue` clause

```ruby
def divide(x, y)
  begin
    x/y
  rescue => e
    puts "There was a %s (%s)" % [e.class, e.message]
    puts e.backtrace
  end
end

> divide(10, 0)
There was a ZeroDivisionError (divided by 0)
       from (irb):10:in `/'
       from (irb):10
       from /Users/username/.rbenv/versions/2.3.1/bin/irb:11:in `<main>'

> divide(10, 'a')
There was a TypeError (String can't be coerced into Fixnum)
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/workspace.rb:87:in `eval'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/workspace.rb:87:in `evaluate'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/context.rb:380:in `evaluate'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb.rb:489:in `block (2 levels) in eval_input'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb.rb:623:in `signal_status'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb.rb:486:in `block in eval_input'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/ruby-lex.rb:246:in `block (2 levels) in each_top_level_statement'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/ruby-lex.rb:232:in `loop'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/ruby-lex.rb:232:in `block in each_top_level_statement'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/ruby-lex.rb:231:in `catch'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb/ruby-lex.rb:231:in `each_top_level_statement'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb.rb:485:in `eval_input'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb.rb:395:in `block in start'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb.rb:394:in `catch'
/Users/username/.rbenv/versions/2.3.1/lib/ruby/2.3.0/irb.rb:394:in `start'
/Users/username/.rbenv/versions/2.3.1/bin/irb:11:in `<main>'

```



## Checking for Different Errors


If you want to do different things based on the kind of error, use multiple `rescue` clauses, each with a different error type as an argument.

```ruby
def divide(x, y)
  begin
    return x/y
  rescue ZeroDivisionError
    puts "Don't divide by zero!"
    return nil
  rescue TypeError
    puts "Division only works on numbers!"
    return nil
  end
end

> divide(10, 0)
Don't divide by zero!

> divide(10, 'a')
Division only works on numbers!

```

If you want to save the error for use in the `rescue` block:

```ruby
rescue ZeroDivisionError => e

```

Use a `rescue` clause with no argument to catch errors of a type not specified in another `rescue` clause.

```ruby
def divide(x, y)
  begin
    return x/y
  rescue ZeroDivisionError
    puts "Don't divide by zero!"
    return nil
  rescue TypeError
    puts "Division only works on numbers!"
    return nil
  rescue => e
    puts "Don't do that (%s)" % [e.class]
    return nil
  end
end

> divide(nil, 2)
Don't do that (NoMethodError)

```

In this case, trying to divide `nil` by 2 is not a `ZeroDivisionError` or a `TypeError`, so it handled by the default `rescue` clause, which prints out a message to let us know that it was a `NoMethodError`.



## Retrying


In a `rescue` clause, you can use `retry` to run the `begin` clause again, presumably after changing the circumstance that caused the error.

```ruby
def divide(x, y)
  begin
    puts "About to divide..."
    return x/y
  rescue ZeroDivisionError
    puts "Don't divide by zero!"
    y = 1
    retry
  rescue TypeError
    puts "Division only works on numbers!"
    return nil
  rescue => e
    puts "Don't do that (%s)" % [e.class]
    return nil
  end
end

```

If we pass parameters that we know will cause a `TypeError`, the `begin` clause is executed (flagged here by printing out "About to divide") and the error is caught as before, and `nil` is returned:

```ruby
> divide(10, 'a')
About to divide...
Division only works on numbers!
 => nil

```

But if we pass parameters that will cause a `ZeroDivisionError`, the `begin` clause is executed, the error is caught, the divisor changed from 0 to 1, and then `retry` causes the `begin` block to be run again (from the top), now with a different `y`. The second time around there is no error and the function returns a value.

```ruby
> divide(10, 0)
About to divide...     # First time, 10 ÷ 0
Don't divide by zero!
About to divide...     # Second time 10 ÷ 1
=> 10

```



## Checking Whether No Error Was Raised


You can use an `else` clause for code that will be run if no error is raised.

```ruby
def divide(x, y)
  begin
    z = x/y
  rescue ZeroDivisionError
    puts "Don't divide by zero!"
  rescue TypeError
    puts "Division only works on numbers!"
    return nil
  rescue => e
    puts "Don't do that (%s)" % [e.class]
    return nil
  else
    puts "This code will run if there is no error."
    return z
  end
end

```

The `else` clause does not run if there is an error that transfers control to one of the `rescue` clauses:

```ruby
> divide(10,0)
Don't divide by zero!
=> nil

```

But if no error is raised, the `else` clause executes:

```ruby
> divide(10,2)
This code will run if there is no error.
=> 5

```

Note that the `else` clause will not be executed **if you return from the `begin` clause**

```ruby
def divide(x, y)
  begin
    z = x/y
    return z                 # Will keep the else clause from running!
  rescue ZeroDivisionError
    puts "Don't divide by zero!"
  else
    puts "This code will run if there is no error."
    return z
  end
end

> divide(10,2)
=> 5

```



## Code That Should Always Run


Use an `ensure` clause if there is code you always want to execute.

```ruby
def divide(x, y)
  begin
    z = x/y
    return z
  rescue ZeroDivisionError
    puts "Don't divide by zero!"
  rescue TypeError
    puts "Division only works on numbers!"
    return nil
  rescue => e
    puts "Don't do that (%s)" % [e.class]
    return nil
  ensure
    puts "This code ALWAYS runs."
  end
end

```

The `ensure` clause will be executed when there is an error:

```ruby
> divide(10, 0)
Don't divide by zero!   # rescue clause
This code ALWAYS runs.   # ensure clause
=> nil

```

And when there is no error:

```ruby
> divide(10, 2)
This code ALWAYS runs.   # ensure clause
=> 5

```

The ensure clause is useful when you want to make sure, for instance, that files are closed.

Note that, unlike the `else` clause, the `ensure` clause **is executed** before the `begin` or `rescue` clause returns a value. If the `ensure` clause has a `return` that will override the `return` value of any other clause!

