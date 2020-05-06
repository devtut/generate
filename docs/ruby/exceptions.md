---
metaTitle: "Ruby - Exceptions"
description: "Creating a custom exception type, Handling multiple exceptions, Handling an exception, Raising an exception, Adding information to (custom) exceptions"
---

# Exceptions



## Creating a custom exception type


A custom exception is any class that extends `Exception` or a subclass of `Exception`.

In general, you should always extend `StandardError` or a descendant. The `Exception` family are usually for virtual-machine or system errors, rescuing them can prevent a forced interruption from working as expected.

```ruby
# Defines a new custom exception called FileNotFound
class FileNotFound < StandardError
end

def read_file(path)
  File.exist?(path) || raise(FileNotFound, "File #{path} not found")
  File.read(path)
end

read_file("missing.txt")  #=> raises FileNotFound.new("File `missing.txt` not found")
read_file("valid.txt")    #=> reads and returns the content of the file

```

It's common to name exceptions by adding the `Error` suffix at the end:

- `ConnectionError`
- `DontPanicError`

However, when the error is self-explanatory, you don't need to add the `Error` suffix because would be redundant:

- `FileNotFound` vs `FileNotFoundError`
- `DatabaseExploded` vs `DatabaseExplodedError`



## Handling multiple exceptions


You can handle multiple errors in the same `rescue` declaration:

```ruby
begin
  # an execution that may fail
rescue FirstError, SecondError => e
  # do something if a FirstError or SecondError occurs
end

```

You can also add multiple `rescue` declarations:

```ruby
begin
  # an execution that may fail
rescue FirstError => e
  # do something if a FirstError occurs
rescue SecondError => e
  # do something if a SecondError occurs
rescue => e
  # do something if a StandardError occurs
end

```

The order of the `rescue` blocks is relevant: the first match is the one executed. Therefore, if you put `StandardError` as the first condition and all your exceptions inherit from `StandardError`, then the other `rescue` statements will never be executed.

```ruby
begin
  # an execution that may fail
rescue => e
  # this will swallow all the errors
rescue FirstError => e
  # do something if a FirstError occurs
rescue SecondError => e
  # do something if a SecondError occurs
end

```

Some blocks have implicit exception handling like `def`, `class`, and `module`. These blocks allow you to skip the `begin` statement.

```ruby
def foo
    ...
rescue CustomError
    ...
ensure
    ...
end

```



## Handling an exception


Use the `begin/rescue` block to catch (rescue) an exception and handle it:

```ruby
begin
  # an execution that may fail
rescue
  # something to execute in case of failure
end

```

A `rescue` clause is analogous to a `catch` block in a curly brace language like C# or Java.

A bare `rescue` like this rescues `StandardError`.

Note: Take care to avoid catching `Exception` instead of the default `StandardError`. The `Exception` class includes `SystemExit` and `NoMemoryError` and other serious exceptions that you usually don't want to catch. Always consider catching `StandardError` (the default) instead.

You can also specify the exception class that should be rescued:

```ruby
begin
  # an excecution that may fail
rescue CustomError
  # something to execute in case of CustomError
  # or descendant
end

```

This rescue clause will not catch any exception that is not a `CustomError`.

You can also store the exception in a specific variable:

```ruby
begin
  # an excecution that may fail
rescue CustomError => error
  # error contains the exception
  puts error.message # provide human-readable details about what went wrong.
  puts error.backtrace.inspect # return an array of strings that represent the call stack
end

```

If you failed to handle an exception, you can raise it any time in a rescue block.

```ruby
begin
   #here goes your code
rescue => e
    #failed to handle 
    raise e
end

```

If you want to retry your `begin` block, call `retry`:

```ruby
begin
   #here goes your code
rescue StandardError => e
   #for some reason you want to retry you code
   retry
end

```

You can be stuck in a loop if you catch an exception in every retry. To avoid this, limit your `retry_count` to a certain number of tries.

```ruby
retry_count = 0
begin
      # an excecution that may fail
rescue
    if retry_count < 5
        retry_count = retry_count + 1
        retry
    else
        #retry limit exceeds, do something else
    end

```

You can also provide an `else` block or an `ensure` block. An `else` block will be executed when the `begin` block completes without an exception thrown. An `ensure` block will always be executed. An `ensure` block is analogous to a `finally` block in a curly brace language like C# or Java.

```ruby
begin
  # an execution that may fail
rescue
  # something to execute in case of failure
else
  # something to execute in case of success
ensure
  # something to always execute
end

```

If you are inside a `def`, `module` or `class` block, there is no need to use the begin statement.

```ruby
def foo
    ...
rescue
    ...
end

```



## Raising an exception


To raise an exception use `Kernel#raise` passing the exception class and/or message:

```ruby
raise StandardError # raises a StandardError.new
raise StandardError, "An error" # raises a StandardError.new("An error")

```

You can also simply pass an error message. In this case, the message is wrapped into a `RuntimeError`:

```ruby
raise "An error" # raises a RuntimeError.new("An error")

```

Here's an example:

```ruby
def hello(subject)
  raise ArgumentError, "`subject` is missing" if subject.to_s.empty?
  puts "Hello #{subject}"
end

hello # => ArgumentError: `subject` is missing
hello("Simone") # => "Hello Simone"

```



## Adding information to (custom) exceptions


It may be helpful to include additional information with an exception, e.g. for logging purposes or to allow conditional handling when the exception is caught:

```ruby
class CustomError < StandardError
  attr_reader :safe_to_retry

  def initialize(safe_to_retry = false, message = 'Something went wrong')
    @safe_to_retry = safe_to_retry
    super(message)
  end
end

```

Raising the exception:

```ruby
raise CustomError.new(true)

```

Catching the exception and accessing the additional information provided:

```ruby
begin
  # do stuff
rescue CustomError => e
  retry if e.safe_to_retry
end

```



#### Remarks


An **exception** is an object that represents the occurrence of an exceptional condition. In other words, it indicates that something went wrong.

In Ruby, **exceptions** are often referred to as **errors**. That's because the base `Exception` class exists as a top-level exception object element, but user-defined execution exceptions are generally `StandardError` or descendants.

