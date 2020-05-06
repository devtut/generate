---
metaTitle: "Ruby - Debugging"
description: "Stepping through code with Pry and Byebug"
---

# Debugging



## Stepping through code with Pry and Byebug


First, you need to install `pry-byebug` gem. Run this command:

```ruby
$ gem install pry-byebug

```

Add this line at the top of your `.rb` file:

```ruby
require 'pry-byebug'

```

Then insert this line wherever you want a breakpoint:

```ruby
binding.pry

```

A `hello.rb` example:

```ruby
require 'pry-byebug'

def hello_world
  puts "Hello"
  binding.pry # break point here
  puts "World"
end

```

When you run the `hello.rb` file, the program will pause at that line. You can then step through your code with the `step` command. Type a variable's name to learn its value. Exit the debugger with `exit-program` or `!!!`.

