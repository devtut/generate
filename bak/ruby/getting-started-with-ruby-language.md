---
metaTitle: "Ruby - Getting started with Ruby Language"
description: "Hello World, Hello World as a Self-Executable File—using Shebang (Unix-like operating systems only), Hello World from IRB, Hello World without source files, Hello World with tk, My First Method"
---

# Getting started with Ruby Language



## Hello World


> 
This example assumes Ruby is installed.


Place the following in a file named `hello.rb`:

```ruby
puts 'Hello World'

```

From the command line, type the following command to execute the Ruby code from the source file:

```ruby
$ ruby hello.rb

```

This should output:

```ruby
Hello World

```

The output will be immediately displayed to the console. Ruby source files don't need to be compiled before being executed. The Ruby interpreter compiles and executes the Ruby file at runtime.



## Hello World as a Self-Executable File—using Shebang (Unix-like operating systems only)


You can add an interpreter directive (shebang) to your script.  Create a file called `hello_world.rb` which contains:

```ruby
#!/usr/bin/env ruby

puts 'Hello World!'

```

Give the script executable permissions.  Here's how to do that in Unix:

```ruby
$ chmod u+x hello_world.rb

```

Now you do not need to call the Ruby interpreter explicitly to run your script.

```ruby
$ ./hello_world.rb

```



## Hello World from IRB


Alternatively, you can use the [Interactive Ruby Shell](http://ruby-doc.org/stdlib-2.3.1/libdoc/irb/rdoc/IRB.html) (IRB) to immediately execute the Ruby statements you previously wrote in the Ruby file.

Start an IRB session by typing:

```ruby
$ irb

```

Then enter the following command:

```ruby
puts "Hello World"

```

This results in the following console output (including newline):

```ruby
Hello World

```

If you don't want to start a new line, you can use `print`:

```ruby
print "Hello World"

```



## Hello World without source files


Run the command below in a shell after installing Ruby.  This shows how you can execute simple Ruby programs without creating a Ruby file:

```ruby
ruby -e 'puts "Hello World"'

```

You can also feed a Ruby program to the interpreter's standard input.  One way to do that is to use a [here document](https://en.wikipedia.org/wiki/Here_document) in your shell command:

```ruby
ruby <<END
puts "Hello World"
END

```



## Hello World with tk


Tk is the standard graphical user interface (GUI) for Ruby. It provides a cross-platform GUI for Ruby programs.

### Example code:

```ruby
require "tk"
TkRoot.new{ title "Hello World!" }
Tk.mainloop

```

**The result:**

[<img src="http://i.stack.imgur.com/Y6sLc.png" alt="Result of Hello World with tk" />](http://i.stack.imgur.com/Y6sLc.png)

**Step by Step explanation:**

```ruby
require "tk"

```

Load the tk package.

```ruby
TkRoot.new{ title "Hello World!" }

```

Define a widget with the title `Hello World`

```ruby
Tk.mainloop

```

Start the main loop and display the widget.



## My First Method


### Overview

Create a new file named `my_first_method.rb`

Place the following code inside the file:

```ruby
def hello_world
  puts "Hello world!"
end

hello_world() # or just 'hello_world' (without parenthesis)

```

Now, from a command line, execute the following:

```ruby
ruby my_first_method.rb

```

The output should be:

```ruby
Hello world!

```

### Explanation

- `def` is a keyword that tells us that we're `def`-ining a method - in this case, `hello_world` is the name of our method.
- `puts "Hello world!"` `puts` (or pipes to the console) the string `Hello world!`
- `end` is a keyword that signifies we're ending our definition of the `hello_world` method
- as the `hello_world` method doesn't accept any arguments, you can omit the parenthesis by invoking the method



#### Remarks


[Ruby](https://www.ruby-lang.org/) is a multi-platform open-source, dynamic object-oriented interpreted language, designed to be simplistic and productive. It was created by Yukihiro Matsumoto (Matz) in 1995.

> 
According to its creator, Ruby was influenced by [Perl](https://en.wikipedia.org/wiki/Perl), [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk), [Eiffel](https://en.wikipedia.org/wiki/Eiffel_(programming_language)), [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)), and [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)). It supports multiple programming paradigms, including functional, object-oriented, and imperative. It also has a dynamic type system and automatic memory management.


