---
metaTitle: "Ruby - IRB"
description: "Basic Usage, Starting an IRB session inside a Ruby script"
---

# IRB




## Basic Usage


IRB means "Interactive Ruby Shell", letting us execute ruby expressions from the standart input.

To start, type `irb` into your shell. You can write anything in Ruby, from simple expressions:

```ruby
$ irb
2.1.4 :001 > 2+2
=> 4

```

to complex cases like methods:

```ruby
2.1.4 :001> def method
2.1.4 :002?>   puts "Hello World"
2.1.4 :003?> end
=> :method
2.1.4 :004 > method
Hello World
=> nil

```



## Starting an IRB session inside a Ruby script


As of Ruby 2.4.0, you can start an interactive IRB session inside any Ruby script using these lines:

```ruby
require 'irb'
binding.irb

```

This will start an IBR REPL where you will have the expected value for `self` and you will be able to access all local variables and instance variables that are in scope.  Type Ctrl+D or `quit` in order to resume your Ruby program.

This can be very useful for debugging.



#### Parameters


|Option|Details
|---|---|---|---|---|---|---|---|---|---
|-f|Suppress read of ~/.irbrc
|-m|Bc mode (load mathn, fraction or matrix are available)
|-d|Set $DEBUG to true (same as `ruby -d')
|-r load-module|Same as `ruby -r'
|-I path|Specify $LOAD_PATH directory
|-U|Same as `ruby -U`
|-E enc|Same as `ruby -E`
|-w|Same as `ruby -w`
|-W[level=2]|Same as `ruby -W`
|--inspect|Use `inspect' for output (default except for bc mode)
|--noinspect|Don't use inspect for output
|--readline|Use Readline extension module
|--noreadline|Don't use Readline extension module
|--prompt prompt-mode|Switch prompt mode. Pre-defined prompt modes are `default',`simple', `xmp' and`inf-ruby'
|--inf-ruby-mode|Use prompt appropriate for inf-ruby-mode on emacs. Suppresses --readline.
|--simple-prompt|Simple prompt mode
|--noprompt|No prompt mode
|--tracer|Display trace for each execution of commands.
|--back-trace-limit n|Display backtrace top n and tail n. The default value is 16.
|--irb_debug n|Set internal debug level to n (not for popular use)
|-v, --version|Print the version of irb

