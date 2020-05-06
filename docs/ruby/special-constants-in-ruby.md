---
metaTitle: "Ruby - Special Constants in Ruby"
description: "__FILE__, __dir__, $PROGRAM_NAME or $0, $$, $1, $2, etc, ARGV or $*, STDIN, STDOUT, STDERR, $stderr, $stdout, $stdin, ENV"
---

# Special Constants in Ruby



## __FILE__


Is the relative path to the file from the current execution directory<br />
Assume we have this directory structure: /home/stackoverflow/script.rb<br />
script.rb contains:

```ruby
puts __FILE__

```

If you are inside /home/stackoverflow and execute the script like `ruby script.rb` then  `__FILE__` will output `script.rb`
If you are inside /home then it will output `stackoverflow/script.rb`

Very useful to get the path of the script in versions prior to 2.0 where `__dir__` doesn't exist.

**Note**
`__FILE__` is not equal to `__dir__`



## __dir__


`__dir__` is not a constant but a function<br />
`__dir__` is equal to `File.dirname(File.realpath(__FILE__))`



## $PROGRAM_NAME or $0


Contains the name of the script being executed.<br />
Is the same as `__FILE__` if you are executing that script.



## $$


The process number of the Ruby running this script



## $1, $2, etc


Contains the subpattern from the corresponding set of parentheses in the last successful pattern matched, not counting patterns matched in nested blocks that have been exited already, or nil if the last pattern match failed. These variables are all read-only.



## ARGV or $*


Command line arguments given for the script. The options for Ruby interpreter are already removed.



## STDIN


The standard input. The default value for $stdin



## STDOUT


The standard output. The default value for $stdout



## STDERR


The standard error output. The default value for $stderr



## $stderr


The current standard error output.



## $stdout


The current standard output



## $stdin


The current standard input



## ENV


The hash-like object contains current environment variables. Setting a value in ENV changes the environment for child processes.

