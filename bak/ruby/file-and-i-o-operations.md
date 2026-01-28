---
metaTitle: "Ruby - File and I/O Operations"
description: "Writing a string to a file, Open and closing a file, get a single char of input, Reading from STDIN, Reading from arguments with ARGV"
---

# File and I/O Operations




## Writing a string to a file


A string can be written to a file with an instance of the `File` class.

```ruby
file = File.new('tmp.txt', 'w')
file.write("NaNaNaNa\n")
file.write('Batman!\n')
file.close

```

The `File` class also offers a shorthand for the `new` and `close` operations with the `open` method.

```ruby
File.open('tmp.txt', 'w') do |f|
  f.write("NaNaNaNa\n")
  f.write('Batman!\n')
end

```

For simple write operations, a string can be also written directly to a file with `File.write`. **Note that this will overwrite the file by default.**

```ruby
File.write('tmp.txt', "NaNaNaNa\n" * 4 + 'Batman!\n')

```

To specify a different mode on `File.write`, pass it as the value of a key called `mode` in a hash as another parameter.

```ruby
File.write('tmp.txt', "NaNaNaNa\n" * 4 + 'Batman!\n', { mode: 'a'})

```



## Open and closing a file


Manually open and close a file.

```ruby
# Using new method
f = File.new("test.txt", "r") # reading
f = File.new("test.txt", "w") # writing
f = File.new("test.txt", "a") # appending

# Using open method
f = open("test.txt", "r")

# Remember to close files
f.close

```

Automatically close a file using a block.

```ruby
f = File.open("test.txt", "r") do |f|
  # do something with file f
  puts f.read # for example, read it
end

```



## get a single char of input


Unlike `gets.chomp` this will not wait for a newline.

First part of the stdlib must be included

```ruby
require 'io/console'

```

Then a helper method can be written:

```ruby
def get_char
  input = STDIN.getch
  control_c_code = "\u0003"
  exit(1) if input == control_c_code
  input
end

```

Its' imporant to exit if `control+c` is pressed.



## Reading from STDIN


```ruby
# Get two numbers from STDIN, separated by a newline, and output the result
number1 = gets
number2 = gets
puts number1.to_i + number2.to_i
## run with: $ ruby a_plus_b.rb
## or:       $ echo -e "1\n2" | ruby a_plus_b.rb

```



## Reading from arguments with ARGV


```ruby
number1 = ARGV[0]
number2 = ARGV[1]
puts number1.to_i + number2.to_i
## run with: $ ruby a_plus_b.rb 1 2

```



#### Parameters


|Flag|Meaning
|---|---|---|---|---|---|---|---|---|---
|"r"|Read-only, starts at beginning of file  (default mode).
|"r+"|Read-write, starts at beginning of file.
|"w"|Write-only, truncates existing file to zero length or creates a new file for writing.
|"w+"|Read-write, truncates existing file to zero length or creates a new file for reading and writing.
|"a"|Write-only, starts at end of file if file exists, otherwise creates a new file for writing.
|"a+"|Read-write, starts at end of file if file exists, otherwise creates a new file for reading and writing.
|"b"|Binary file mode. Suppresses EOL <-> CRLF conversion on Windows. And sets external encoding to ASCII-8BIT unless explicitly specified. (This flag may only appear in conjunction with the above flags. For example, `File.new("test.txt", "rb")` would open `test.txt` in `read-only` mode as a `binary` file.)
|"t"|Text file mode. (This flag may only appear in conjunction with the above flags. For example, `File.new("test.txt", "wt")` would open `test.txt` in `write-only` mode as a `text` file.)

