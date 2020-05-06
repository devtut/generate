---
metaTitle: "Ruby - Comments"
description: "Single & Multiple line comments"
---

# Comments



## Single & Multiple line comments


Comments are programmer-readable annotations that are ignored at runtime. Their purpose is to make source code easier to understand.

**Single line comments**

The `#` character is used to add single line comments.

```ruby
#!/usr/bin/ruby -w
# This is a single line comment.
puts "Hello World!"

```

When executed, the above program will output `Hello World!`

**Multiline comments**

Multiple-line comments can be added by using `=begin` and `=end` syntax (also known as the comment block markers) as follows:

```ruby
#!/usr/bin/ruby -w
=begin
This is a multiline comment.
Write as many line as you want. 
=end
puts "Hello World!"

```

When executed, the above program will output `Hello World!`

