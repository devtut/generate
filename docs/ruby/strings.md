---
metaTitle: "Ruby - Strings"
description: "Difference between single-quoted and double-quoted String literals, Creating a String, Case manipulation, String concatenation, Splitting a String, Positioning strings, Joining Strings, String starts with, String interpolation, String ends with, Multiline strings, Formatted strings, String character replacements, Understanding the data in a string, String Substitution"
---

# Strings




## Difference between single-quoted and double-quoted String literals


The main difference is that double-quoted `String` literals support string interpolations and the full set of escape sequences.

For instance, they can include arbitrary Ruby expressions via interpolation:

```ruby
# Single-quoted strings don't support interpolation
puts 'Now is #{Time.now}'
# Now is #{Time.now}

# Double-quoted strings support interpolation
puts "Now is #{Time.now}"
# Now is 2016-07-21 12:43:04 +0200

```

Double-quoted strings also support the [entire set of escape sequences](https://github.com/ruby/ruby/blob/trunk/doc/syntax/literals.rdoc#strings) including `"\n"`, `"\t"`...

```ruby
puts 'Hello\nWorld'
# Hello\nWorld

puts "Hello\nWorld"
# Hello
# World

```

... while single-quoted strings support **no** escape sequences, baring the minimal set necessary for single-quoted strings to be useful: Literal single quotes and backslashes, `'\''` and `'\\'` respectively.



## Creating a String


Ruby provides several ways to create a [`String`](http://ruby-doc.org/core/String.html) object. The most common way is using single or double quotes to create a "[string literal](http://ruby-doc.org/core/doc/syntax/literals_rdoc.html#label-Strings)":

```ruby
s1 = 'Hello'
s2 = "Hello"

```

The main difference is that double-quoted string literals are a little bit more flexible as they support interpolation and some backslash escape sequences.

There are also several other possible ways to create a string literal using arbitrary string delimiters. An arbitrary string delimiter is a `%` followed by a matching pair of delimiters:

```ruby
%(A string)
%{A string}
%<A string>
%|A string|
%!A string!

```

Finally, you can use the `%q` and `%Q` sequence, that are equivalent to `'` and `"`":

```ruby
puts %q(A string)
# A string
puts %q(Now is #{Time.now})
# Now is #{Time.now}

puts %Q(A string)
# A string
puts %Q(Now is #{Time.now})
# Now is 2016-07-21 12:47:45 +0200

```

`%q` and `%Q` sequences are useful when the string contains either single quotes, double quotes, or a mix of both. In this way, you don't need to escape the content:

```ruby
%Q(<a href="/profile">User's profile<a>)

```

You can use several different delimiters, as long as there is a matching pair:

```ruby
%q(A string)
%q{A string}
%q<A string>
%q|A string|
%q!A string!

```



## Case manipulation


```ruby
"string".upcase     # => "STRING"
"STRING".downcase   # => "string"
"String".swapcase   # => "sTRING"
"string".capitalize # => "String"

```

These four methods do not modify the original receiver. For example,

```ruby
str = "Hello"
str.upcase # => "HELLO"
puts str   # => "Hello"

```

There are four similar methods that perform the same actions but modify original receiver.

```ruby
"string".upcase!     # => "STRING"
"STRING".downcase!   # => "string"
"String".swapcase!   # => "sTRING"
"string".capitalize! # => "String"

```

For example,

```ruby
str = "Hello"
str.upcase!  # => "HELLO"
puts str     # => "HELLO"

```

Notes:

- prior to Ruby 2.4 these methods do not handle unicode.



## String concatenation


Concatenate strings with the `+` operator:

```ruby
s1 = "Hello"
s2 = " "
s3 = "World"

puts s1 + s2 + s3
# => Hello World

s = s1 + s2 + s3
puts s
# => Hello World

```

Or with the `<<` operator:

```ruby
s = 'Hello'
s << ' '
s << 'World'
puts s
# => Hello World

```

Note that the `<<` operator modifies the object on the left hand side.

You also can multiply strings, e.g.

```ruby
"wow" * 3
# => "wowwowwow"

```



## Splitting a String


`String#split` splits a `String` into an `Array`, based on a delimiter.

```ruby
"alpha,beta".split(",")
# => ["alpha", "beta"]

```

An empty `String` results into an empty `Array`:

```ruby
"".split(",")
# => []

```

A non-matching delimiter results in an `Array` containing a single item:

```ruby
"alpha,beta".split(".")
# => ["alpha,beta"]

```

You can also split a string using regular expressions:

```ruby
"alpha, beta,gamma".split(/, ?/)
# => ["alpha", "beta", "gamma"]

```

The delimiter is optional, by default a string is split on whitespace:

```ruby
"alpha beta".split
# => ["alpha", "beta"] 

```



## Positioning strings


In Ruby, strings can be left-justified, right-justified or centered

To left-justify  string, use the `ljust` method. This takes in two parameters, an integer representing the number of characters of the new string and a string, representing the pattern to be filled.

If the integer is greater than the length of the original string, the new string will be left-justified with the optional string parameter taking the remaining space. If the string parameter is not given, the string will be padded with spaces.

```ruby
str ="abcd"
str.ljust(4)          => "abcd"
str.ljust(10)         => "abcd      "

```

To right-justify a string, use the `rjust` method. This takes in two parameters, an integer representing the number of characters of the new string and a string, representing the pattern to be filled.

If the integer is greater than the length of the original string, the new string will be right-justified with the optional string parameter taking the remaining space. If the string parameter is not given, the string will be padded with spaces.

```ruby
str = "abcd"
str.rjust(4)          => "abcd"
str.rjust(10)         => "      abcd"

```

To center a string, use the `center` method. This takes in two parameters, an integer representing the width of the new string and a string, which the original string will be padded with. The string will be aligned to the center.

```ruby
str = "abcd"
str.center(4)          => "abcd"
str.center(10)         => "   abcd   "

```



## Joining Strings


`Array#join` joins an `Array` into a `String`, based on a delimiter:

```ruby
["alpha", "beta"].join(",")
# => "alpha,beta"

```

The delimiter is optional, and defaults to an empty `String`.

```ruby
["alpha", "beta"].join
# => "alphabeta"

```

An empty `Array` results in an empty `String`, no matter which delimiter is used.

```ruby
[].join(",")
# => ""

```



## String starts with


To find if a string starts with a pattern, the `start_with?` method comes in handy

```ruby
str = "zebras are cool"
str.start_with?("zebras")      => true

```

You can also check the position of the pattern with `index`

```ruby
str = "zebras are cool"
str.index("zebras").zero?      => true

```



## String interpolation


The double-quoted delimiter `"` and `%Q` sequence supports string interpolation using `#{ruby_expression}`:

```ruby
puts "Now is #{Time.now}"
# Now is Now is 2016-07-21 12:47:45 +0200

puts %Q(Now is #{Time.now})
# Now is Now is 2016-07-21 12:47:45 +0200

```



## String ends with


To find if a string ends with a pattern, the `end_with?` method comes in handy

```ruby
str = "I like pineapples"
str.end_with?("pineaaples")      => false

```



## Multiline strings


The easiest way to create a multiline string is to just use multiple lines between quotation marks:

```ruby
address = "Four score and seven years ago our fathers brought forth on this
continent, a new nation, conceived in Liberty, and dedicated to the
proposition that all men are created equal."

```

The main problem with that technique is that if the string includes a quotation, it'll break the string syntax. To work around the problem, you can use a [heredoc](https://ruby-doc.org/core-2.3.0/doc/syntax/literals_rdoc.html#label-Here+Documents) instead:

```ruby
puts <<-RAVEN
  Once upon a midnight dreary, while I pondered, weak and weary, 
  Over many a quaint and curious volume of forgotten lore— 
      While I nodded, nearly napping, suddenly there came a tapping, 
  As of some one gently rapping, rapping at my chamber door. 
  "'Tis some visitor," I muttered, "tapping at my chamber door— 
              Only this and nothing more." 
  RAVEN

```

Ruby supports shell-style here documents with `<<EOT`, but the terminating text must start the line. That screws up code indentation, so there's not a lot of reason to use that style. Unfortunately, the string will have indentations depending no how the code itself is indented.

Ruby 2.3 solves the problem by introducing `<<~` which strips out excess leading spaces:

```ruby
def build_email(address)
  return (<<~EMAIL)
    TO: #{address}

    To Whom It May Concern:

    Please stop playing the bagpipes at sunrise!
                     
    Regards,
    Your neighbor               
  EMAIL
end

```

[Percent Strings](https://ruby-doc.org/core-2.3.0/doc/syntax/literals_rdoc.html#label-Percent+Strings) also work to create multiline strings:

```ruby
%q(
HAMLET        Do you see yonder cloud that's almost in shape of a camel?
POLONIUS        By the mass, and 'tis like a camel, indeed.
HAMLET        Methinks it is like a weasel.
POLONIUS        It is backed like a weasel.
HAMLET        Or like a whale?
POLONIUS        Very like a whale
)

```

There are a few ways to avoid interpolation and escape sequences:

<li>
Single quote instead of double quote: `'\n is a carriage return.'`
</li>
<li>
Lower case `q` in a percent string: `%q[#{not-a-variable}]`
</li>
<li>
Single quote the terminal string in a heredoc:

```ruby
<<-'CODE'
   puts 'Hello world!'
CODE

```


</li>



## Formatted strings


Ruby can inject an array of values into a string by replacing any placeholders with the values from the supplied array.

```ruby
"Hello %s, my name is %s!" % ['World', 'br3nt']
# => Hello World, my name is br3nt!

```

The place holders are represented by two `%s` and the values are supplied by the array `['Hello', 'br3nt']`.  The `%` operator instructs the string to inject the values of the array.



## String character replacements


The `tr` method returns a copy of a string where the characters of the first argument are replaced by the characters of the second argument.

```ruby
"string".tr('r', 'l') # => "stling"

```

To replace only the first occurrence of a pattern with with another expression use the `sub` method

```ruby
"string ring".sub('r', 'l') # => "stling ring"

```

If you would like to replace **all** occurrences of a pattern with that expression use `gsub`

```ruby
"string ring".gsub('r','l') # => "stling ling" 

```

To delete characters, pass in an empty string for the second parameter

You can also use regular expressions in all these methods.

It's important to note that these methods will only return a new copy of a string and won't modify the string in place. To do that, you need to use the `tr!`, `sub!` and `gsub!` methods respectively.



## Understanding the data in a string


In Ruby, a string is just a sequence of [bytes](https://en.wikipedia.org/wiki/Byte) along with the name of an encoding (such as `UTF-8`, `US-ASCII`, `ASCII-8BIT`) that specifies how you might interpret those bytes as characters.

Ruby strings can be used to hold text (basically a sequence of characters), in which case the UTF-8 encoding is usually used.

```ruby
"abc".bytes  # => [97, 98, 99]
"abc".encoding.name  # => "UTF-8"

```

Ruby strings can also be used to hold binary data (a sequence of bytes), in which case the ASCII-8BIT encoding is usually used.

```ruby
[42].pack("i").encoding  # => "ASCII-8BIT"

```

It is possible for the sequence of bytes in a string to not match the encoding, resulting in errors if you try to use the string.

```ruby
"\xFF \xFF".valid_encoding? # => false
"\xFF \xFF".split(' ')      # ArgumentError: invalid byte sequence in UTF-8

```



## String Substitution


```ruby
p "This is %s" % "foo"
# => "This is foo"

p "%s %s %s" % ["foo", "bar", "baz"]
# => "foo bar baz"

p "%{foo} == %{foo}" % {:foo => "foo" }
# => "foo == foo"

```

See [String `%`](http://ruby-doc.org/core-2.3.0/String.html#method-i-25) docs and [Kernel::sprintf](http://ruby-doc.org/core-2.3.1/Kernel.html#method-i-sprintf) for more details.



#### Syntax


- 'A string' // creates a string via single-quoted literal
- "A string" // creates a string via double-quoted literal
- String.new("A string")
- %q(A string) // alternative syntax for creating single quoted strings
- %Q(A string) // alternative syntax for creating double quoted strings

