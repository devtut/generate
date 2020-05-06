---
metaTitle: "Ruby - Regular Expressions and Regex Based Operations"
description: "=~ operator, Regular Expressions in Case Statements, Groups, named and otherwise., Quantifiers, Character classes, Defining a Regexp, match? - Boolean Result, Common quick usage"
---

# Regular Expressions and Regex Based Operations



## =~ operator


```ruby
if /hay/ =~ 'haystack'
  puts "There is hay in the word haystack"
end

```

**Note:** The order **is significant**. Though `'haystack' =~ /hay/` is in most cases an equivalent, side effects might differ:

- Strings captured from named capture groups are assigned to local variables only when `Regexp#=~` is called ([`regexp =~ str`](http://ruby-doc.org/core-2.3.1/String.html#method-i-3D-7E));
- Since the right operand might be is an arbitrary object, for `regexp =~ str` there will be called either `Regexp#=~` or `String#=~`.

Note that this does not return a true/false value, it instead returns either the index of the match if found, or nil if not found. Because all integers in ruby are truthy (including 0) and nil is falsy, this works. If you want a boolean value, use `#===` as shown in [another example](http://stackoverflow.com/documentation/ruby/1357/regular-expressions-and-regex-based-operations/4424/simple-regex-match#t=201607221936139047493).



## Regular Expressions in Case Statements


You can test if a string matches several regular expressions using a switch statement.

### Example

```ruby
case "Ruby is #1!"
when /\APython/
    puts "Boooo."
when /\ARuby/
    puts "You are right."
else 
    puts "Sorry, I didn't understand that."
end

```

This works because case statements are checked for equality using the `===` operator, not the `==` operator. When a regex is on the left hand side of a comparison using `===`, it will test a string to see if it matches.



## Groups, named and otherwise.


Ruby extends the standard group syntax `(...)` with a named group, `(?<name>...)`. This allows for extraction by name instead of having to count how many groups you have.

```ruby
name_reg = /h(i|ello), my name is (?<name>.*)/i #i means case insensitive

name_input = "Hi, my name is Zaphod Beeblebrox"

match_data = name_reg.match(name_input) #returns either a MatchData object or nil
match_data = name_input.match(name_reg) #works either way

if match_data.nil? #Always check for nil! Common error.
  puts "No match"
else
  match[0] #=> "Hi, my name is Zaphod Beeblebrox"
  match[1] #=> "i" #the first group, (i|ello)
  match[2] #=> "Zaphod Beeblebrox"
  #Because it was a named group, we can get it by name
  match[:name]  #=> "Zaphod Beeblebrox"
  match["name"] #=> "Zaphod Beeblebrox"
  puts "Hello #{match[:name]}!"
end

```

The index of the match is counted based on the order of the left parentheses (with the entire regex being the first group at index 0)

```ruby
reg = /(((a)b)c)(d)/
match = reg.match 'abcd'
match[0] #=> "abcd"
match[1] #=> "abc"
match[2] #=> "ab"
match[3] #=> "a"
match[4] #=> "d"

```



## Quantifiers


Quantifiers allows to specify count of repeated strings.

<li>
Zero or one:

```ruby
/a?/

```


</li>
<li>
Zero or many:

```ruby
/a*/

```


</li>
<li>
One or many:

```ruby
/a+/

```


</li>
<li>
Exact number:

```ruby
/a{2,4}/ # Two, three or four
/a{2,}/  # Two or more
/a{,4}/  # Less than four (including zero)

```


</li>

By default, [quantifiers are greedy](https://ruby-doc.org/core-2.1.0/Regexp.html#class-Regexp-label-Repetition), which means they take as many characters as they can while still making a match. Normally this is not noticeable:

```ruby
/(?<site>.*) Stack Exchange/ =~ 'Motor Vehicle Maintenance & Repair Stack Exchange'

```

The named capture group `site` will be set to ''Motor Vehicle Maintenance & Repair' as expected. But if 'Stack Exchange' is an optional part of the string (because it could be 'Stack Overflow' instead), the naive solution will not work as expected:

```ruby
/(?<site>.*)( Stack Exchange)?/

```

This version will still match, but the named capture will include 'Stack Exchange' since `*` greedily eats those characters. The solution is to add another question mark to make the `*` lazy:

```ruby
/(?<site>.*?)( Stack Exchange)?/

```

**Appending `?` to any quantifier will make it lazy.**



## Character classes


Describes ranges of symbols

You can enumerate symbols explicitly

```ruby
/[abc]/ # 'a' or 'b' or 'c'

```

Or use ranges

```ruby
/[a-z]/ # from 'a' to 'z'

```

It is possible to combine ranges and single symbols

```ruby
/[a-cz]/ # 'a' or 'b' or 'c' or 'z'

```

Leading dash (`-`) is treated as charachter

```ruby
/[-a-c]/ # '-' or 'a' or 'b' or 'c'

```

Classes can be negative when preceding symbols with `^`

```ruby
/[^a-c]/ # Not 'a', 'b' or 'c'

```

There are some shortcuts for widespread classes and special charachters, plus line endings

```ruby
^  # Start of line
$  # End of line
\A # Start of string
\Z # End of string, excluding any new line at the end of string
\z # End of string
.  # Any single character
\s # Any whitespace character
\S # Any non-whitespace character
\d # Any digit
\D # Any non-digit
\w # Any word character (letter, number, underscore)
\W # Any non-word character
\b # Any word boundary

```

`\n` will be understood simply as new line

To escape any reserved charachter, such as `/` or `[]` and others use backslash (left slash)

```ruby
\\ # => \
\[\] # => []

```



## Defining a Regexp


A Regexp can be created in three different ways in Ruby.

<li>
using slashes: `/ /`
</li>
<li>
using `%r{}`
</li>
<li>
using `Regex.new`

```ruby
#The following forms are equivalent
regexp_slash = /hello/
regexp_bracket = %r{hello}
regexp_new = Regexp.new('hello')

string_to_match = "hello world!"

#All of these will return a truthy value
string_to_match =~ regexp_slash    # => 0
string_to_match =~ regexp_bracket  # => 0
string_to_match =~ regexp_new      # => 0

```


</li>



## match? - Boolean Result


Returns `true` or `false`, which indicates whether the regexp is matched or not without updating `$~` and other related variables. If the second parameter is present, it specifies the position in the string to begin the search.

```ruby
/R.../.match?("Ruby")    #=> true
/R.../.match?("Ruby", 1) #=> false
/P.../.match?("Ruby")    #=> false

```

Ruby 2.4+



## Common quick usage


Regular expressions are often used in methods as parameters to check if other strings are present or to search and/or replace strings.

You'll often see the following:

```ruby
string = "My not so long string"
string[/so/] # gives so
string[/present/] # gives nil
string[/present/].nil? # gives true

```

So you can simply use this as a check if a string contains a substring

```ruby
puts "found" if string[/so/]

```

More advanced but still short and quick: search for a specific group by using the second parameter, 2 is the second in this example because numbering starts at 1 and not 0, a group is what is enclosed in parentheses.

```ruby
string[/(n.t).+(l.ng)/, 2] # gives long

```

Also often used: search and replace with `sub` or `gsub`, `\1` gives the first found group, `\2` the second:

```ruby
string.gsub(/(n.t).+(l.ng)/, '\1 very \2') # My not very long string

```

The last result is remembered and can be used on the following lines

```ruby
$2 # gives long

```

