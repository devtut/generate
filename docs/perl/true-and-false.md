---
metaTitle: "Perl - True and false"
description: "List of true and false values"
---

# True and false



## List of true and false values


```perl
use feature qw( say );

# Numbers are true if they're not equal to 0.
say 0             ? 'true' : 'false'; # false
say 1             ? 'true' : 'false'; # true
say 2             ? 'true' : 'false'; # true
say -1            ? 'true' : 'false'; # true
say 1-1           ? 'true' : 'false'; # false
say 0e7           ? 'true' : 'false'; # false
say -0.00         ? 'true' : 'false'; # false

# Strings are true if they're not empty.
say 'a'           ? 'true' : 'false'; # true
say 'false'       ? 'true' : 'false'; # true
say ''            ? 'true' : 'false'; # false

# Even if a string would be treated as 0 in numeric context, it's true if nonempty.
# The only exception is the string "0", which is false.
# To force numeric context add 0 to the string
say '0'           ? 'true' : 'false'; # false
say '0.0'         ? 'true' : 'false'; # true
say '0e0'         ? 'true' : 'false'; # true
say '0 but true'  ? 'true' : 'false'; # true
say '0 whargarbl' ? 'true' : 'false'; # true
say 0+'0 argarbl' ? 'true' : 'false'; # false

# Things that become numbers in scalar context are treated as numbers.
my @c = ();
my @d = (0);
say @c            ? 'true' : 'false'; # false
say @d            ? 'true' : 'false'; # true

# Anything undefined is false.
say undef         ? 'true' : 'false'; # false

# References are always true, even if they point at something false
my @c = ();
my $d = 0;
say \@c            ? 'true' : 'false'; # true
say \$d            ? 'true' : 'false'; # true
say \0             ? 'true' : 'false'; # true
say \''            ? 'true' : 'false'; # true

```



#### Syntax


- undef # False
- ''    # Defined, False
- 0     # Defined, Has Length, False
- '0'   # Defined, Has Length, False



#### Remarks


Perl does not have a boolean data type, nor does it have any `true` and `false` keywords like many other languages. However, every scalar value will evaluate to true or false when evaluated in a boolean context (the condition in an if statement or a while loop, for example).

### The following values are considered false:

- `''`, the empty string. This is what the built-in comparison operators return (e.g. `0 == 1`)
- `0`, the number 0, even if you write it as 000 or 0.0
- `'0'`, the string that contains a single 0 digit
- `undef`, the undefined value
- Objects that use [overloading](http://perldoc.perl.org/overload.html) to numify/stringify into false values, such as [`JSON::false`](http://search.cpan.org/%7Emakamaka/JSON-2.90/lib/JSON.pm#JSON::false)

### All other values are true:

- any non-zero number such as `1`, `3.14`, [`'NaN'` or `'Inf'`](http://www.learning-perl.com/2015/05/perls-special-not-a-numbers/)
<li>any string that is numerically 0 but not literally the string `'0'`, such as `'00'`, `'0e0'`, `"0\n"` and `"abc"`.<br />
If you are **intentionally** returning a true numerically 0 value, prefer `'0E0'` (used by well known modules) or `'0 but true'` (used by Perl functions)</li>
- any other string that is not empty, such as `' '`, `'false'`
- all references, even if they reference false values, such as `\''`, `[]`, or `{}`
- an array or hash of false values

### The following operators are commonly treated to return a boolean in scalar context:

<li>
`@a` returns whether the array is empty or not
</li>
<li>
`%h` returns whether the hash is empty or not
</li>
<li>
`grep` returns whether any matching items were found or not
</li>
<li>
`@a = LIST` and `(LIST) = LIST` return whether the right-hand side LIST produced any scalars or not
</li>

