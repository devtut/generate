---
metaTitle: "Perl - Interpolation in Perl"
description: "What is interpolated, Basic interpolation"
---

# Interpolation in Perl



## What is interpolated


Perl interpolates variable names:

```perl
my $name = 'Paul';
print "Hello, $name!\n"; # Hello, Paul!

my @char = ('a', 'b', 'c');
print "$char[1]\n"; # b

my %map = (a => 125, b => 1080, c => 11);
print "$map{a}\n"; # 125

```

Arrays may be interpolated as a whole, their elements are separated by spaces:

```perl
my @char = ('a', 'b', 'c');
print "My chars are @char\n"; # My chars are a b c

```

Perl does **not** interpolate hashes as a whole:

```perl
my %map = (a => 125, b => 1080, c => 11);
print "My map is %map\n"; # My map is %map

```

and function calls (including constants):

```perl
use constant {
    PI => '3.1415926'
};
print "I like PI\n";         # I like PI
print "I like " . PI . "\n"; # I like 3.1415926

```

Perl interpolates **escape sequences** starting with `\`:

```perl
\t                  horizontal tab
\n                  newline
\r                  return
\f                  form feed
\b                  backspace
\a                  alarm (bell)
\e                  escape

```

Interpolation of `\n` depends on the system where program is working: it will produce a newline character(s) according to the current system conventions.

Perl does **not** interpolate `\v`, which means vertical tab in C and other languages.

Character may be addressed using their codes:

```perl
\x{1d11e}     𝄞 by hexadecimal code
\o{350436}    𝄞 by octal code
\N{U+1d11e}   𝄞 by Unicode code point

```

or Unicode names:

```perl
\N{MUSICAL SYMBOL G CLEF}

```

Character with codes from `0x00` to `0xFF` in the **native** encoding may be addressed in a shorter form:

```perl
\x0a     hexadecimal
\012     octal

```

Control character may be addressed using special escape sequences:

```perl
\c@      chr(0)
\ca      chr(1)
\cb      chr(2)
...
\cz      chr(26)
\c[      chr(27)
\c\      chr(28) # Cannot be used at the end of a string
                 # since backslash will interpolate the terminating quote
\c]      chr(29)
\c^      chr(30)
\c_      chr(31)
\c?      chr(127)

```

Uppercase letters have the same meaning: `"\cA" == "\ca"`.

Interpretation of all escape sequences except for `\N{...}` may depend on the platform since they use platform- and encoding-dependent codes.



## Basic interpolation


Interpolation means that Perl interpreter will substitute the values of variables for their name and some symbols (which are impossible or difficult to type in directly) for special sequences of characters (it is also known as escaping). The most important distinction is between single and double quotes: double quotes interpolate the enclosed string, but single quotes do not.

```perl
my $name = 'Paul';
my $age = 64;
print "My name is $name.\nI am $age.\n"; # My name is Paul.
                                         # I am 64.

```

But:

```perl
print 'My name is $name.\nI am $age.\n'; # My name is $name.\nI am $age.\n

```

You can use `q{}` (with any delimiter) instead of single quotes and `qq{}` instead of double quotes. For example, `q{I'm 64}` allows to use an apostrophe within a non-interpolated string (otherwise it would terminate the string).

Statements:

```perl
print qq{$name said: "I'm $age".}; # Paul said: "I'm 64".
print "$name said: \"I'm $age\"."  # Paul said: "I'm 64".

```

do the same thing, but in the first one you do not need to escape double quotes within the string.

If your variable name clashes with surrounding text, you can use the syntax `${var}` to disambiguate:

```perl
my $decade = 80;
print "I like ${decade}s music!"  # I like 80s music!

```

