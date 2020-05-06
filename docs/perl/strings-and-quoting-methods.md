---
metaTitle: "Perl - Strings and quoting methods"
description: "String Literal Quoting, Double-quoting, Heredocs, Removing trailing newlines"
---

# Strings and quoting methods



## String Literal Quoting


String literals imply no escaping or interpolation ( with the exception of quoting string terminators )

```perl
print 'This is a string literal\n'; # emits a literal \ and n to terminal

print 'This literal contains a \'postraphe '; # emits the ' but not its preceding \

```

You can use alternative quoting mechanisms to avoid clashes:

```perl
print q/This is is a literal \' <-- 2 characters /;  # prints both \ and '
print q^This is is a literal \' <-- 2 characters ^;  # also

```

Certain chosen quote characters are "balanced"

```perl
print q{ This is a literal and I contain { parens! } }; # prints inner { }

```



## Double-quoting


Double-quoted strings use **interpolation** and **escaping** – unlike single-quoted strings. To double-quote a string, use either double quotes `"` or the `qq` operator.

```perl
my $greeting = "Hello!\n";
print $greeting;
# => Hello! (followed by a linefeed)

my $bush = "They misunderestimated me."
print qq/As Bush once said: "$bush"\n/;
# => As Bush once said: "They misunderestimated me." (with linefeed)

```

The `qq` is useful here, to avoid having to escape the quotation marks. Without it, we would have to write...

```perl
print "As Bush once said: \"$bush\"\n";

```

... which just isn't as nice.

Perl doesn't limit you to using a slash `/` with `qq`; you can use any (visible) character.

```perl
use feature 'say';

say qq/You can use slashes.../;
say qq{...or braces...};
say qq^...or hats...^;
say qq|...or pipes...|;
# say qq ...but not whitespace. ;

```

You can also interpolate arrays into strings.

```perl
use feature 'say';

my @letters = ('a', 'b', 'c');
say "I like these letters: @letters.";
# => I like these letters: a b c.

```

By default the values are space-separated – because the special variable `$"` defaults to a single space. This can, of course, be changed.

```perl
use feature 'say';

my @letters = ('a', 'b', 'c');
{local $" = ", "; say "@letters"; }    # a, b, c

```

If you prefer, you have the option to `use English` and change `$LIST_SEPARATOR` instead:

```perl
use v5.18; # English should be avoided on older Perls
use English;

my @letters = ('a', 'b', 'c');
{ local $LIST_SEPARATOR = "\n"; say "My favourite letters:\n\n@letters" }

```

For anything more complex than this, you should use a loop instead.

```perl
say "My favourite letters:";
say;
for my $letter (@letters) {
  say " - $letter";
}

```

Interpolation does **not** work with hashes.

```perl
use feature 'say';

my %hash = ('a', 'b', 'c', 'd');
say "This doesn't work: %hash"         # This doesn't work: %hash

```

Some code abuses interpolation of references – **avoid it**.

```perl
use feature 'say';

say "2 + 2 == @{[ 2 + 2 ]}";           # 2 + 2 = 4 (avoid this)
say "2 + 2 == ${\( 2 + 2 )}";          # 2 + 2 = 4 (avoid this)

```

The so-called "cart operator" causes perl to dereference `@{ ... }` the array reference `[ ... ]` that contains the expression that you want to interpolate, `2 + 2`. When you use this trick, Perl builds an anonymous array, then dereferences it and discards it.

The `${\( ... )}` version is somewhat less wasteful, but it still requires allocating memory and it is even harder to read.

Instead, consider writing:

- `say "2 + 2 == " . 2 + 2;`
- `my $result = 2 + 2; say "2 + 2 == $result"`



## Heredocs


Large Multi-Line strings are burdensome to write.

```perl
my $variable = <<'EOF';
this block of text is interpreted literally,
no \'quotes matter, they're just text
only the trailing left-aligned EOF matters.
EOF

```

NB: Make sure you ignore stack-overflows syntax highlighter: It is very wrong.

And Interpolated Heredocs work the same way.

```perl
my $variable = <<"I Want it to End";
this block of text is interpreted.
quotes\nare interpreted, and $interpolations
get interpolated... 
but still, left-aligned "I Want it to End" matters.
I Want it to End

```

Pending in 5.26.0* is an "Indented Heredoc" Syntax which trims left-padding off for you

```perl
my $variable = <<~"MuchNicer";
    this block of text is interpreted.
    quotes\nare interpreted, and $interpolations
    get interpolated... 
    but still, left-aligned "I Want it to End" matters.
MuchNicer

```



## Removing trailing newlines


The function `chomp` will remove **one** newline character, if present, from each scalar passed to it. `chomp` will mutate the original string and will return the number of characters removed

```perl
my $str = "Hello World\n\n";
my $removed = chomp($str);
print $str;     # "Hello World\n"
print $removed; # 1    

# chomp again, removing another newline
$removed = chomp $str;
print $str;     # "Hello World"
print $removed; # 1    

# chomp again, but no newline to remove
$removed = chomp $str;
print $str;     # "Hello World"
print $removed; # 0    

```

You can also `chomp` more than one string at once:

```perl
my @strs = ("Hello\n", "World!\n\n"); # one newline in first string, two in second

my $removed = chomp(@strs); # @strs is now  ("Hello", "World!\n")
print $removed;             # 2

$removed = chomp(@strs); # @strs is now ("Hello", "World!")
print $removed;          # 1  

$removed = chomp(@strs); # @strs is still ("Hello", "World!")
print $removed;          # 0

```

But usually, no one worries about how many newlines were removed, so `chomp` is usually seen in void context, and usually due to having read lines from a file:

```perl
while (my $line = readline $fh)
{
    chomp $line;

    # now do something with $line
}

my @lines = readline $fh2;

chomp (@lines); # remove newline from end of each line

```



#### Remarks


The version syntax doesn't allow us to guard off versions that don't exist yet, so this is a reminder for somebody to go back and edit them in once it lands(  RE: Perl 5.26 ). The version guards rather need to have a "future" classification for tentative features that might be available to people brave enough to do a source checkout.

