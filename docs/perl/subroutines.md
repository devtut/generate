---
metaTitle: "Perl - Subroutines"
description: "Creating subroutines, Subroutines, Subroutine arguments are passed by reference (except those in signatures)"
---

# Subroutines



## Creating subroutines


Subroutines are created by using the keyword `sub` followed by an identifier and a code block enclosed in braces.

You can access the arguments by using the special variable `@_`, which contains all arguments as an array.

```perl
sub function_name {
    my ($arg1, $arg2, @more_args) = @_;
    # ...
}

```

Since the function `shift` defaults to shifting `@_` when used inside a subroutine, it's a common pattern to extract the arguments sequentially into local variables at the beginning of a subroutine:

```perl
sub function_name {
    my $arg1 = shift;
    my $arg2 = shift;
    my @more_args = @_;
    # ...
}

# emulate named parameters (instead of positional)
sub function_name {
    my %args = (arg1 => 'default', @_);
    my $arg1 = delete $args{arg1};
    my $arg2 = delete $args{arg2};
    # ...
}

sub {
    my $arg1 = shift;
    # ...
}->($arg);

```

Alternatively, the experimental feature `"signatures"` can be used to unpack parameters, which are passed by value (**not** by reference).

```perl
use feature "signatures";

sub function_name($arg1, $arg2, @more_args) {
    # ...
}

```

Default values can be used for the parameters.

```perl
use feature "signatures";

sub function_name($arg1=1, $arg2=2) {
    # ...
}

```

You can use any expression to give a default value to a parameter – including other parameters.

```perl
sub function_name($arg1=1, $arg2=$arg1+1) {
    # ...
}

```

Note that you can't reference parameters which are defined after the current parameter – hence the following code doesn't work quite as expected.

```perl
sub function_name($arg1=$arg2, $arg2=1) {
    print $arg1;  # => <nothing>
    print $arg2;  # => 1
}

```



## Subroutines


Subroutines hold code. Unless specified otherwise, they are globally defined.

```perl
# Functions do not (have to) specify their argument list
sub returns_one {
  # Functions return the value of the last expression by default
  # The return keyword here is unnecessary, but helps readability.
  return 1;
}

# Its arguments are available in @_, however
sub sum {
  my $ret = 0;
  for my $value (@_) {
    $ret += $value
  }
  return $ret;
}

# Perl makes an effort to make parens around argument list optional
say sum 1..3;     # 6

# If you treat functions as variables, the & sigil is mandatory.
say defined &sum; # 1

```

[Some builtins](http://learn.perl.org/docs/keywords.html) such as `print` or `say` are keywords, not functions, so e.g. `&say` is undefined. It also does mean that you can define them, but you will have to specify the package name to actually call them

```perl
# This defines the function under the default package, 'main'
sub say {
  # This is instead the say keyword
  say "I say, @_";
}

# ...so you can call it like this: 
main::say('wow'); # I say, wow.

```

Since Perl 5.18, you can also have non-global functions:

```perl
use feature 'lexical_subs';
my $value;
{
  # Nasty code ahead
  my sub prod {
    my $ret = 1;
    $ret *= $_ for @_;
    $ret;
  }
  $value = prod 1..6; # 720 
  say defined &prod; # 1
}
say defined &prod; # 0

```

Since 5.20, you can also have named parameters.

```perl
use feature 'signatures';
sub greet($name) {
  say "Hello, $name";
}

```

This should **not** be confused with prototypes, a facility Perl has to let you define functions that behave like built-ins. Function prototypes must be visible at compile time and its effects can be ignored by specifying the `&` sigil. Prototypes are generally considered to be an advanced feature that is best used with great care.

```perl
# This prototype makes it a compilation error to call this function with anything 
# that isn't an array. Additionally, arrays are automatically turned into arrayrefs
sub receives_arrayrefs(\@\@) {
   my $x = shift;
   my $y = shift;
}

my @a = (1..3);
my @b = (1..4);
receives_arrayrefs(@a, @b);    # okay,   $x = \@a, $y = \@b, @_ = ();
receives_arrayrefs(\@a, \@b);  # compilation error, "Type … must be array …"
BEGIN { receives_arrayrefs(\@a, \@b); }

# Specify the sigil to ignore the prototypes. 
&receives_arrayrefs(\@a, \@b); # okay,   $x = \@a, $y = \@b, @_ = ();
&receives_arrayrefs(@a, @b);   # ok, but $x = 1,   $y = 2,   @_ = (3,1,2,3,4);

```



## Subroutine arguments are passed by reference (except those in signatures)


Subroutine arguments in Perl are passed by reference, unless they are in the signature. This means that the members of the `@_` array inside the sub are just **aliases** to the actual arguments. In the following example, `$text` in the main program is left modified after the subroutine call because `$_[0]` inside the sub is actually just a different name for the same variable. The second invocation throws an error because a string literal is not a variable and therefore can't be modified.

```perl
use feature 'say';

sub edit {
    $_[0] =~ s/world/sub/;
}

my $text = "Hello, world!";
edit($text);
say $text;      # Hello, sub!

edit("Hello, world!"); # Error: Modification of a read-only value attempted

```

To avoid clobbering your caller's variables it is therefore important to copy `@_` to locally scoped variables (`my ...`) as described under "Creating subroutines".



#### Remarks


Subroutines get their arguments to magic variable called  `@_`. While it doesn't have to be unpacked, it's recommended, as it helps readability, and prevents accidental changes as arguments of `@_` are passed by reference (can be modified).

