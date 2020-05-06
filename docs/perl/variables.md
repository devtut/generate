---
metaTitle: "Perl - Variables"
description: "Scalars, Array References, Arrays, Scalar References, Typeglobs, typeglob refs, filehandles and constants, Hashes, Hash References, Sigils"
---

# Variables




## Scalars


Scalars are Perl's most basic data type. They're marked with the sigil `$` and hold a single value of one of three types:

- **a number** (`3`, `42`, `3.141`, etc.)
- **a string** (`'hi'`, `"abc"`, etc.)
- **a reference** to a variable (see other examples).

```perl
my $integer = 3;                      # number
my $string = "Hello World";           # string
my $reference = \$string;             # reference to $string

```

**Perl converts between numbers and strings on the fly**, based on what a particular operator expects.

```perl
my $number = '41';                    # string '41'
my $meaning = $number + 1;            # number  42
my $sadness = '20 apples';            # string '20 apples'
my $danger = $sadness * 2;            # number '40', raises warning

```

When converting a string into a number, Perl takes as many digits from the front of a string as it can – hence why `20 apples` is converted into `20` in the last line.

Based on whether you want to treat the contents of a scalar as a string or a number, you need to use different operators. Do not mix them.

```perl
# String comparison                   # Number comparison
'Potato' eq 'Potato';                 42 == 42;
'Potato' ne 'Pomato';                 42 != 24;
'Camel'  lt 'Potato';                 41 < 42;
'Zombie' gt 'Potato';                 43 > 42;

# String concatenation                # Number summation
'Banana' . 'phone';                   23 + 19;

# String repetition                   # Number multiplication 
'nan' x 3;                            6 * 7;

```

Attempting to use string operations on numbers will not raise warnings; attempting to use number operations on non-numeric strings will. Do be aware that some non-digit strings such as `'inf'`, `'nan'`, `'0 but true'` count as numbers.



## Array References


Array References are scalars (`$`) which refer to Arrays.

```perl
my @array = ("Hello"); # Creating array, assigning value from a list
my $array_reference = \@array;

```

These can be created more short-hand as follows:

```perl
my $other_array_reference = ["Hello"];

```

Modifying / Using array references require dereferencing them first.

```perl
my @contents = @{ $array_reference };               # Prefix notation
my @contents = @$array_reference;                   # Braces can be left out

```

New postfix dereference syntax, available by default from v5.24

```perl
use v5.24;
my @contents = $array_reference->@*; # New postfix notation 

```

When accessing an arrayref's contents by index you can use the `->` syntactical sugar.

```perl
my @array = qw(one two three);      my $arrayref = [ qw(one two three) ]
my $one = $array[0];                my $one = $arrayref->[0];

```

Unlike arrays, arrayrefs can be nested:

```perl
my @array = ( (1, 0), (0, 1) )  # ONE array of FOUR elements: (1, 0, 0, 1)
my @matrix = ( [1, 0], [0, 1] ) # an array of two arrayrefs
my $matrix = [ [0, 1], [1, 0] ] # an arrayref of arrayrefs
# There is no namespace conflict between scalars, arrays and hashes
# so @matrix and $matrix _both_ exist at this point and hold different values.

my @diagonal_1 = ($matrix[0]->[1], $matrix[1]->[0])     # uses @matrix
my @diagonal_2 = ($matrix->[0]->[1], $matrix->[1]->[0]) # uses $matrix
# Since chained []- and {}-access can only happen on references, you can
# omit some of those arrows.
my $corner_1 = $matrix[0][1];   # uses @matrix;
my $corner_2 = $matrix->[0][1]; # uses $matrix;  

```

When used as Boolean, references are always true.



## Arrays


Arrays store an ordered sequence of values. You can access the contents by index, or iterate over them. The values will stay in the order you filled them in.

```perl
my @numbers_to_ten = (1,2,3,4,5,6,7,8,9,10); # More conveniently: (1..10)
my @chars_of_hello = ('h','e','l','l','o');
my @word_list = ('Hello','World');

# Note the sigil: access an @array item with $array[index]
my $second_char_of_hello = $chars_of_hello[1]; # 'e'

# Use negative indices to count from the end (with -1 being last)
my $last_char_of_hello = $chars_of_hello[-1];

# Assign an array to a scalar to get the length of the array
my $length_of_array = @chars_of_hello; # 5

# You can use $# to get the last index of an array, and confuse Stack Overflow
my $last_index_of_array = $#chars_of_hello; # 4

# You can also access multiple elements of an array at the same time
# This is called "array slice"
# Since this returns multiple values, the sigil to use here on the RHS is @
my @some_chars_of_hello = @chars_of_hello[1..3]; # ('H', 'e', 'l')
my @out_of_order_chars = @chars_of_hello[1,4,2]; # ('e', 'o', 'l')

# In Python you can say array[1:-1] to get all elements but first and last
# Not so in Perl: (1..-1) is an empty list. Use $# instead
my @empty_list = @chars_of_hello[1..-1];                           # ()
my @inner_chars_of_hello = @chars_of_hello[1..$#chars_of_hello-1]; # ('e','l','l')

# Access beyond the end of the array yields undef, not an error
my $undef = $chars_of_hello[6]; # undef    

```

Arrays are mutable:

```perl
use utf8; # necessary because this snippet is utf-8
$chars_of_hello[1] = 'u';              #     ('h','u','l','l','o')
push @chars_of_hello, ('!', '!');      #     ('h','u','l','l','o','!','!')
pop @chars_of_hello;                   #     ('h','u','l','l','o','!')
shift @chars_of_hello;                 #         ('u','l','l','o','!')
unshift @chars_of_hello, ('¡', 'H');   # ('¡','H','u','l','l','o','!')
@chars_of_hello[2..5] = ('O','L','A'); # ('¡','H','O','L','A',undef,'!') whoops! 
delete $chars_of_hello[-2];            # ('¡','H','O','L','A',      '!')

# Setting elements beyond the end of an array does not result in an error
# The array is extended with undef's as necessary. This is "autovivification."
my @array;           # ()
my @array[3] = 'x';  # (undef, undef, undef, 'x')

```

Finally, you can loop over the contents of an array:

```perl
use v5.10; # necessary for 'say'
for my $number (@numbers_to_ten) {
  say $number ** 2;
}

```

When used as booleans, arrays are true if they are not empty.



## Scalar References


A **reference** is a scalar variable (one prefixed by `$` ) which “refers to” some other data.

```perl
my $value     = "Hello";
my $reference = \$value;
print $value;     # => Hello
print $reference; # => SCALAR(0x2683310)

```

To get the referred-to data, you **de-reference** it.

```perl
say ${$reference};                  # Explicit prefix syntax
say $$reference;                    # The braces can be left out (confusing)

```

New postfix dereference syntax, available by default from v5.24

```perl
use v5.24;
say $reference->$*; # New postfix notation

```

This "de-referenced value" can then be changed like it was the original variable.

```perl
${$reference} =~ s/Hello/World/;
print ${$reference};  # => World
print $value;         # => World

```

A reference is always **truthy** – even if the value it refers to is falsy (like `0` or `""`).

### You may want a Scalar Reference If:

<li>
You want to pass a string to a function, and have it modify that string for you without it being a return value.
</li>
<li>
You wish to explicitly avoid Perl implicitly copying the contents of a large string at some point in your function passing ( especially relevant on older Perls without copy-on-write strings )
</li>
<li>
You wish to disambiguate string-like values with specific meaning, from strings that convey content, for example:
<ul>
- Disambiguate a file name from file content
- Disambiguate returned content from a returned error string

You wish to implement a lightweight inside out object model, where objects handed to calling code don't carry user visible metadata:

```perl
our %objects;
my $next_id = 0;
sub new { 
   my $object_id = $next_id++;
   $objects{ $object_id } = { ... }; # Assign data for object
   my $ref = \$object_id;
   return bless( $ref, "MyClass" );
}

```



## Typeglobs, typeglob refs, filehandles and constants


A typeglob `*foo` holds references to the contents of **global** variables with that name: `$foo`, `@foo`, `$foo`, `&foo`, etc. You can access it like an hash and assign to manipulate the symbol tables directly (evil!).

```perl
use v5.10; # necessary for say
our $foo = "foo";
our $bar;
say ref *foo{SCALAR};     # SCALAR
say ${ *foo{SCALAR} };    # bar
*bar = *foo;
say $bar;                 # bar
$bar = 'egg';
say $foo;                 # egg

```

Typeglobs are more commonly handled when dealing with files. `open`, for example, produces a reference to a typeglob when asked to create a non-global filehandle:

```perl
use v5.10; # necessary for say
open(my $log, '> utf-8', '/tmp/log') or die $!; # open for writing with encoding
say $log 'Log opened';

# You can dereference this globref, but it's not very useful.
say ref $log;                   # GLOB
say (*{$log}->{IO} // 'undef'); # undef

close $log or die $!;

```

Typeglobs can also be used to make global read-only variables, though [`use constant`](http://perldoc.perl.org/constant.html) is in broader use.

```perl
# Global constant creation
*TRUE = \('1');
our $TRUE;
say $TRUE;  # 1
$TRUE = ''; # dies, "Modification of a read-only value attempted"

# use constant instead defines a parameterless function, therefore it's not global,
# can be used without sigils, can be imported, but does not interpolate easily.
use constant (FALSE => 0);
say FALSE;        # 0
say &FALSE;       # 0
say "${\FALSE}";  # 0 (ugh)
say *FALSE{CODE}; # CODE(0xMA1DBABE)

# Of course, neither is truly constant when you can manipulate the symbol table...
*TRUE = \('');
use constant (EVIL => 1);
*FALSE = *EVIL;

```



## Hashes


Hashes can be understood as lookup-tables. You can access its contents by specifiying a key for each of them. Keys must be strings. If they're not, they will be converted to strings.

If you give the hash simply a known key, it will serve you its value.

```perl
# Elements are in (key, value, key, value) sequence
my %inhabitants_of = ("London", 8674000, "Paris", 2244000);

# You can save some typing and gain in clarity by using the "fat comma"
# syntactical sugar. It behaves like a comma and quotes what's on the left.
my %translations_of_hello = (spanish => 'Hola', german => 'Hallo', swedish => 'Hej'); 

```

In the following example, note the brackets and sigil: you access an element of `%hash` using `$hash{key}` because the **value** you want is a scalar. Some consider it good practice to quote the key while others find this style visually noisy. Quoting is only required for keys that could be mistaken for expressions like `$hash{'some-key'}`

```perl
my $greeting = $translations_of_hello{'spanish'};

```

While Perl by default will try to use barewords as strings, `+` modifier can also be used to indicate to Perl that key should not be interpolated but executed with result of execution being used as a key:

```perl
my %employee = ( name => 'John Doe', shift => 'night' );
# this example will print 'night'
print $employee{shift}; 

# but this one will execute [shift][1], extracting first element from @_,
# and use result as a key
print $employee{+shift};

```

Like with arrays, you can access multiple hash elements at the same time. This is called a **hash slice**. The resulting value is a list, so use the `@` sigil:

```perl
my @words = @translations_of_hello{'spanish', 'german'}; # ('Hola', 'Hallo')

```

Iterate over the keys of an hash with `keys` `keys` will return items in a random order. Combine with `sort` if you wish.

```perl
for my $lang (sort keys %translations_of_hello) {
  say $translations_of_hello{$lang};
}

```

If you do not actually need the keys like in the previous example, `values` returns the hash's values directly:

```perl
for my $translation (values %translations_of_hello) {
  say $translation;
}

```

You can also use a while loop with `each` to iterate over the hash. This way, you will get both the key and the value at the same time, without a separate value lookup. Its use is however discouraged, as [`each` can break in mistifying ways.](http://blogs.perl.org/users/rurban/2014/04/do-not-use-each.html)

```perl
# DISCOURAGED
while (my ($lang, $translation) = each %translations_of_hello) {
  say $translation;
}

```

Access to unset elements returns undef, not an error:

```perl
my $italian = $translations_of_hello{'italian'}; # undef

```

`map` and list flattening can be used to create hashes out of arrays. This is a popular way to create a 'set' of values, e.g. to quickly check whether a value is in `@elems`. This operation usually takes O(n) time (i.e. proportional to the number of elements) but can be done in constant time (O(1)) by turning the list into a hash:

```perl
@elems = qw(x y x z t);
my %set = map { $_ => 1 } @elems;   # (x, 1, y, 1, t, 1)
my $y_membership = $set{'y'};       # 1
my $w_membership = $set{'w'};       # undef

```

This requires some explanation. The contents of `@elems` get read into a list, which is processed by `map`. `map` accepts a code block that gets called for each value of its input list; the value of the element is available for use in `$_`. Our code block returns **two** list elements for each input element: `$_`, the input element, and `1`, just some value. Once you account for list flattening, the outcome is that `map { $_ => 1 } @elems` turns `qw(x y x z t)` into `(x => 1, y => 1, x => 1, z => 1, t => 1)`.

As those elements get assigned into the hash, odd elements become hash keys and even elements become hash values. When a key is specified multiple times in a list to be assigned to a hash, the **last** value wins. This effectively discards duplicates.

A faster way to turn a list into a hash uses assignment to a hash slice. It uses the `x` operator to multiply the single-element list `(1)` by the size of `@elems`, so there is a `1` value for each of the keys in the slice on the left hand side:

```perl
@elems = qw(x y x z t);
my %set;
@set{@elems} = (1) x @elems;

```

The following application of hashes also exploits the fact that hashes and lists can often be used interchangeably to implement named function args:

```perl
sub hash_args {
  my %args = @_;
  my %defaults = (foo => 1, bar => 0);
  my %overrides = (__unsafe => 0);
  my %settings = (%defaults, %args, %overrides);
}

# This function can then be called like this:
hash_args(foo => 5, bar => 3); # (foo => 5, bar => 3, __unsafe ==> 0)
hash_args();                   # (foo => 1, bar => 0, __unsafe ==> 0)
hash_args(__unsafe => 1)       # (foo => 1, bar => 0, __unsafe ==> 0)

```

When used as booleans, hashes are true if they are not empty.



## Hash References


Hash references are scalars which contain a pointer to the memory location containing the data of a hash.  Because the scalar points directly to the hash itself, when it is passed to a subroutine, changes made to the hash are not local to the subroutine as with a regular hash, but instead are global.

First, let's examine what happens when you pass a normal hash to a subroutine and modify it within there:

```perl
use strict;
use warnings;
use Data::Dumper;

sub modify
{
    my %hash = @_;

    $hash{new_value} = 2;

    print Dumper("Within the subroutine");
    print Dumper(\%hash);

    return;
}

my %example_hash = (
    old_value   => 1,
);

modify(%example_hash);

print Dumper("After exiting the subroutine");
print Dumper(\%example_hash);

```

Which results in:

```perl
$VAR1 = 'Within the subroutine';
$VAR1 = {
          'new_value' => 2,
          'old_value' => 1
        };
$VAR1 = 'After exiting the subroutine';
$VAR1 = {
          'old_value' => 1
        };

```

Notice that after we exit the subroutine, the hash remains unaltered; all changes to it were local to the modify subroutine, because we passed a copy of the hash, not the hash itself.

In comparison, when you pass a hashref, you are passing the address to the original hash, so any changes made within the subroutine will be made to the original hash:

```perl
use strict;
use warnings;
use Data::Dumper;

sub modify
{
    my $hashref = shift;

    # De-reference the hash to add a new value
    $hashref->{new_value} = 2;

    print Dumper("Within the subroutine");
    print Dumper($hashref);

    return;
}

# Create a hashref
my $example_ref = {
    old_value   => 1,
};

# Pass a hashref to a subroutine
modify($example_ref);

print Dumper("After exiting the subroutine");
print Dumper($example_ref);

```

This will result in:

```perl
$VAR1 = 'Within the subroutine';
$VAR1 = {
          'new_value' => 2,
          'old_value' => 1
        };
$VAR1 = 'After exiting the subroutine';
$VAR1 = {
          'new_value' => 2,
          'old_value' => 1
        };

```



## Sigils


Perl has a number of sigils:

```perl
$scalar = 1; # individual value
@array = ( 1, 2, 3, 4, 5 ); # sequence of values
%hash = ('it', 'ciao', 'en', 'hello', 'fr', 'salut'); # unordered key-value pairs
&function('arguments'); # subroutine
*typeglob; # symbol table entry

```

These look like sigils, but aren't:

```perl
\@array; # \ returns the reference of what's on the right (so, a reference to @array)
$#array; # this is the index of the last element of @array

```

You can use braces after the sigil if you should be so inclined. Occasionally, this improves readability.

```perl
say ${value} = 5;

```

While you use different sigils to define variables of different types, the same variable can be accessed in different ways based on what sigils you use.

```perl
%hash;            # we use % because we are looking at an entire hash
$hash{it};        # we want a single value, however, that's singular, so we use $
$array[0];        # likewise for an array. notice the change in brackets.
@array[0,3];      # we want multiple values of an array, so we instead use @
@hash{'it','en'}; # similarly for hashes (this gives the values: 'ciao', 'hello')
%hash{'it','fr'}; # we want an hash with just some of the keys, so we use %
                  # (this gives key-value pairs: 'it', 'ciao', 'fr', 'salut')

```

This is especially true of references. In order to use a referenced value you can combine sigils together.

```perl
my @array = 1..5;                    # This is an array
my $reference_to_an_array = \@array; # A reference to an array is a singular value
push @array, 6;                      # push expects an array
push @$reference_to_an_array, 7;     # the @ sigil means what's on the right is an array
                                     # and what's on the right is $reference_to_an_array
                                     # hence: first a @, then a $

```

Here's a perhaps less confusing way to think about it. As we saw earlier, you can use braces to wrap what's on the right of a sigil. So you can think of `@{}` as something that takes an array reference and gives you the referenced array.

```perl
# pop does not like array references
pop $reference_to_an_array; # ERROR in Perl 5.20+
# but if we use @{}, then...
pop @{ $reference_to_an_array }; # this works!

```

As it turns out, `@{}` actually accepts an expression:

```perl
my $values = undef;
say pop @{ $values };       # ERROR: can't use undef as an array reference
say pop @{ $values // [5] } # undef // [5] gives [5], so this prints 5

```

...and the same trick works for other sigils, too.

```perl
# This is not an example of good Perl. It is merely a demonstration of this language feature
my $hashref = undef;
for my $key ( %{ $hashref // {} } ) {
  "This doesn't crash";
}

```

...but if the "argument" to a sigil is simple, you can leave the braces away.

```perl
say $$scalar_reference;
say pop @$array_reference;
for keys (%$hash_reference) { ... };

```

Things can get excessively extravagant. This works, but please Perl responsibly.

```perl
my %hash = (it => 'ciao', en => 'hi', fr => 'salut');
my $reference = \%hash;
my $reference_to_a_reference = \$reference;

my $italian = $hash{it};                              # Direct access 
my @greets = @$reference{'it', 'en'};                 # Dereference, then access as array
my %subhash = %$$reference_to_a_reference{'en', 'fr'} # Dereference ×2 then access as hash

```

For most normal use, you can just use subroutine names without a sigil. (Variables without a sigil are typically called "barewords".) The `&` sigil is only useful in a limited number of cases.

<li>
Making a reference to a subroutine:

```perl
sub many_bars { 'bar' x $_[0] }
my $reference = \&many_bars;
say $reference->(3); # barbarbar

```


</li>
<li>
Calling a function ignoring its prototype.
</li>
<li>
Combined with goto, as a slightly weird function call that has the current call frame replaced with the caller. Think the linux `exec()` API call, but for functions.
</li>



#### Syntax


- my  # Lexical declaration
- our # Global declaration
- $foo # Scalar
- @foo # Array
- $#foo # Array Last-Index
- %foo # Hash
- ${$foo} # Scalar De-Reference
- @{$foo} # Array  De-Reference
- $#{$foo} # Array-DeRef Last-Index
- %{$foo} # Hash De-Reference
- $foo[$index]    # Array get indexed
- ${$foo}[$index] # Array De-Reference and get indexed.
- $foo->[$index]  # Array De-Reference and get indexed ( Simplified )
- $foo{$key}  # Hash get value for key
- ${$foo}{$key} # Hash Dereference and get value for key
- $foo->{$key}  # Hash Dereference and get value for key ( Simplified )
- \$x # Reference to Scalar
- \@x # Reference to Array
- \%x # Reference to Hash
- =[ ] # Reference to Anonymous Array (Inline)
- ={ } # Reference to Anonymous Hash (Inline)

