---
metaTitle: "Perl - Lists"
description: "Array as list, Assigning a list to a hash, Lists can be passed into subroutines, Return list from subroutine, Hash as list, Using arrayref to pass array to sub"
---

# Lists



## Array as list


The array is one of Perl's basic variable types. It contains a list, which is an ordered sequence of zero or more scalars. The array is the variable holding (and providing access to) the list data, as is documented in [perldata](http://perldoc.perl.org/perldata.html).

You can assign a list to an array:

```perl
my @foo = ( 4, 5, 6 );

```

You can use an array wherever a list is expected:

```perl
join '-', ( 4, 5, 6 );
join '-', @foo;

```

Some operators only work with arrays since they mutate the list an array contains:

```perl
shift @array;
unshift @array, ( 1, 2, 3 );
pop @array;
push @array, ( 7, 8, 9 );

```



## Assigning a list to a hash


Lists can also be assigned to hash variables. When creating a list that will be assigned to a hash variable, it is recommended to use the ****fat comma**** `=>` between keys and values to show their relationship:

```perl
my %hash = ( foo => 42, bar => 43, baz => 44 );

```

The `=>` is really only a special comma that automatically quotes the operand to its left. So, you **could** use normal commas, but the relationship is not as clear:

```perl
my %hash = ( 'foo', 42, 'bar', 43, 'baz', 44 );

```

You can also use quoted strings for the left hand operand of the fat comma `=>`, which is especially useful for keys containing spaces.

```perl
my %hash = ( 'foo bar' => 42, 'baz qux' => 43 );

```

For details see [Comma operator](http://perldoc.perl.org/perlop.html#Comma-Operator) at `perldoc perlop`.



## Lists can be passed into subroutines


As to pass list into a subroutine, you specify the subroutine's name and then supply the list to it:

```perl
test_subroutine( 'item1', 'item2' );
test_subroutine  'item1', 'item2';     # same

```

Internally Perl makes **aliases** to those arguments and put them into the array `@_` which is available within the subroutine:

```perl
@_ =  ( 'item1', 'item2' ); # Done internally by perl

```

You access subroutine arguments like this:

```perl
sub test_subroutine {
    print $_[0]; # item1
    print $_[1]; # item2
}

```

**Aliasing** gives you the ability to change the original value of argument passed to subroutine:

```perl
sub test_subroutine {
    $_[0] +=  2;
}

my $x =  7;
test_subroutine( $x );
print $x; # 9

```

To prevent inadvertent changes of original values passed into your subroutine, you should copy them:

```perl
sub test_subroutine {
    my( $copy_arg1, $copy_arg2 ) =  @_;
    $copy_arg1 += 2;
}

my $x =  7;
test_subroutine $x; # in this case $copy_arg2 will have `undef` value
print $x; # 7

```

To test how many arguments were passed into the subroutine, check the size of `@_`

```perl
sub test_subroutine {
    print scalar @_, ' argument(s) passed into subroutine';
}

```

If you pass array arguments into a subroutine they all will be **flattened**:

```perl
my @x =  ( 1, 2, 3 );
my @y =  qw/ a b c /; # ( 'a', 'b', 'c' )
test_some_subroutine @x, 'hi', @y; # 7 argument(s) passed into subroutine
# @_ =  ( 1, 2, 3, 'hi', 'a', 'b', 'c' ) # Done internally for this call

```

If your `test_some_subroutine` contains the statement `$_[4] = 'd'`, for the above call it will cause `$y[0]` to have value `d` afterwards:

```perl
print "@y"; # d b c

```



## Return list from subroutine


You can, of course, return lists from subs:

```perl
sub foo {
    my @list1 =  ( 1, 2, 3 );
    my @list2 =  ( 4, 5 );

    return    ( @list1, @list2 );
}

my @list =  foo();
print @list;          # 12345

```

**But it is not the recommended way to do that** unless you know what you are doing.

While this is OK when the result is in **LIST** context, in **SCALAR** context things are unclear. Let's take a look at the next line:

```perl
print scalar foo();  # 2

```

Why `2`? What is going on?

1. Because `foo()` evaluated in **SCALAR** context, this list `( @list1, @list2 )` also evaluated in **SCALAR** context
1. In **SCALAR** context, LIST returns its last element. Here it is `@list2`
1. Again in **SCALAR** context, **array** `@list2` returns the number of its elements. Here it is `2`.

In most cases the **right strategy will return references to data structures**.<br />
So in our case we should do the following instead:

```

return    ( \@list1, \@list2 );

```

Then the caller does something like this to receive the two returned **arrayrefs**:

```

my ($list1, $list2) = foo(...);

```



## Hash as list


In list context hash is flattened.

```perl
my @bar =  ( %hash, %hash );

```

The **array** `@bar` is initialized by list of two `%hash` hashes

- both `%hash` are flattened
- new list is created from flattened items
- `@bar` array is initialized by that list

It is guaranteed that key-value pairs goes together. Keys are always even indexed, values - odd. It is not guaranteed that key-value pairs are always flattened in same order:

```perl
my %hash =  ( a => 1, b => 2 );
print %hash; # Maybe 'a1b2' or 'b2a1'

```



## Using arrayref to pass array to sub


The arrayref for `@foo` is `\@foo`.  This is handy if you need to pass an array and other things to a subroutine.  Passing `@foo` is like passing multiple scalars.  But passing `\@foo` is a single scalar.  Inside the subroutine:

```perl
xyz(\@foo, 123);
...
sub xyz {
    my ($arr, $etc) = @_;
    print $arr->[0]; # using the first item in $arr. It is like $foo[0]

```

