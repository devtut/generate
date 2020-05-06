---
metaTitle: "Perl - Special variables"
description: "Special variables in perl:"
---

# Special variables




## Special variables in perl:


**1.** `$_` : The default input and pattern-searching space.

**Example 1:**

```perl
my @array_variable = (1 2 3 4);
foreach (@array_variable){
    print $_."\n";    # $_ will get the value 1,2,3,4 in loop, if no other variable is supplied.
}

```

**Example 2:**

```perl
while (<FH>){
    chomp($_);    # $_ refers to the iterating lines in the loop.
}

```

The following functions use `$_` as a default argument:

```perl
abs, alarm, chomp, chop, chr, chroot, cos, defined, eval,
evalbytes, exp, fc, glob, hex, int, lc, lcfirst, length, log,
lstat, mkdir, oct, ord, pos, print, printf, quotemeta, readlink,
readpipe, ref, require, reverse (in scalar context only), rmdir,
say, sin, split (for its second argument), sqrt, stat, study,
uc, ucfirst, unlink, unpack.

```

**2.** `@_` : This array contains the arguments passed to subroutine.

**Example 1:**

```perl
example_sub( $test1, $test2, $test3 );

sub example_sub {
    my ( $test1, $test2, $test3 ) = @_; 
}

```

Within a subroutine the array `@_` contains the **arguments** passed to that subroutine. Inside a subroutine, `@_` is the `default` array for the array operators `pop` and `shift`.



#### Remarks


TO DO : Add more contents.

