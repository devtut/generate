---
metaTitle: "Perl - Sorting"
description: "Basic Lexical Sort, The Schwartzian Transform, Case Insensitive Sort, Numeric Sort, Reverse Sort"
---

# Sorting


For sorting lists of things, Perl has only a single function, unsurprisingly called `sort`. It is flexible enough to sort all kinds of items: numbers, strings in any number of encodings, nested data structures or objects. However, due to its flexibility, there are quite a few tricks and idioms to be learned for its use.



## Basic Lexical Sort


```perl
@sorted = sort @list;

@sorted = sort { $a cmp $b } @list;

sub compare { $a cmp $b }
@sorted = sort compare @list;

```

The three examples above do exactly the same thing. If you don't supply any comparator function or block, `sort` assumes you want the list on its right sorted lexically. This is usually the form you want if you just need your data in some predictable order and don't care about linguistic correctness.

`sort` passes pairs of items in `@list` to the comparator function, which tells `sort` which item is larger. The `cmp` operator does this for strings while `<=>` does the same thing for numbers. The comparator is called quite often, on average **n** * log(**n**) times with **n** being the number of elements to be sorted, so it's important it be fast. This is the reason `sort` uses predefined package global variables (`$a` and `$b`) to pass the elements to be compared to the block or function, instead of proper function parameters.

If you `use locale`, `cmp` takes locale specific collation order into account, e.g. it will sort `Å` like `A` under a Danish locale but after `Z` under an English or German one. However, it doesn't take the more complex Unicode sorting rules into account nor does it offer any control over the order—for example phone books are often sorted differently from dictionaries. For those cases, the `Unicode::Collate` and particularly `Unicode::Collate::Locale` modules are recommended.



## The Schwartzian Transform


This is probably the most famous example of a sort optimization making use of Perl's functional programming facilities, to be used where the sort order of items depend on an expensive function.

```perl
# What you would usually do
@sorted = sort { slow($a) <=> slow($b) } @list;

# What you do to make it faster
@sorted =
map { $_->[0] }
sort { $a->[1] <=> $b->[1] }
map { [ $_, slow($_) ] }
@list;

```

The trouble with the first example is that the comparator is called very often and keeps recalculating values using a slow function over and over. A typical example would be sorting file names by their file size:

```perl
use File::stat;
@sorted = sort { stat($a)->size <=> stat($b)->size } glob "*";

```

This works, but at best it incurs the overhead of two system calls per comparison, at worst it has to go to the disk, twice, for every single comparison, and that disk may be in an overloaded file server on the other side of the planet.

Enter Randall Schwartz's trick.

The Schwartzian Transform basically shoves `@list` through three functions, bottom-to-top. The first `map` turns each entry into a two-element list of the original item and the result of the slow function as a sort key, so at the end of this we have called `slow()` exactly once for each element. The following `sort` can then simply access the sort key by looking in the list. As we don't care about the sort keys but only need the original elements in sorted order, the final `map` throws away the two-element lists from the already-sorted list it receives from `@sort` and returns a list of only their first members.



## Case Insensitive Sort


The traditional technique to make `sort` ignore case is to pass strings to `lc` or `uc` for comparison:

```perl
@sorted = sort { lc($a) cmp lc($b) } @list;

```

This works on all versions of Perl 5 and is completely sufficient for English; it doesn't matter whether you use `uc` or `lc`. However, it presents a problem for languages like Greek or Turkish where there is no 1:1 correspondence between upper- and lowercase letters so you get different results depending on whether you use `uc` or `lc`. Therefore, Perl 5.16 and higher have a **case folding** function called `fc` that avoids this problem, so modern multi-lingual sorting should use this:

```perl
@sorted = sort { fc($a) cmp fc($b) } @list;

```



## Numeric Sort


```perl
@sorted = sort { $a <=> $b } @list;

```

Comparing `$a` and `$b` with the `<=>` operator ensures they are compared numerically and not textually as per default.



## Reverse Sort


```perl
@sorted = sort { $b <=> $a } @list;
@sorted = reverse sort { $a <=> $b } @list;

```

Sorting items in descending order can simply be achieved by swapping `$a` and `$b` in the comparator block. However, some people prefer the clarity of a separate `reverse` even though it is slightly slower.



#### Syntax


- sort SUBNAME LIST
- sort BLOCK LIST
- sort LIST

