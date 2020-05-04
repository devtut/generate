---
metaTitle: "Brace Expansion"
description: "Modifying filename extension, Create directories to group files by month and year, Create a backup of dotfiles, Use increments, Using brace expansion to create lists, Make Multiple Directories with Sub-Directories"
---

# Brace Expansion




## Modifying filename extension


```bash
$ mv filename.{jar,zip}

```

This expands into `mv filename.jar filename.zip` .



## Create directories to group files by month and year


```bash
$ mkdir 20{09..11}-{01..12}

```

Entering the `ls` command will show that the following directories were created:

```bash
2009-01 2009-04 2009-07 2009-10 2010-01 2010-04 2010-07 2010-10 2011-01 2011-04 2011-07 2011-10
2009-02 2009-05 2009-08 2009-11 2010-02 2010-05 2010-08 2010-11 2011-02 2011-05 2011-08 2011-11
2009-03 2009-06 2009-09 2009-12 2010-03 2010-06 2010-09 2010-12 2011-03 2011-06 2011-09 2011-12

```

Putting a `0` in front of `9` in the example ensures the numbers are padded with a single `0`. You can also pad numbers with multiple zeros, for example:

```bash
$ echo {001..10}
001 002 003 004 005 006 007 008 009 010

```



## Create a backup of dotfiles


```bash
$ cp .vimrc{,.bak}

```

This expands into the command `cp .vimrc .vimrc.bak`.



## Use increments


```bash
$ echo {0..10..2}
0 2 4 6 8 10

```

A third parameter to specify an increment, i.e. `{start..end..increment}`

Using increments is not constrained to just numbers

```bash
$ for c in {a..z..5}; do echo -n $c; done
afkpuz

```



## Using brace expansion to create lists


Bash can easily create lists from alphanumeric characters.

```bash
# list from a to z    
$ echo {a..z}
a b c d e f g h i j k l m n o p q r s t u v w x y z
    
# reverse from z to a
$ echo {z..a}
z y x w v u t s r q p o n m l k j i h g f e d c b a

# digits
$ echo {1..20}
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
    
# with leading zeros
$ echo {01..20}
01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20
    
# reverse digit
$ echo {20..1}
20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1
   
# reversed with leading zeros
$ echo {20..01}
20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01

# combining multiple braces
$ echo {a..d}{1..3}
a1 a2 a3 b1 b2 b3 c1 c2 c3 d1 d2 d3

```

Brace expansion is the very first expansion that takes place, so it cannot be combined with any other expansions.

Only chars and digits can be used.

This won't work: `echo {$(date +$H)..24}`



## Make Multiple Directories with Sub-Directories


```bash
mkdir -p toplevel/sublevel_{01..09}/{child1,child2,child3}

```

This will create a top level folder called `toplevel`, nine folders inside of `toplevel` named `sublevel_01`, `sublevel_02`, etc. Then inside of those sublevels: `child1`, `child2`, `child3` folders, giving you:

```bash
toplevel/sublevel_01/child1
toplevel/sublevel_01/child2
toplevel/sublevel_01/child3
toplevel/sublevel_02/child1

```

and so on. I find this very useful for creating multiple folders and sub folders for my specific purposes, with one bash command. Substitute variables to help automate/parse information given to the script.



#### Remarks


[Bash Reference Manual: Brace Expansion](https://www.gnu.org/software/bash/manual/html_node/Brace-Expansion.html)

