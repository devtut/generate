---
metaTitle: "Perl - Getting started with Perl Language"
description: "Getting started with Perl"
---

# Getting started with Perl Language



## Getting started with Perl


Perl tries to do what you mean:

```perl
print "Hello World\n";

```

The two tricky bits are the semicolon at the end of the line and the `\n`, which adds a newline (line feed). If you have a relatively new version of perl, you can use `say` instead of `print` to have the carriage return added automatically:

```perl
use feature 'say';
say "Hello World";

```

The say feature is also enabled automatically with a `use v5.10` (or higher) declaration:

```perl
use v5.10;
say "Hello World";

```

It's pretty common to just use [perl on the command line](http://perldoc.perl.org/perlrun.html) using the `-e` option:

```perl
$ perl -e 'print "Hello World\n"'
Hello World

```

Adding the `-l` option is one way to print newlines automatically:

```perl
$ perl -le 'print "Hello World"'
Hello World

```

If you want to enable [new features](http://perldoc.perl.org/feature.html), use the `-E` option instead:

```perl
$ perl -E 'say "Hello World"'
Hello World

```

You can also, of course, save the script in a file. Just remove the `-e` command line option and use the filename of the script: `perl script.pl`. For programs longer than a line, it's wise to turn on a couple of options:

```perl
use strict;
use warnings;

print "Hello World\n";

```

There's no real disadvantage other than making the code slightly longer. In exchange, the strict pragma prevents you from using code that is potentially unsafe and warnings notifies you of many common errors.

Notice the line-ending semicolon is optional for the last line, but is a good idea in case you later add to the end of your code.

For more options how to run Perl, see [perlrun](http://perldoc.perl.org/perlrun.html)  or type `perldoc perlrun` at a command prompt. For a more detailed introduction to Perl, see [perlintro](http://perldoc.perl.org/perlintro.html) or type `perldoc perlintro` at a command prompt. For a quirky interactive tutorial, [Try Perl](http://tryperl.pl).



#### Remarks


Perl is the camel of languages: useful, but not always beautiful. It has rather good documentation of its own which can be accessed using the `perldoc` command from your shell/command prompt. It's also available online at [perldoc.perl.org](http://perldoc.perl.org/).

