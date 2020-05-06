---
metaTitle: "Perl - Perl script debugging"
description: "Run script in debug mode, Use a nonstandard debugger"
---

# Perl script debugging



## Run script in debug mode


To run script in debug mode you should add `-d` option in the command line:

```perl
$perl -d script.pl

```

If `t` is specified, it indicates to the debugger that threads will be used in the code being debugged:

```perl
$perl -dt script.pl

```

Additional info at `perldoc`[perlrun](http://perldoc.perl.org/perlrun.html#*-dt*)



## Use a nonstandard debugger


`$perl -d:MOD script.pl` runs the program under the control of a debugging, profiling, or tracing module installed as `Devel::MOD`.

For example, `-d:NYTProf` executes the program using the  [`Devel::NYTProf` profiler](http://perldoc.perl.org/perlrun.html#*-d%3a*_MOD%5B%3dbar%2cbaz%5D_).

See all [available Devel modules](https://metacpan.org/search?q=Devel%3A%3A&search_type=modules) here

Recommended modules:

- [`Devel::NYTProf`](https://metacpan.org/pod/Devel::NYTProf) -- Powerful fast feature-rich Perl source code profiler
- [`Devel::Trepan`](https://metacpan.org/pod/Devel::Trepan) -- A modular gdb-like Perl debugger
- [`Devel::MAT`](https://metacpan.org/pod/Devel::MAT) -- Perl Memory Analysis Tool
- [`Devel::hdb`](https://metacpan.org/pod/Devel::hdb) -- Perl debugger as a web page and REST service
- [`Devel::DebugHooks::KillPrint`](https://metacpan.org/pod/release/KES/Devel-DebugHooks-0.02_16/lib/Devel/DebugHooks.pm) -- Allows to forget about debugging by `print` statement
- [`Devel::REPL`](https://metacpan.org/pod/Devel::REPL) -- A modern perl interactive shell
- [`Devel::Cover`](https://metacpan.org/pod/Devel::Cover) -- Code coverage metrics for Perl

