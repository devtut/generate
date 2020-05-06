---
metaTitle: "Perl - Best Practices"
description: "Using Perl::Critic"
---

# Best Practices



## Using Perl::Critic


If you'd like to start implementing best practices, for yourself or your team, then [Perl::Critic](https://metacpan.org/pod/Perl::Critic) is the best place to start. The module is based on the **[Perl Best Practices](http://shop.oreilly.com/product/9780596001735.do)** book by Damien Conway and does a fairly good job implementing the suggestions made therein.

> 
**Note:** **I should mention (and Conway himself says in the book) that these are suggestions. I've found the book provides solid reasoning in most cases, though I certainly don't agree with all of them. The important thing to remember is that, whatever practices you decide to adopt, you remain consistent. The more predictable your code is, the easier it will be to maintain.**


You can also try out Perl::Critic through your browser at [perlcritic.com](http://perlcritic.com/).

### Installation

```perl
cpan Perl::Critic

```

This will install the basic ruleset and a **perlcritic** script that can be called from the command line.

### Basic Usage

The [CPAN doc for perlcritic](https://metacpan.org/pod/distribution/Perl-Critic/bin/perlcritic) contains full documentation, so I will only be going over the most common use cases to get you started. Basic usage is to simply call perlcritic on the file:

```

perlcritic -1 /path/to/script.pl

```

perlcritic works both on scripts and on modules. The **-1** refers to the severity level of the rules you want to run against the script. There are five levels that correspond to how much Perl::Critic will pick apart your code.

**-5** is the most gentle and will only warn about potentially dangerous problems that could cause unexpected results. **-1** is the most brutal and will complain about things as small as your code being tidy or not. In my experience, keeping code compliant with level 3 is good enough to keep out of danger without getting too persnickety.

By default, any failures will list the reason and severity the rule triggers on:

```perl
perlcritic -3 --verbose 8 /path/to/script.pl

Debugging module loaded at line 16, column 1.  You've loaded Data::Dumper, which probably shouln't be loaded in production.  (Severity: 4)
Private subroutine/method '_sub_name' declared but not used at line 58, column 1.  Eliminate dead code.  (Severity: 3)
Backtick operator used at line 230, column 37.  Use IPC::Open3 instead.  (Severity: 3)
Backtick operator used at line 327, column 22.  Use IPC::Open3 instead.  (Severity: 3)

```

### Viewing Policies

You can quickly see which rules are being triggered and why by utilizing perlcritic's **--verbose** option:

Setting the level to 8 will show you the rule that triggered a warning:

```perl
perlcritic -3 --verbose 8 /path/to/script.pl

[Bangs::ProhibitDebuggingModules] Debugging module loaded at line 16, column 1.  (Severity: 4)
[Subroutines::ProhibitUnusedPrivateSubroutines] Private subroutine/method '_sub_name' declared but not used at line 58, column 1.  (Severity: 3)
[InputOutput::ProhibitBacktickOperators] Backtick operator used at line 230, column 37.  (Severity: 3)
[InputOutput::ProhibitBacktickOperators] Backtick operator used at line 327, column 22.  (Severity: 3)

```

While a level of 11 will show the specific reasons why the rule exists:

```perl
perlcritic -3 --verbose 11 /path/to/script.pl

Debugging module loaded at line 16, near 'use Data::Dumper;'.
  Bangs::ProhibitDebuggingModules (Severity: 4)
    This policy prohibits loading common debugging modules like the
    Data::Dumper manpage.

    While such modules are incredibly useful during development and
    debugging, they should probably not be loaded in production use. If this
    policy is violated, it probably means you forgot to remove a `use
    Data::Dumper;' line that you had added when you were debugging.
Private subroutine/method '_svn_revisions_differ' declared but not used at line 58, near 'sub _sub_name {'.
  Subroutines::ProhibitUnusedPrivateSubroutines (Severity: 3)
    By convention Perl authors (like authors in many other languages)
    indicate private methods and variables by inserting a leading underscore
    before the identifier. This policy catches such subroutines which are
    not used in the file which declares them.

    This module defines a 'use' of a subroutine as a subroutine or method
    call to it (other than from inside the subroutine itself), a reference
    to it (i.e. `my $foo = \&_foo'), a `goto' to it outside the subroutine
    itself (i.e. `goto &_foo'), or the use of the subroutine's name as an
    even-numbered argument to `use overload'.
Backtick operator used at line 230, near 'my $filesystem_diff = join q{}, `diff $trunk_checkout $staging_checkout`;'.
  InputOutput::ProhibitBacktickOperators (Severity: 3)
    Backticks are super-convenient, especially for CGI programs, but I find
    that they make a lot of noise by filling up STDERR with messages when
    they fail. I think its better to use IPC::Open3 to trap all the output
    and let the application decide what to do with it.

        use IPC::Open3 'open3';
        $SIG{CHLD} = 'IGNORE';

        @output = `some_command`;                      #not ok

        my ($writer, $reader, $err);
        open3($writer, $reader, $err, 'some_command'); #ok;
        @output = <$reader>;  #Output here
        @errors = <$err>;     #Errors here, instead of the console
Backtick operator used at line 327, near 'my $output = `$cmd`;'.
  InputOutput::ProhibitBacktickOperators (Severity: 3)
    Backticks are super-convenient, especially for CGI programs, but I find
    that they make a lot of noise by filling up STDERR with messages when
    they fail. I think its better to use IPC::Open3 to trap all the output
    and let the application decide what to do with it.

        use IPC::Open3 'open3';
        $SIG{CHLD} = 'IGNORE';

        @output = `some_command`;                      #not ok

        my ($writer, $reader, $err);
        open3($writer, $reader, $err, 'some_command'); #ok;
        @output = <$reader>;  #Output here
        @errors = <$err>;     #Errors here, instead of the console

```

### Ignoring Code

There will be times when you can't comply with a Perl::Critic policy. In those cases, you can wrap special comments, "**## use critic()**" and "**## no critic**", around your code to make Perl::Critic ignore them. Simply add the rules you want to ignore in the parentheses (multiples can be separated by a comma).

```perl
##no critic qw(InputOutput::ProhibitBacktickOperator)
my $filesystem_diff = join q{}, `diff $trunk_checkout $staging_checkout`;
## use critic

```

Make sure to wrap the entire code block or Critic may not recognize the ignore statement.

```perl
## no critic (Subroutines::ProhibitExcessComplexity)
sub no_time_to_refactor_this {
    ...
}
## use critic

```

Note that there are certain policies that are run on the document level and cannot be exempted this way. However, they can be turned off...

### Creating Permanent Exceptions

Using ## no critic() is nice, but as you start to adopt coding standards, you will likely want to make permanent exceptions to certain rules. You can do this by creating a **.perlcriticrc** configuration file.

This file will allow you to customize not only which policies are run, but how they are run. Using it is as simple as placing the file in your home directory (in Linux, unsure if it's the same place on Windows). Or, you can specify the config file when running the command using the **--profile** option:

```perl
perlcritic -1 --profile=/path/to/.perlcriticrc /path/to/script.pl

```

Again, the [perlcritic CPAN page](https://metacpan.org/pod/distribution/Perl-Critic/bin/perlcritic) has a full list of these options. I will list some examples from my own config file:

Apply basic settings:

```perl
#very very harsh
severity = 1
color-severity-medium = bold yellow
color-severity-low = yellow
color-severity-lowest = bold blue

```

Disable a rule (note the dash in front of the policy name):

```perl
# do not require version control numbers
[-Miscellanea::RequireRcsKeywords]

# pod spelling is too over-zealous, disabling
[-Documentation::PodSpelling]

```

Modifying a rule:

```perl
# do not require checking for print failure ( false positives for printing to stdout, not filehandle )
[InputOutput::RequireCheckedSyscalls]
    functions = open close

# Allow specific unused subroutines for moose builders
[Subroutines::ProhibitUnusedPrivateSubroutines]
private_name_regex = _(?!build_)\w+

```

### Conclusion

Properly utilized, Perl::Critic can be an invaluable tool to help teams keep their coding consistent and easily maintainable no matter what best practice policies you employ.

