---
metaTitle: "Perl - Control Statements"
description: "Conditionals, Loops"
---

# Control Statements



## Conditionals


Perl supports many kinds of conditional statements (statements that are based on boolean results). The most common conditional statements are if-else, unless, and ternary statements. `given` statements are introduced as a switch-like construct from C-derived languages and are available in versions Perl 5.10 and above.

### If-Else Statements

The basic structure of an if-statement is like this:

```perl
if (EXPR) BLOCK
if (EXPR) BLOCK else BLOCK
if (EXPR) BLOCK elsif (EXPR) BLOCK ...
if (EXPR) BLOCK elsif (EXPR) BLOCK ... else BLOCK

```

For simple if-statements, the if can precede or succeed the code to be executed.

```perl
$number = 7;
if ($number > 4) { print "$number is greater than four!"; }

# Can also be written this way
print "$number is greater than four!" if $number > 4;

```



## Loops


Perl supports many kinds of loop constructs: for/foreach, while/do-while, and until.

```perl
@numbers = 1..42;
for (my $i=0; $i <= $#numbers; $i++) {
    print "$numbers[$i]\n";
}

#Can also be written as
foreach my $num (@numbers) {
    print "$num\n";
}

```

The while loop evaluates the conditional **before** executing the associated block. So, sometimes the block is never executed. For example, the following code would never be executed if the filehandle `$fh` was the filehandle for an empty file, or if was already exhausted before the conditional.

```perl
while (my $line = readline $fh) {
    say $line;
}

```

The `do`/`while` and `do`/`until` loops, on the other hand, evaluate the conditional **after** each time the block is executed. So, a `do`/`while` or a `do`/`until` loop is always executed at least once.

```perl
my $greeting_count = 0;
do {
    say "Hello";
    $greeting_count++;
} until ( $greeting_count > 1)

# Hello
# Hello

```

