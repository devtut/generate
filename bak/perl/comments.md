---
metaTitle: "Perl - Comments"
description: "Single-line comments, Multi-line comments"
---

# Comments



## Single-line comments


Single-line comments begin with a pound sign `#` and go to the end of the line:

```perl
# This is a comment

my $foo = "bar"; # This is also a comment

```



## Multi-line comments


Multi-line comments start with `=` and with the `=cut` statement. These are special comments called POD (Plain Old Documentation).

Any text between the markers will be commented out:

```perl
=begin comment

This is another comment.
And it spans multiple lines!

=end comment

=cut

```

