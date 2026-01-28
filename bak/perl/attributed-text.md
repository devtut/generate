---
metaTitle: "Perl - Attributed Text"
description: "Printing colored Text"
---

# Attributed Text



## Printing colored Text


```perl
#!/usr/bin/perl

use Term::ANSIColor;

print color("cyan"), "Hello", color("red"), "\tWorld", color("green"), "\tIt's Me!\n", color("reset");

```

[<img src="http://i.stack.imgur.com/FXQAm.png" alt="enter image description here" />](http://i.stack.imgur.com/FXQAm.png)

