---
metaTitle: "Perl - Dates and Time"
description: "Date formatting"
---

# Dates and Time



## Date formatting


Time::Piece is available in perl 5 after version 10

```perl
use Time::Piece;

my $date = localtime->strftime('%m/%d/%Y');
print $date;

```

> 

```perl
Output
07/26/2016

```




