---
metaTitle: "Perl - Split a string on unquoted separators"
description: "parse_line(), Text::CSV or Text::CSV_XS"
---

# Split a string on unquoted separators



## parse_line()


Using `parse_line()` of [Text::ParseWords](http://perldoc.perl.org/Text/ParseWords.html):

```perl
use 5.010;
use Text::ParseWords;

my $line = q{"a quoted, comma", word1, word2};
my @parsed = parse_line(',', 1, $line);
say for @parsed;

```

Output:

```perl
"a quoted, comma"
 word1
 word2

```



## Text::CSV or Text::CSV_XS


```perl
use Text::CSV; # Can use Text::CSV which will switch to _XS if installed
$sep_char = ",";
my $csv = Text::CSV->new({sep_char => $sep_char});
my $line = q{"a quoted, comma", word1, word2};
$csv->parse($line);
my @fields = $csv->fields();
print join("\n", @fields)."\n";

```

Output:

```perl
a quoted, comma
 word1
 word2

```

### NOTES

<li>
By default, Text::CSV does not strip whitespace around separator character, the way `Text::ParseWords` does. However, adding `allow_whitespace=>1` to constructor attributes achieves that effect.

```perl
my $csv = Text::CSV_XS->new({sep_char => $sep_char, allow_whitespace=>1});  

```


Output:

```perl
a quoted, comma
word1
word2

```


</li>
<li>
The library supports escaping special characters (quotes, separators)
</li>
<li>
The library supports configurable separator character, quote character, and escape character
</li>

Documentatoin: [http://search.cpan.org/perldoc/Text::CSV](http://search.cpan.org/perldoc/Text::CSV)

