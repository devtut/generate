---
metaTitle: "Perl - Regular Expressions"
description: "Matching strings, Replace a string using regular expressions, Usage of &#92;Q and &#92;E in pattern matching, Parsing a string with a regex"
---

# Regular Expressions



## Matching strings


The `=~` operator attempts to match a regular expression (set apart by `/`) to a string:

```perl
my $str = "hello world";
print "Hi, yourself!\n" if $str =~ /^hello/;

```

`/^hello/` is the actual regular expression. The `^` is a special character that tells the regular expression to start with the beginning of the string and not match in the middle somewhere. Then the regex tries to find the following letters in order `h`, `e`, `l`, `l`, and `o`.

Regular expressions attempt to match the default variable (`$_`) if bare:

```perl
$_ = "hello world";

print "Ahoy!\n" if /^hello/;

```

You can also use different delimiters is you precede the regular expression with the `m` operator:

```perl
m~^hello~;
m{^hello}; 
m|^hello|;

```

This is useful when matching strings that include the `/` character:

```perl
print "user directory" if m|^/usr|;

```



## Replace a string using regular expressions


```perl
s/foo/bar/;         # replace "foo" with "bar" in $_
my $foo = "foo";
$foo =~ s/foo/bar/; # do the above on a different variable using the binding operator =~
s~ foo ~ bar ~;     # using ~ as a delimiter
$foo = s/foo/bar/r; # non-destructive r flag: returns the replacement string without modifying the variable it's bound to
s/foo/bar/g;        # replace all instances

```



## Usage of &#92;Q and &#92;E in pattern matching


### What's between &#92;Q and &#92;E is treated as normal characters

```perl
#!/usr/bin/perl

my $str = "hello.it's.me";

my @test = (
  "hello.it's.me",
    "hello/it's!me",
    );

sub ismatched($) { $_[0] ? "MATCHED!" : "DID NOT MATCH!" }

my @match = (
      [ general_match=> sub { ismatched /$str/ } ],
      [ qe_match    => sub { ismatched /\Q$str\E/ } ],
      );

for (@test) {
    print "\String = '$_':\n";

foreach my $method (@match) {
    my($name,$match) = @$method;
    print "  - $name: ", $match->(), "\n";
}

```

}

Output

> 

```perl
String = 'hello.it's.me':
  - general_match: MATCHED!
  - qe_match: MATCHED!
String = 'hello/it's!me':
  - general_match: MATCHED!
  - qe_match: DID NOT MATCH!

```






## Parsing a string with a regex


Generally, it's not a good idea to [use a regular expression to parse a complex structure](http://stackoverflow.com/a/1732454/1438). But it can be done. For instance, you might want to load data into hive table and fields are separated by comma but complex types like array are separated by a "|". Files contain records with all fields separated by comma and complex type are inside square bracket. In that case, this bit of disposable Perl might be sufficient:

```perl
echo "1,2,[3,4,5],5,6,[7,8],[1,2,34],5" | \
    perl -ne \
        'while( /\[[^,\]]+\,.*\]/ ){
            if( /\[([^\]\|]+)\]/){
                $text = $1;
                $text_to_replace = $text;
                $text =~ s/\,/\|/g;
                s/$text_to_replace/$text/;
            }
        } print'

```

You'll want to spot check the output:

> 
1,2,[3|4|5],5,6,[7|8],[1|2|34],5


