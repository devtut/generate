---
metaTitle: "Perl - Reading a file's content into a variable"
description: "Path::Tiny, The manual way, File::Slurp, File::Slurper, Slurping a file into an array variable, Slurp file in one-liner"
---

# Reading a file's content into a variable



## Path::Tiny


Using the idiom from [The Manual Way](https://stackoverflow.com/documentation/perl/1779/reading-a-file-into-a-variable/5786/the-manual-way#t=201609131958379930125) several times in a script soon gets tedious so you might want to try a module.

```perl
use Path::Tiny;
my $contents = path($filename)->slurp;

```

You can pass a `binmode` option if you need control over file encodings, line endings etc. - see `man perlio`:

```perl
my $contents = path($filename)->slurp( {binmode => ":encoding(UTF-8)"} );

```

`Path::Tiny` also has [a lot of other functions](http://search.cpan.org/%7Edagolden/Path-Tiny-0.096/lib/Path/Tiny.pm) for dealing with files so it may be a good choice.



## The manual way


```perl
open my $fh, '<', $filename
    or die "Could not open $filename for reading: $!";
my $contents = do { local $/; <$fh> };

```

After opening the file (read `man perlio` if you want to read specific file encodings instead of raw bytes), the trick is in the `do` block: `<$fh>`, the file handle in a diamond operator, returns a single record from the file. The "input record separator" variable `$/` specifies what a "record" is—by default it is set to a newline character so "a record" means "a single line". As `$/` is a global variable, `local` does two things: it creates a temporary local copy of `$/` that will vanish at the end of the block, and gives it the (non-)value `undef` (the "value" which Perl gives to uninitialized variables). When the input record separator has that (non-)value, the diamond operator will return the entire file. (It considers the entire file to be a single line.)

Using `do`, you can even get around manually opening a file. For repeated reading of files,

```perl
sub readfile { do { local(@ARGV,$/) = $_[0]; <> } }
my $content = readfile($filename);

```

can be used. Here, another global variable(`@ARGV`) is localized to simulate the same process used when starting a perl script with parameters. `$/` is still `undef`, since the array in front of it "eats" all incoming arguments. Next, the diamond operator `<>` again delivers one record defined by `$/` (the whole file) and returns from the `do` block, which in turn return from the sub.

The sub has no explicit error handling, which is bad practice! If an error occurs while reading the file, you will receive `undef` as return value, as opposed to an empty string from an empty file.

Another disadvantage of the last code is the fact that you cannot use PerlIO for different file encodings—you always get raw bytes.



## File::Slurp


Don't use it. Although it has been around for a long time and is still the module most programmers will suggest, [it is broken and not likely to be fixed](http://blogs.perl.org/users/leon_timmermans/2015/08/fileslurp-is-broken-and-wrong.html).



## File::Slurper


This is a minimalist module that only slurps files into variables, nothing else.

```perl
use File::Slurper 'read_text';
my $contents = read_text($filename);

```

`read_text()` takes two optional parameters to specify the file encoding and whether line endings should be translated between the unixish LF or DOSish CRLF standards:

```perl
my $contents = read_text($filename, 'UTF-8', 1);

```



## Slurping a file into an array variable


```perl
open(my $fh, '<', "/some/path") or die $!;
my @ary = <$fh>;

```

When evaluated in list context, the diamond operator returns a list consisting of all the lines in the file (in this case, assigning the result to an array supplies list context). The line terminator is retained, and can be removed by chomping:

```perl
chomp(@ary); #removes line terminators from all the array elements.

```



## Slurp file in one-liner


Input record separator can be specified with `-0` switch (**zero**, not **capital O**). It takes an octal or hexadecimal number as value. Any value `0400` or above will cause Perl to slurp files, but by convention, the value used for this purpose is `0777`.

```perl
perl -0777 -e 'my $file = <>; print length($file)' input.txt

```

Going further with minimalism, specifying `-n` switch causes Perl to automatically read each line (in our case — the whole file) into variable `$_`.

```perl
perl -0777 -ne 'print length($_)' input.txt

```

