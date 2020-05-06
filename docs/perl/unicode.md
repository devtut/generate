---
metaTitle: "Perl - Unicode"
description: "The utf8 pragma: using Unicode in your sources, Create filenames, Read filenames, Command line switches for one-liners, Standard I/O, File handles, Handling invalid UTF-8"
---

# Unicode



## The utf8 pragma: using Unicode in your sources


The [`utf8`](http://perldoc.perl.org/utf8.html) pragma indicates that the source code will be interpreted as UTF-8. Of course, this will only work if your text editor is also saving the source as UTF-8 encoded.

Now, string literals can contain arbitrary Unicode characters; identifiers can also contain Unicode but only word-like characters (see [perldata](http://perldoc.perl.org/perldata.html#Identifier-parsing) and [perlrecharclass](http://perldoc.perl.org/perlrecharclass.html#Backslash-sequences) for more information):

```perl
use utf8;
my $var1 = '§я§©😄';      # works fine
my $я = 4;                # works since я is a word (matches \w) character
my $p§2 = 3;              # does not work since § is not a word character.
say "ya" if $var1 =~ /я§/; # works fine (prints "ya")

```

**Note**: When printing text to the terminal, make sure it supports UTF-8.*

There may be complex and counter-intuitive relationships between output and source encoding. Running on a UTF-8 terminal, you may find that adding the `utf8` pragma seems to break things:

```perl
$ perl -e 'print "Møøse\n"'
Møøse
$ perl -Mutf8 -e 'print "Møøse\n"'
M��se
$ perl -Mutf8 -CO -e 'print "Møøse\n"'
Møøse

```

In the first case, Perl treats the string as raw bytes and prints them like that. As these bytes happen to be valid UTF-8, they look correct even though Perl doesn't really know what characters they are (e.g. `length("Møøse")` will return 7, not 5). Once you add `-Mutf8`, Perl correctly decodes the UTF-8 source to characters, but output is in Latin-1 mode by default and printing Latin-1 to a UTF-8 terminal doesn't work. Only when you switch `STDOUT` to UTF-8 using `-CO` will the output be correct.

`use utf8` doesn't affect standard I/O encoding nor file handles!



## Create filenames


The following examples use the UTF-8 encoding to represent filenames (and directory names) on disk. If you want to use another encoding, you should use [`Encode::encode(...)`](https://metacpan.org/pod/Encode#encode).

```perl
use v5.14;
# Make Perl recognize UTF-8 encoded characters in literal strings.
# For this to work: Make sure your text-editor is using UTF-8, so
# that bytes on disk are really UTF-8 encoded.
use utf8;

# Ensure that possible error messages printed to screen are converted to UTF-8.
# For this to work: Check that your terminal emulator is using UTF-8.
binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
    
my $filename = 'æ€'; # $filename is now an internally UTF-8 encoded string. 

# Note: in the following it is assumed that $filename has the internal UTF-8
#  flag set, if $filename is pure ASCII, it will also work since its encoding
#  overlaps with UTF-8. However, if it has another encoding like extended ASCII,
#  $filename will be written with that encoding and not UTF-8. 
# Note: it is not necessary to encode $filename as UTF-8 here
#  since Perl is using UTF-8 as its internal encoding of $filename already

# Example1  -- using open()
open ( my $fh, '>', $filename ) or die "Could not open '$filename': $!";
close $fh;

# Example2 -- using qx() and touch
qx{touch $filename};

# Example3 -- using system() and touch
system 'touch', $filename;

# Example4 -- using File::Touch
use File::Touch;
eval { touch( $filename ) }; die "Could not create file '$filename': $!" if $@;

```



## Read filenames


Perl does not attempt to decode filenames returned by builtin functions or modules. Such strings representing filenames should always be decoded explicitly, in order for Perl to recognize them as Unicode.

```perl
use v5.14;
use Encode qw(decode_utf8);

# Ensure that possible error messages printed to screen are converted to UTF-8.
# For this to work: Check that you terminal emulator is using UTF-8.
binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

# Example1  -- using readdir()
my $dir = '.';
opendir(my $dh, $dir) or die "Could not open directory '$dir': $!";
while (my $filename = decode_utf8(readdir $dh)) {
    # Do something with $filename
}
close $dh;

# Example2  -- using getcwd()
use Cwd qw(getcwd);
my $dir = decode_utf8( getcwd() );

# Example3  -- using abs2rel()
use File::Spec;
use utf8;
my $base = 'ø';
my $path = "$base/b/æ";
my $relpath = decode_utf8( File::Spec->abs2rel( $path, $base ) );
# Note: If you omit $base, you need to encode $path first:
use Encode qw(encode_utf8);
my $relpath = decode_utf8( File::Spec->abs2rel( encode_utf8( $path ) ) );

# Example4  -- using File::Find::Rule (part1 matching a filename)
use File::Find::Rule;
use utf8;
use Encode qw(encode_utf8);
my $filename = 'æ';
# File::Find::Rule needs $filename to be encoded
my @files = File::Find::Rule->new->name( encode_utf8($filename) )->in('.');
$_ = decode_utf8( $_ ) for @files;

# Example5  -- using File::Find::Rule (part2 matching a regular expression)
use File::Find::Rule;
use utf8;
my $pat = '[æ].$'; # Unicode pattern
# Note: In this case:  File::Find::Rule->new->name( qr/$pat/ )->in('.')
#  will not work since $pat is Unicode and filenames are bytes
#  Also encoding $pat first will not work correctly
my @files;
File::Find::Rule->new->exec( sub { wanted( $pat, \@files ) } )->in('.');
$_ = decode_utf8( $_ ) for @files;
sub wanted {
    my ( $pat, $files ) = @_;
    my $name = decode_utf8( $_ );
    my $full_name = decode_utf8( $File::Find::name );
    push @$files, $full_name if $name =~ /$pat/;
}

```

Note: if you are concerned about invalid UTF-8 in the filenames, the use of `decode_utf8( ... )` in the above examples should probably be replaced by `decode( 'utf-8', ... )`. This is because `decode_utf8( ... )` is a synonym for `decode( 'utf8', ... )` and there is a difference between the encodings `utf-8` and `utf8` (see [Remarks](http://stackoverflow.com/documentation/perl/4375/unicode&a=remarks) below for more information) where `utf-8` is more strict on what is acceptable than `utf8`.



## Command line switches for one-liners


### Enable utf8 pragma

In order to enable `utf8` pragma in one-liner, perl interpreter should be called with `-Mutf8` option:

```perl
perl -Mutf8 -E 'my $人 = "human"; say $人'

```

### Unicode handling with -C switch

The `-C` command line flag lets you control Unicode features. It can be followed by a list of option letters.

### Standard I/O

- `I` - `STDIN` will be in **UTF-8**
- `O` - `STDOUT` will be in **UTF-8**
- `E` - `STDERR` will be in **UTF-8**
- `S` - shorthand for `IOE`, standard I/O streams will be in **UTF-8**

```perl
echo "Ματαιότης ματαιοτήτων" | perl -CS -Mutf8 -nE 'say "ok" if /Ματαιότης/'

```

### Script's arguments

- `A` - treats `@ARGV` as an array of **UTF-8** encoded strings

```perl
perl -CA -Mutf8 -E 'my $arg = shift; say "anteater" if $arg eq "муравьед"' муравьед

```

### Default PerlIO layer

- `i` - **UTF-8** is the default PerlIO layer for input streams
- `o` - **UTF-8** is the default PerlIO layer for output streams
- `D` - shorthand for `io`

```perl
perl -CD -Mutf8 -e 'open my $fh, ">", "utf8.txt" or die $!; print $fh "개미 조심해"'

```

`-M` and `-C` switches may be combined:

```perl
perl -CASD -Mutf8 -E 'say "Ματαιότης ματαιοτήτων\n"';

```



## Standard I/O


The encoding to be used for the standard I/O filehandles (`STDIN`, `STDOUT`, and `STDERR`), can be set separately for each handle using [`binmode`](http://perldoc.perl.org/functions/binmode.html):

```perl
binmode STDIN, ':encoding(utf-8)';
binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

```

Note: when reading one would in general prefer `:encoding(utf-8)` over `:utf8`, see [Remarks](http://stackoverflow.com/documentation/perl/4375/unicode&a=remarks) for more information.

Alternatively, you can use the [`open`](http://perldoc.perl.org/open.html) pragma.

```perl
# Setup such that all subsequently opened input streams will use ':encoding(utf-8)'
# and all subsequently opened output streams will use ':utf8'
# by default
use open (IN => ':encoding(utf-8)', OUT => ':utf8');
# Make the (already opened) standard file handles inherit the setting 
# given by the IO settings for the open pragma
use open ( :std );
# Now, STDIN has been converted to ':encoding(utf-8)', and 
# STDOUT and STDERR have ':utf8'

```

Alternatively, to set all filehandles (both those yet to be opened and also the standard ones) to use `:encoding(utf-8)`:

```perl
use open qw( :encoding(utf-8) :std );

```



## File handles


### Setting encoding with open()

When opening a text file, you may specify it's encoding explicitly with a three-argument [`open()`](http://perldoc.perl.org/functions/open.html). This en-/decoder attached to a file handle is called an "I/O layer":

```perl
my $filename = '/path/to/file';
open my $fh, '<:encoding(utf-8)', $filename or die "Failed to open $filename: $!";

```

See [Remarks](http://stackoverflow.com/documentation/perl/4375/unicode&a=remarks) for a discussion of the differences between `:utf8` and `:encoding(utf-8)`.

### Setting encoding with binmode()

Alternatively, you may use binmode() to set the encoding for individual file handle:

```perl
my $filename = '/path/to/file';
open my $fh, '<', $filename or die "Failed to open $filename: $!";
binmode $fh, ':encoding(utf-8)';

```

### open pragma

To avoid setting encoding for each file handle separately, you may use the [`open`](http://perldoc.perl.org/open.html) pragma to set a default I/O layer used by all subsequent calls to the `open()` function and similar operators within the lexical scope of this pragma:

```perl
# Set input streams to ':encoding(utf-8)' and output streams to ':utf8'
use open (IN => ':encoding(utf-8)', OUT => ':utf8');
# Or to set all input and output streams to ':encoding(utf-8)'
use open ':encoding(utf-8)';

```

### Setting encoding with command line -C flag

Finally, it is also possible to run the perl interpreter with a `-CD` flag that applies UTF-8 as the default I/O layer. However, this option should be avoided since it relies on specific user behaviour which cannot be predicted nor controlled.



## Handling invalid UTF-8


### Reading invalid UTF-8

When reading UTF-8 encoded data, it is important to be aware of the fact the UTF-8 encoded data can be invalid or malformed. Such data should usually not be accepted by your program (unless you know what you are doing). When unexpectedly encountering malformed data, different actions can be considered:

- Print stacktrace or error message, and abort program gracefully, or
- Insert a substitution character at the place where the malformed byte sequence appeared, print a warning message to STDERR and continue reading as nothing happened.

By default, Perl will [`warn`](http://perldoc.perl.org/functions/warn.html) you about encoding glitches, but it will not abort your program.
You can make your program abort by making UTF-8 warnings fatal, but be aware of the caveats in [Fatal Warnings](http://perldoc.perl.org/warnings.html#Fatal-Warnings).

The following example writes 3 bytes in encoding ISO 8859-1 to disk. It then tries to read the bytes back again as UTF-8 encoded data. One of the bytes, `0xE5`, is an invalid UTF-8 one byte sequence:

```perl
use strict;
use warnings;
use warnings FATAL => 'utf8';

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
my $bytes = "\x{61}\x{E5}\x{61}";  # 3 bytes in iso 8859-1: aåa
my $fn = 'test.txt';
open ( my $fh, '>:raw', $fn ) or die "Could not open file '$fn': $!";
print $fh $bytes;
close $fh;
open ( $fh, "<:encoding(utf-8)", $fn ) or die "Could not open file '$fn': $!";
my $str = do { local $/; <$fh> };
close $fh;
print "Read string: '$str'\n";

```

The program will abort with a fatal warning:

```perl
utf8 "\xE5" does not map to Unicode at ./test.pl line 10.

```

Line 10 is here the second last line, and the error occurs in the part of the line with `<$fh>` when trying to read a line from the file.

If you don't make warnings fatal in the above program, Perl will still print the warning. However, in this case it will try to recover from the malformed byte `0xE5` by inserting the four characters `\xE5` into the stream, and then continue with the next byte. As a result, the program will print:

```perl
Read string: 'a\xE5a'

```



#### Remarks


### A Warning on Filename Encoding

It should be worth mentioning that Filename Encoding is not only **platform** specific but also **filesystem** specific.

It is never **entirely** safe to assume (but often usually is) that just because you can encode and write to a given filename, that when you later try to open that same filename for reading, it will still be called the same thing.

For instance, if you write to a filesystem such as `FAT16` which doesn't support unicode, your filenames might silently get translated into ASCII-compatible forms.

But it is even **less** safe to assume that a file you can create, read and write to by explicit naming will be called the same thing when queried through other calls, for instance, `readdir` might return different bytes for your filename than you specified to `open`.

On some systems such as VAX, you can't even always assume that `readdir` will return the same filename you specified with `open` for filenames as simple as `foo.bar`, because filename **extensions** can be mangled by the OS.

Also, on UNIX, there is a very liberal set of legal characters for filenames that the OS permits, excluding only `/` and `\0`, where as on Windows, there are specific ranges of characters that are forbidden in filenames and will cause errors.

**Exercise much caution here, avoid fancy tricks with filenames if you have a choice**, and always have tests to make sure any fancy tricks you **do** use are consistent.

**Exercise doubly as much caution** if you're writing code intended to be run on platforms outside your control, such as if you're writing code that is intended for `CPAN`, and assume at least 5% of your user base will be stuck using some ancient or broken technology, either by choice, by accident, or by powers outside their control, and that these will conspire to create bugs for them.

### :encoding(utf8) vs :utf8

Since UTF-8 is one of the internal formats for representation of strings in Perl, the encoding/decoding step may often be skipped. Instead of `:encoding(utf-8)`, you can simply use `:utf8`, if your data is already in UTF-8. `:utf8` can be used safely with output streams, whereas for input stream it can be dangerous, because it causes internal inconsistency when you have invalid byte sequences. Also, using `:utf8` for input may result in security breaches, so the use of `:encoding(utf-8)` is advisable.

More details: [What is the difference between :encoding and :utf8](http://perldoc.perl.org/perlunifaq.html#What-is-the-difference-between-%3aencoding-and-%3autf8%3f)

### UTF-8 vs utf8 vs UTF8

As of Perl `v5.8.7`, `"UTF-8"` (with dash) means UTF-8 in its strict and security-conscious form, whereas `"utf8"` means UTF-8 in its liberal and loose form.

For example, `"utf8"` can be used for code points that don't exist in Unicode, like `0xFFFFFFFF`. Correspondingly, invalid UTF-8 byte sequences like `"\x{FE}\x{83}\x{BF}\x{BF}\x{BF}\x{BF}\x{BF}"` will decode into an invalid Unicode (but valid Perl) codepoint (`0xFFFFFFFF`) when using `"utf8"`, whereas the `"UTF-8"` encoding would not allow decoding to codepoints outside the range of valid Unicode and  would give you a substitution character (`0xFFFD`) instead.

Since encoding names are case insensitive, `"UTF8"` is the same as `"utf8"` (i.e. **non-strict** variant).

More details: [UTF-8 vs. utf8 vs. UTF8](http://perldoc.perl.org/Encode.html#UTF-8-vs.-utf8-vs.-UTF8)

### More Reading

Details about Perl's Unicode handling is described in more detail in the following sources:

- [perlunicode](https://metacpan.org/pod/perlunicode)
- [perlunitut](https://metacpan.org/pod/distribution/perl/pod/perlunitut.pod)
- [perluniintro](https://metacpan.org/pod/distribution/perl/pod/perluniintro.pod)
- [perlunifaq](https://metacpan.org/pod/distribution/perl/pod/perlunifaq.pod)
- [perlunicook](https://metacpan.org/pod/distribution/perl/pod/perlunicook.pod)
- [utf8 pragma](http://perldoc.perl.org/utf8.html)
- [unicode_strings feature](http://perldoc.perl.org/feature.html#The-%27unicode_strings%27-feature)
- [open pragma](http://perldoc.perl.org/open.html)
- [PerlIO](http://perldoc.perl.org/PerlIO.html)
- [PerlIO::encoding](http://perldoc.perl.org/PerlIO/encoding.html)
- [open function](http://perldoc.perl.org/functions/open.html)
- [Encode](https://metacpan.org/pod/Encode)
- [perlrun - command line switches](http://perldoc.perl.org/perlrun.html#Command-Switches)
- [Chapter 6, Programming Perl](http://shop.oreilly.com/product/9780596004927.do)

Posts from stackoverflow.com (caveat: might not be up-to-date):

- [Why does modern Perl avoid UTF-8 by default?](http://stackoverflow.com/q/6162484/2173773)

Youtube videos:

- [A Million Billion Squiggly Characters](https://www.youtube.com/watch?v=TmTeXcEixEg) by Ricardo Signes at YAPC NA 2016.

