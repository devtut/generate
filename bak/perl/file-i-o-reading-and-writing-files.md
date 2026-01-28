---
metaTitle: "Perl - File I/O (reading and writing files)"
description: "Opening A FileHandle for Reading, Reading from a file, Write to a file, use autodie and you won't need to check file open/close failures, Rewind a filehandle, Reading and Writing gzip compressed files, Setting the default Encoding for IO, Reading from and writing to a file"
---

# File I/O (reading and writing files)




## Opening A FileHandle for Reading


### Opening Generic ASCII Text Files

```perl
open my $filehandle, '<', $name_of_file or die "Can't open $name_of_file, $!";

```

This is the basic idiom for "default" File IO and makes `$filehandle` a readable input stream of `bytes`, filtered by a default system-specific decoder, which can be locally set with the [`open` pragma](http://stackoverflow.com/documentation/perl/1604/file-i-o-reading-and-writing-files/24140/setting-the-default-encoding-for-io)

Perl itself does not handle errors in file opening, so you have to handle those yourself by checking the exit condition of `open`. `$!` is populated with the error message that caused open to fail.

On Windows, the default decoder is a "CRLF" filter, which maps any "\r\n" sequences in the input to "\n"

### Opening Binary Files

```perl
open my $filehandle, '<:raw', 'path/to/file' or die "Can't open $name_of_file, $!";

```

This specifies that Perl should **not** perform a `CRLF` translation on Windows.

### Opening UTF8 Text Files

```perl
open my $filehandle, '<:raw:encoding(utf-8)', 'path/to/file' 
   or die "Can't open $name_of_file, $!";

```

This specifies that Perl should both avoid `CRLF` translation, and then decode the resulting bytes into strings of **characters** ( internally implemented as arrays of integers which can exceed 255 ), instead of strings of **bytes**



## Reading from a file


```perl
my $filename = '/path/to/file';

open my $fh, '<', $filename or die "Failed to open file: $filename"; 
    
# You can then either read the file one line at a time...
while(chomp(my $line = <$fh>)) {
    print $line . "\n";
}

# ...or read whole file into an array in one go
chomp(my @fileArray = <$fh>); 

```

If you know that your input file is UTF-8, you can specify the encoding:

```perl
open my $fh, '<:encoding(utf8)', $filename or die "Failed to open file: $filename";

```

After finished reading from the file, the filehandle should be closed:

```perl
close $fh or warn "close failed: $!";

```

See also: [Reading a file into a variable](http://stackoverflow.com/documentation/perl/1779/reading-a-file-into-a-variable#t=201609170552195534952)

Another and **faster** way to read a file is to use File::Slurper Module. This is useful if you work with many files.

```perl
use File::Slurper;
my $file = read_text("path/to/file"); # utf8 without CRLF transforms by default
print $file; #Contains the file body

```

See also: [[Reading a file with slurp]](https://metacpan.org/pod/File::Slurper)



## Write to a file


This code opens a file for writing. Returns an error if the file couldn't be opened. Also closes the file at the end.

```perl
#!/usr/bin/perl
use strict;
use warnings;
use open qw( :encoding(UTF-8) :std ); # Make UTF-8 default encoding

# Open "output.txt" for writing (">") and from now on, refer to it as the variable $fh.
open(my $fh, ">", "output.txt")
# In case the action failed, print error message and quit.
or die "Can't open > output.txt: $!";

```

Now we have an open file ready for writing which we access through `$fh` (this variable is called a **filehandle**). Next we can direct output to that file using the `print` operator:

```perl
# Print "Hello" to $fh ("output.txt").
print $fh "Hello";
# Don't forget to close the file once we're done!
close $fh or warn "Close failed: $!";

```

The `open` operator has a scalar variable (`$fh` in this case) as its first parameter. Since it is defined in the `open` operator it is treated as a **filehandle**. Second parameter `">"` (greater than) defines that the file is opened for writing. The last parameter is the path of the file to write the data to.

To write the data into the file, the `print` operator is used along with the **filehandle**. Notice that in the `print` operator there is no comma between the **filehandle** and the statement itself, just whitespace.



## "use autodie" and you won't need to check file open/close failures


### `autodie` allows you to work with files without having to explicitly check for open/close failures.

Since Perl 5.10.1, the [`autodie`](http://perldoc.perl.org/autodie.html) pragma has been available in core Perl. When used, Perl will automatically check for errors when opening and closing files.

Here is an example in which all of the lines of one file are read and then written to the end of a log file.

```perl
use 5.010;    # 5.010 and later enable "say", which prints arguments, then a newline
use strict;   # require declaring variables (avoid silent errors due to typos)
use warnings; # enable helpful syntax-related warnings
use open qw( :encoding(UTF-8) :std ); # Make UTF-8 default encoding
use autodie;  # Automatically handle errors in opening and closing files

open(my $fh_in, '<', "input.txt"); # check for failure is automatic

# open a file for appending (i.e. using ">>")
open( my $fh_log, '>>', "output.log"); # check for failure is automatic

while (my $line = readline $fh_in) # also works: while (my $line = <$fh_in>)
{
     # remove newline
     chomp $line;

     # write to log file
     say $fh_log $line or die "failed to print '$line'"; # autodie doesn't check print
}

# Close the file handles (check for failure is automatic)
close $fh_in;
close $fh_log;

```

By the way, you should technically always check `print` statements. Many people don't, but `perl` (the Perl interpreter) doesn't do this automatically and [neither does `autodie`](http://perldoc.perl.org/autodie.html#SYNOPSIS).



## Rewind a filehandle


Sometimes it is needful to backtrack after reading.

```perl
# identify current position in file, in case the first line isn't a comment
my $current_pos = tell; 

while (my $line = readline $fh)
{
    if ($line =~ /$START_OF_COMMENT_LINE/)
    {
        push @names, get_name_from_comment($line);
    }
    else {
        last; # break out of the while loop
    }
    $current_pos = tell; # keep track of current position, in case we need to rewind the next line read
}

# Step back a line so that it can be processed later as the first data line
seek $fh, $current_pos, 0;

```



## Reading and Writing gzip compressed files


### Writing a gzipped file

To write a gzipped file, `use` the module `IO::Compress::Gzip` and create a filehandle by creating a new instance of `IO::Compress::Gzip` for the desired output file:

```perl
use strict;
use warnings;
use open qw( :encoding(UTF-8) :std ); # Make UTF-8 default encoding

use IO::Compress::Gzip;

my $fh_out = IO::Compress::Gzip->new("hello.txt.gz");

print $fh_out "Hello World!\n";

close $fh_out;

use IO::Compress::Gzip;

```

### Reading from a gzipped file

To read from a gzipped file, `use` the module `IO::Uncompress::Gunzip` and then create a filehandle by creating a new instance of `IO::Uncompress::Gunzip` for the input file:

```perl
#!/bin/env perl
use strict;
use warnings;
use open qw( :encoding(UTF-8) :std ); # Make UTF-8 default encoding

use IO::Uncompress::Gunzip;

my $fh_in = IO::Uncompress::Gunzip->new("hello.txt.gz");

my $line = readline $fh_in;

print $line;

```



## Setting the default Encoding for IO


```perl
# encode/decode UTF-8 for files and standard input/output
use open qw( :encoding(UTF-8) :std ); 

```

This `pragma` changes the default mode of reading and writing text ( files, standard input, standard output, and standard error ) to UTF-8, which is typically what you want when writing new applications.

ASCII is a subset of UTF-8, so this is not expected to cause any problems with legacy ASCII files and will help protect you the accidental file corruption that can happen when treating UTF-8 files as ASCII.

However, it is important that you know what the encoding of your files is that you are dealing with and handle them accordingly. ([Reasons that we should not ignore Unicode.](http://stackoverflow.com/a/6163129/215487)) For more in depth treatment of Unicode, please see the [Perl Unicode topic](http://stackoverflow.com/documentation/perl/4375/unicode).



## Reading from and writing to a file


Before reading and writing text files you should know what encoding to use. [See the Perl Unicode Documentation for more details on encoding](http://stackoverflow.com/documentation/perl/4375/unicode). Here we show the setting of UTF-8 as the default encoding and decoding for the function `open`. This is done by using the `open` pragma near the top of your code (right after `use strict;` and `use warnings;` would be appropriate):

```perl
use strict;
use warnings;
use open qw( :encoding(UTF-8) :std ); 

```

The `open` function creates a filehandle that is used for reading from and/or writing to a file. The `open` function has the signature

`open(FILEHANDLE, MODE, FILEPATH)` and returns a false value if the operation fails. The error description is then stored to `$!`.

**Reading**

```perl
#!/usr/bin/perl
use strict;
use warnings;
use open qw( :encoding(UTF-8) :std ); # Make UTF-8 default encoding

my $file_path = "/path/to/file";
open(my $file_handle, '<', $file_path) or die "Could not open file! $!";

while(my $row = <$file_handle>) {
    print chomp($row), "\n";
}

close $file_handle 
      or warn "Close failed!";

```

**Writing**

```perl
#!/usr/bin/perl
use strict;
use warnings;
use open qw( :encoding(UTF-8) :std ); # Make UTF-8 default encoding

my $file_path = "/path/to/file";
open(my $file_handle, '>', $file_path) or die "Could not open file! $!";

print $file_handle "Writing to a file";

close $file_handle 
      or warn "Close failed!";

```

**Reading chunks**

Opening and reading big files can take some time and resources. If only a small part of the content is required, it might be a good idea to read the content in chunks using the `read` function which has the signature

`read(FILEHANDLE, SCALAR, LENGTH, OFFSET)`

`FILEHANDLE` must be an opened file handle, `SCALAR` will hold the read data after the operation. `LENGTH` specifies the number of characters to be read starting from the `OFFSET`. The function returns the number of characters read, `0` if the end of file was reached and `undef` in case of an error.

```perl
read($file_handle, $data, 16, 0); 

```

Reads 16 characters from the beginning of the file into `$data`.



#### Parameters


|Mode|Explaination
|---|---|---|---|---|---|---|---|---|---
|`>`|**Write (trunc)**. Will overwrite existing files. Creates a new file if no file was found
|`>>`|**Write (append)**. Will not overwrite files but append new content at the end of it. Will also create a file if used for opening a non existing file.
|`<`|**Read**. Opens the file in read only mode.
|`+<`|**Read / Write**. Will not create or truncate the file.
|`+>`|**Read / Write (trunc)**. Will create and truncate the file.
|`+>>`|**Read / Write (append)**. Will create but not truncate the file.



#### Remarks


`chomp` is often used when reading from a file. By default it trims the newline character, although for its full functionality refer to the [perldocs](http://perldoc.perl.org/functions/chomp.html).

Beware of the difference between characters and bytes: Not all encodings - especially UTF-8 -  use 1-byte-characters. While this is handled pretty much flawlessly by PerlIO, there is one potential pitfall of note:

- `read` uses **characters** for its **length** and **offset** parameters
- `seek` and `tell` **always** use **bytes** for positioning

So don't use arithmetics based on these mixed values. Instead use e.g. `Encode::encode('utf8',$value_by_read)` to get the octets(bytes) from a `read`result, whose count you can then use with `tell` and `seek`.

