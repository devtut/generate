---
metaTitle Files & Folders I/O
description File modes, Reading a file line-by-line, Iterate files (recursively), Getting the full contents of a file, Writing to a file, Check whether a file or path exists, Random File Access Using mmap, Copying contents of one file to a different file, Copy a directory tree, Read a file between a range of lines, Replacing text in a file, Checking if a file is empty
---

# Files & Folders I/O


When it comes to storing, reading, or communicating data, working with the files of an operating system is both necessary and easy with Python. Unlike other languages where file input and output requires complex reading and writing objects, Python simplifies the process only needing commands to open, read/write and close the file. This topic explains how Python can interface with files on the operating system.



## File modes


There are different modes you can open a file with, specified by the `mode` parameter. These include:

<li>
<p>`'r'` - reading mode. The default. It allows you only to read the
file, not to modify it. When using this mode the file must exist.</p>
</li>
<li>
`'w'` - writing mode. It will create a new file if it does not exist, otherwise will erase the file and allow you to write to it.
</li>
<li>
`'a'` - append mode. It will write data to the end of the file. It does not erase the file, and the file must exist for this mode.
</li>
<li>
`'rb'` - reading mode in binary. This is similar to `r` except that the reading is forced in binary mode. This is also a default choice.
</li>
<li>
`'r+'` - reading mode plus writing mode at the same time. This allows you to read and write into files at the same time without having to use `r` and `w`.
</li>
<li>
`'rb+'` - reading and writing mode in binary. The same as `r+` except the data is in binary
</li>
<li>
`'wb'` - writing mode in binary. The same as `w` except the data is in binary.
</li>
<li>
`'w+'` - writing and reading mode. The exact same as `r+` but if the file does not exist, a new one is made. Otherwise, the file is overwritten.
</li>
<li>
`'wb+'` - writing and reading mode in binary mode. The same as `w+` but the data is in binary.
</li>
<li>
`'ab'` - appending in binary mode. Similar to `a` except that the data is in binary.
</li>
<li>
`'a+'` - appending and reading mode. Similar to `w+` as it will create a new file if the file does not exist. Otherwise, the file pointer is at the end of the file if it exists.
</li>
<li>
`'ab+'` - appending and reading mode in binary. The same as `a+` except that the data is in binary.
<pre><code> with open(filename, 'r') as f:
     f.read()
 with open(filename, 'w') as f:
     f.write(filedata)
 with open(filename, 'a') as f:
     f.write('\n' + newdata)
</code></pre>
</li>

||r|r+|w|w+|a|a+
|------
|Read|✔|✔|✘|✔|✘|✔
|Write|✘|✔|✔|✔|✔|✔
|Creates file|✘|✘|✔|✔|✔|✔
|Erases file|✘|✘|✔|✔|✘|✘
|Initial position|Start|Start|Start|Start|End|End

Python 3 added a new mode for `exclusive creation` so that you will not accidentally truncate or overwrite and existing file.

- `'x'` - open for exclusive creation, will raise `FileExistsError` if the file already exists
- `'xb'` - open for exclusive creation writing mode in binary. The same as `x` except the data is in binary.
- `'x+'` - reading and writing mode. Similar to `w+` as it will create a new file if the file does not exist. Otherwise, will raise `FileExistsError`.
- `'xb+'` - writing and reading mode. The exact same as `x+` but the data is binary

||x|x+
|------
|Read|✘|✔
|Write|✔|✔
|Creates file|✔|✔
|Erases file|✘|✘
|Initial position|Start|Start

Allow one to write your file open code in a more pythonic manner:

```
try:
    with open("fname", "r") as fout:
        # Work with your open file
except FileExistsError:
    # Your error handling goes here

```

In Python 2 you would have done something like

```
import os.path
if os.path.isfile(fname):
    with open("fname", "w") as fout:
        # Work with your open file
else:
    # Your error handling goes here

```



## Reading a file line-by-line


The simplest way to iterate over a file line-by-line:

```
with open('myfile.txt', 'r') as fp:
    for line in fp:
        print(line)

```

`readline()` allows for more granular control over line-by-line iteration. The example below is equivalent to the one above:

```
with open('myfile.txt', 'r') as fp:
    while True:
        cur_line = fp.readline()
        # If the result is an empty string
        if cur_line == '':
            # We have reached the end of the file
            break
        print(cur_line)

```

Using the for loop iterator and readline() together is considered bad practice.

More commonly, the `readlines()` method is used to store an iterable collection of the file's lines:

```
with open("myfile.txt", "r") as fp:
    lines = fp.readlines()
for i in range(len(lines)):
    print("Line " + str(i) + ": " + line)

```

This would print the following:

> 
Line 0: hello
Line 1: world




## Iterate files (recursively)


To iterate all files, including in sub directories, use os.walk:

```
import os
for root, folders, files in os.walk(root_dir):
    for filename in files:
        print root, filename

```

root_dir can be "." to start from current directory, or any other path to start from.

If you also wish to get information about the file, you may use the more efficient method [os.scandir](https://docs.python.org/3/library/os.html#os.scandir) like so:

```
for entry in os.scandir(path):
   if not entry.name.startswith('.') and entry.is_file():
       print(entry.name)

```



## Getting the full contents of a file


The preferred method of file i/o is to use the `with` keyword.  This will ensure the file handle is closed once the reading or writing has been completed.

```
with open('myfile.txt') as in_file:
    content = in_file.read()

print(content)

```

or, to handle closing the file manually, you can forgo `with` and simply call `close` yourself:

```
in_file = open('myfile.txt', 'r')
content = in_file.read()
print(content)
in_file.close()

```

Keep in mind that without using a `with` statement, you might accidentally keep the file open in case an unexpected exception arises like so:

```
in_file = open('myfile.txt', 'r')
raise Exception("oops")
in_file.close()  # This will never be called

```



## Writing to a file


```
with open('myfile.txt', 'w') as f:
    f.write("Line 1")
    f.write("Line 2")
    f.write("Line 3")
    f.write("Line 4")

```

If you open `myfile.txt`, you will see that its contents are:

> 
Line 1Line 2Line 3Line 4


Python doesn't automatically add line breaks, you need to do that manually:

```
with open('myfile.txt', 'w') as f:
    f.write("Line 1\n")
    f.write("Line 2\n")
    f.write("Line 3\n")
    f.write("Line 4\n")

```

> 
<p>Line 1<br />
Line 2<br />
Line 3<br />
Line 4</p>


Do not use `os.linesep` as a line terminator when writing files opened in text mode (the default); use `\n` instead.

If you want to specify an encoding, you simply add the `encoding` parameter to the `open` function:

```
with open('my_file.txt', 'w', encoding='utf-8') as f:
    f.write('utf-8 text')

```

It is also possible to use the print statement to write to a file.  The mechanics are different in Python 2 vs Python 3, but the concept is the same in that you can take the output that would have gone to the screen and send it to a file instead.

```
with open('fred.txt', 'w') as outfile:
    s = "I'm Not Dead Yet!"
    print(s) # writes to stdout
    print(s, file = outfile) # writes to outfile

    #Note: it is possible to specify the file parameter AND write to the screen
    #by making sure file ends up with a None value either directly or via a variable
    myfile = None
    print(s, file = myfile) # writes to stdout
    print(s, file = None)   # writes to stdout

```

In Python 2 you would have done something like

```
outfile = open('fred.txt', 'w')
s = "I'm Not Dead Yet!"
print s   # writes to stdout
print >> outfile, s   # writes to outfile

```

Unlike using the write function, the print function does automatically add line breaks.



## Check whether a file or path exists


Employ the [EAFP](https://docs.python.org/glossary.html#term-eafp) coding style and `try` to open it.

```
import errno

try:
    with open(path) as f:
        # File exists
except IOError as e:
    # Raise the exception if it is not ENOENT (No such file or directory)
    if e.errno != errno.ENOENT:
        raise
    # No such file or directory

```

This will also avoid race-conditions if another process deleted the file between the check and when it is used. This race condition could happen in the following cases:

<li>
Using the `os` module:
<pre><code>import os
os.path.isfile('/path/to/some/file.txt')
</code></pre>
</li>

<li>
Using `pathlib`:
<pre><code>import pathlib
path = pathlib.Path('/path/to/some/file.txt')
if path.is_file():
    ...
</code></pre>
</li>

To check whether a given path exists or not, you can follow the above EAFP procedure, or explicitly check the path:

```
import os
path = "/home/myFiles/directory1"

if os.path.exists(path):
    ## Do stuff

```



## Random File Access Using mmap


Using the [`mmap`](https://docs.python.org/2/library/mmap.html) module allows the user to randomly access locations in a file by mapping the file into memory. This is an alternative to using normal file operations.

```
import mmap

with open('filename.ext', 'r') as fd:
    # 0: map the whole file
    mm = mmap.mmap(fd.fileno(), 0)

    # print characters at indices 5 through 10
    print mm[5:10]

    # print the line starting from mm's current position
    print mm.readline()

    # write a character to the 5th index
    mm[5] = 'a'

    # return mm's position to the beginning of the file
    mm.seek(0)

    # close the mmap object
    mm.close()

```



## Copying contents of one file to a different file


```
with open(input_file, 'r') as in_file, open(output_file, 'w') as out_file:
    for line in in_file:
        out_file.write(line)

```

- Using the `shutil` module:

```
import shutil
shutil.copyfile(src, dst)

```



## Copy a directory tree


```
import shutil
source='//192.168.1.2/Daily Reports'
destination='D:\\Reports\\Today'
shutil.copytree(source, destination)  

```

The destination directory **must not exist** already.



## Read a file between a range of lines


So let's suppose you want to iterate only between some specific lines of a file

You can make use of `itertools` for that

This will read through the lines 13 to 20 as in python indexing starts from 0. So line number 1 is indexed as 0

As can also read some extra lines by making use of the `next()` keyword here.

And when you are using the file object as an iterable, please don't use the `readline()` statement here as the two techniques of traversing a file are not to be mixed together



## Replacing text in a file


```
import fileinput

replacements = {'Search1': 'Replace1',
                'Search2': 'Replace2'}

for line in fileinput.input('filename.txt', inplace=True):
    for search_for in replacements:
        replace_with = replacements[search_for]
        line = line.replace(search_for, replace_with)
    print(line, end='')

```



## Checking if a file is empty


```
>>> import os
>>> os.stat(path_to_file).st_size == 0

```

or

```
>>> import os    
>>> os.path.getsize(path_to_file) > 0

```

However, both will throw an exception if the file does not exist. To avoid having to catch such an error, do this:

```
import os
def is_empty_file(fpath):  
    return os.path.isfile(fpath) and os.path.getsize(fpath) > 0

```

which will return a `bool` value.



#### Syntax


- file_object = open(filename [, access_mode][, buffering])



#### Parameters


|Parameter|Details
|------
|filename|the path to your file or, if the file is in the working directory, the filename of your file
|access_mode|a string value that determines how the file is opened
|buffering|an integer value used for optional line buffering



#### Remarks


### Avoiding the cross-platform Encoding Hell

When using Python's built-in `open()`, it is best-practice to always pass the `encoding` argument, if you intend your code to be run cross-platform.
The Reason for this, is that a system's default encoding differs from platform to platform.

While `linux` systems do indeed use `utf-8` as default, this is ****not**** necessarily true for MAC and Windows.

To check a system's default encoding, try this:

```
import sys
sys.getdefaultencoding()

```

from any python interpreter.

Hence, it is wise to always sepcify an encoding, to make sure the strings you're working with are encoded as what you think they are, ensuring cross-platform compatiblity.

```
with open('somefile.txt', 'r', encoding='UTF-8') as f:
    for line in f:
        print(line)

```

