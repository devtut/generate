# os.path


This module implements some useful functions on pathnames. The path parameters can be passed as either strings, or bytes. Applications are encouraged to represent file names as (Unicode) character strings.



## Join Paths


To join two or more path components together, firstly import os module of python and then use following:

```
import os
os.path.join('a', 'b', 'c')

```

The advantage of using os.path is that it allows code to remain compatible over all operating systems, as this uses the separator appropriate for the platform it's running on.

For example, the result of this command on Windows will be:

```
>>> os.path.join('a', 'b', 'c')
'a\b\c'

```

In an Unix OS:

```
>>> os.path.join('a', 'b', 'c')
'a/b/c'

```



## Path Component Manipulation


To split one component off of the path:

```
>>> p = os.path.join(os.getcwd(), 'foo.txt')
>>> p
'/Users/csaftoiu/tmp/foo.txt'
>>> os.path.dirname(p)
'/Users/csaftoiu/tmp'
>>> os.path.basename(p)
'foo.txt'
>>> os.path.split(os.getcwd())
('/Users/csaftoiu/tmp', 'foo.txt')
>>> os.path.splitext(os.path.basename(p))
('foo', '.txt')

```



## Absolute Path from Relative Path


Use `os.path.abspath`:

```
>>> os.getcwd()
'/Users/csaftoiu/tmp'
>>> os.path.abspath('foo')
'/Users/csaftoiu/tmp/foo'
>>> os.path.abspath('../foo')
'/Users/csaftoiu/foo'
>>> os.path.abspath('/foo')
'/foo'

```



## Get the parent directory


```
os.path.abspath(os.path.join(PATH_TO_GET_THE_PARENT, os.pardir))

```



## If the given path exists.


to check if the given path exists

```
path = '/home/john/temp'
os.path.exists(path)
#this returns false if path doesn't exist or if the path is a broken symbolic link

```



## check if the given path is a directory, file, symbolic link, mount point etc.


to check if the given path is a directory

```
dirname = '/home/john/python'
os.path.isdir(dirname)

```

to check if the given path is a file

```
filename = dirname + 'main.py'
os.path.isfile(filename)

```

to check if the given path is [symbolic link](https://en.wikipedia.org/wiki/Symbolic_link)

```
symlink = dirname + 'some_sym_link'
os.path.islink(symlink)

```

to check if the given path is a [mount point](http://www.linuxtopia.org/online_books/introduction_to_linux/linux_Mount_points.html)

```
mount_path = '/home'
os.path.ismount(mount_path)

```



#### Syntax


- os.path.join(a, *p)
- os.path.basename(p)
- os.path.dirname(p)
- os.path.split(p)
- os.path.splitext(p)

