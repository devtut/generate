# os.path



## Join Paths


To join two or more path components together, firstly import os module of python and then use following:

```
import os
os.path.join('a', 'b', 'c')

```

The advantage of using os.path is that it allows code to remain compatible over all operating systems, as this uses the separator appropriate for the platform it's running on.

For example, the result of this command on Windows will be:

```
&gt;&gt;&gt; os.path.join('a', 'b', 'c')
'a\b\c'

```

In an Unix OS:

```
&gt;&gt;&gt; os.path.join('a', 'b', 'c')
'a/b/c'

```



## Path Component Manipulation


To split one component off of the path:

```
&gt;&gt;&gt; p = os.path.join(os.getcwd(), 'foo.txt')
&gt;&gt;&gt; p
'/Users/csaftoiu/tmp/foo.txt'
&gt;&gt;&gt; os.path.dirname(p)
'/Users/csaftoiu/tmp'
&gt;&gt;&gt; os.path.basename(p)
'foo.txt'
&gt;&gt;&gt; os.path.split(os.getcwd())
('/Users/csaftoiu/tmp', 'foo.txt')
&gt;&gt;&gt; os.path.splitext(os.path.basename(p))
('foo', '.txt')

```



## Absolute Path from Relative Path


Use `os.path.abspath`:

```
&gt;&gt;&gt; os.getcwd()
'/Users/csaftoiu/tmp'
&gt;&gt;&gt; os.path.abspath('foo')
'/Users/csaftoiu/tmp/foo'
&gt;&gt;&gt; os.path.abspath('../foo')
'/Users/csaftoiu/foo'
&gt;&gt;&gt; os.path.abspath('/foo')
'/foo'

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

to check if the given path is [symbolic link](http://web.archive.org/web/20170305005347/https://en.wikipedia.org/wiki/Symbolic_link)

```
symlink = dirname + 'some_sym_link'
os.path.islink(symlink)

```

to check if the given path is a [mount point](http://web.archive.org/web/20170305005347/http://www.linuxtopia.org/online_books/introduction_to_linux/linux_Mount_points.html)

```
mount_path = '/home'
os.path.ismount(mount_path)

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



#### Syntax


- os.path.join(a, *p)
- os.path.basename(p)
- os.path.dirname(p)
- os.path.split(p)
- os.path.splitext(p)

