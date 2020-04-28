---
metaTitle: "Getting started with Python Language"
description: "Getting Started, Creating variables and assigning values, Block Indentation, Datatypes, Collection Types, IDLE - Python GUI, User Input, Built in Modules and Functions, Creating a module, Installation of Python 2.7.x and 3.x, String function - str() and repr(), Installing external modules using pip, Help Utility"
---

# Getting started with Python Language



## Getting Started


Python is a widely used high-level programming language for general-purpose programming, created by Guido van Rossum and first released in 1991. Python features a dynamic type system and automatic memory management and supports multiple programming paradigms, including object-oriented, imperative, functional programming, and procedural styles. It has a large and comprehensive standard library.

Two major versions of Python are currently in active use:

- Python 3.x is the current version and is under active development.
- Python 2.x is the legacy version and will receive only security updates until 2020. No new features will be implemented. Note that many projects still use Python 2, although migrating to Python 3 is getting easier.

You can download and install either version of Python [here](https://www.python.org/downloads/). See [Python 3 vs. Python 2](http://stackoverflow.com/documentation/python/809/incompatibilities-moving-from-python-2-to-python-3#t=201703280213123640438) for a comparison between them. In addition, some third-parties offer re-packaged versions of Python that add commonly used libraries and other features to ease setup for common use cases, such as math, data analysis or scientific use. See [the list at the official site](https://www.python.org/download/alternatives/).

### Verify if Python is installed

To confirm that Python was installed correctly, you can verify that by running the following command in your favorite terminal (If you are using Windows OS, you need to add path of python to the environment variable before using it in command prompt):

```py
$ python --version

```

If you have **Python 3** installed, and it is your default version (see [**Troubleshooting**](http://stackoverflow.com/documentation/python/193/introduction-to-python/2653/idle-python-gui) for more details) you should see something like this:

```py
$ python --version
Python 3.6.0

```

If you have **Python 2** installed, and it is your default version (see [**Troubleshooting**](http://stackoverflow.com/documentation/python/193/introduction-to-python/2653/idle-python-gui) for more details) you should see something like this:

```py
$ python --version
Python 2.7.13

```

If you have installed Python 3, but `$ python --version` outputs a Python 2 version, you also have Python 2 installed. This is often the case on MacOS, and many Linux distributions. Use `$ python3` instead to explicitly use the Python 3 interpreter.

### Hello, World in Python using IDLE

[IDLE](https://docs.python.org/2/library/idle.html) is a simple editor for Python, that comes bundled with Python.

**How to create Hello, World program in IDLE**

<li>Open IDLE on your system of choice.
<ul>
- In older versions of Windows, it can be found at `All Programs` under the Windows menu.
- In Windows 8+, search for `IDLE` or find it in the apps that are present in your system.
- On Unix-based (including Mac) systems you can open it from the shell by typing `$ idle python_file.py`.

In the shell, there is a prompt of three right angle brackets:

```py
>>>

```

Now write the following code in the prompt:

```py
>>> print("Hello, World")

```

Hit <kbd>Enter</kbd>.

```py
>>> print("Hello, World")
Hello, World

```

### Hello World Python file

Create a new file `hello.py` that contains the following line:

```py
print('Hello, World')

```

You can use the Python 3 `print` function in Python 2 with the following `import` statement:

```py
from __future__ import print_function

```

Python 2 has a number of functionalities that can be optionally imported from Python 3 using the `__future__` module, as [discussed here](http://stackoverflow.com/documentation/python/809/incompatibility-between-python-3-and-python-2/6894/use-future-imports#t=201612062212456247425).

If using Python 2, you may also type the line below. Note that this is not valid in Python 3 and thus not recommended because it reduces cross-version code compatibility.

```py
print 'Hello, World'

```

In your terminal, navigate to the directory containing the file `hello.py`.

Type `python hello.py`, then hit the <kbd>Enter</kbd> key.

```py
$ python hello.py
Hello, World

```

You should see `Hello, World` printed to the console.

You can also substitute `hello.py` with the path to your file. For example, if you have the file in your home directory and your user is "user" on Linux, you can type `python /home/user/hello.py`.

### Launch an interactive Python shell

By executing (running) the `python` command in your terminal, you are presented with an interactive Python shell. This is also known as the [Python Interpreter](https://docs.python.org/3.6/tutorial/interpreter.html) or  a REPL (for 'Read Evaluate Print Loop').

```py
$ python
Python 2.7.12 (default, Jun 28 2016, 08:46:01) 
[GCC 6.1.1 20160602] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> print 'Hello, World'
Hello, World
>>>

```

If you want to run Python 3 from your terminal, execute the command `python3`.

```py
$ python3
Python 3.6.0 (default, Jan 13 2017, 00:00:00) 
[GCC 6.1.1 20160602] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> print('Hello, World')
Hello, World
>>>

```

Alternatively, start the interactive prompt and load file with `python -i <file.py>`.

In command line, run:

```py
$ python -i hello.py
"Hello World"
>>>

```

There are multiple ways to close the Python shell:

```py
>>> exit()

```

or

```py
>>> quit()

```

Alternatively, <kbd>CTRL + D</kbd> will close the shell and put you back on your terminal's command line.

If you want to cancel a command you're in the middle of typing and get back to a clean command prompt, while staying inside the Interpreter shell, use <kbd>CTRL + C</kbd>.

[Try an interactive Python shell online](https://www.python.org/shell/).

### Other Online Shells

Various websites provide online access to Python shells.

Online shells may be useful for the following purposes:

<li>Run a small code snippet from a machine which lacks python
installation(smartphones, tablets etc).</li>
- Learn or teach basic Python.
- Solve online judge problems.

Examples:

> 
<p>Disclaimer: documentation author(s) are not affiliated with any
resources listed below.</p>


- [https://www.python.org/shell/](https://www.python.org/shell/) - The online Python shell hosted by the official Python website.
<li>[https://ideone.com/](https://ideone.com/) - Widely used on the Net to illustrate code
snippet behavior.</li>
<li>[https://repl.it/languages/python3](https://repl.it/languages/python3) - Powerful and simple online
compiler, IDE and interpreter.  Code, compile, and run code in Python.</li>
<li>[https://www.tutorialspoint.com/execute_python_online.php](https://www.tutorialspoint.com/execute_python_online.php) -
Full-featured UNIX shell, and a user-friendly project explorer.</li>
<li>[http://rextester.com/l/python3_online_compiler](http://rextester.com/l/python3_online_compiler) - Simple and easy to use IDE which
shows execution time</li>

### Run commands as a string

Python can be passed arbitrary code as a string in the shell:

```py
$ python -c 'print("Hello, World")'
Hello, World

```

This can be useful when concatenating the results of scripts together in the shell.

### Shells and Beyond

**Package Management** - The PyPA recommended tool for installing Python packages is [PIP](https://pip.pypa.io/en/stable/). To install, on your command line execute `pip install <the package name>`. For instance, `pip install numpy`. (Note: On windows you must add pip to your PATH environment variables. To avoid this, use `python -m pip install <the package name>`)

**Shells** - So far, we have discussed different ways to run code using Python's native interactive shell.  Shells use Python's interpretive power for experimenting with code real-time.  Alternative shells include [IDLE](https://docs.python.org/3/library/idle.html) - a pre-bundled GUI, [IPython](https://ipython.org/install.html) - known for extending the interactive experience, etc.

**Programs** - For long-term storage you can save content to .py files and edit/execute them as scripts or programs with external tools e.g. shell, [IDEs](https://wiki.python.org/moin/IntegratedDevelopmentEnvironments) (such as [PyCharm](https://www.jetbrains.com/pycharm/download/)), [Jupyter notebooks](https://github.com/jupyter/notebook), etc.  Intermediate users may use these tools; however, the methods discussed here are sufficient for getting started.

[Python tutor](http://www.pythontutor.com/visualize.html#mode=edit) allows you to step through Python code so you can visualize how the program will flow, and helps you to understand where your program went wrong.

[PEP8](https://www.python.org/dev/peps/pep-0008/) defines guidelines for formatting Python code. Formatting code well is important so you can quickly read what the code does.



## Creating variables and assigning values


To create a variable in Python, all you need to do is specify the variable name, and then assign a value to it.

```py
<variable name> = <value>

```

Python uses `=` to assign values to variables. There's no need to declare a variable in advance (or to assign a data type to it), assigning a value to a variable itself declares and initializes the variable with that value. There's no way to declare a variable without assigning it an initial value.

```py
# Integer
a = 2
print(a)
# Output: 2

# Integer    
b = 9223372036854775807
print(b)
# Output: 9223372036854775807

# Floating point
pi = 3.14
print(pi)
# Output: 3.14

# String
c = 'A'
print(c)
# Output: A

# String    
name = 'John Doe'
print(name)
# Output: John Doe

# Boolean    
q = True
print(q)
# Output: True

# Empty value or null data type
x = None
print(x)
# Output: None

```

Variable assignment works from left to right. So the following will give you an syntax error.

```py
0 = x
=> Output: SyntaxError: can't assign to literal

```

You can not use python's keywords as a valid variable name. You can see the list of keyword by:

```py
import keyword
print(keyword.kwlist)

```

Rules for variable naming:

1. Variables names must start with a letter or an underscore.

```

x  = True   # valid
 _y = True   # valid

 9x = False  # starts with numeral 
 => SyntaxError: invalid syntax   

 $y = False #  starts with symbol 
 => SyntaxError: invalid syntax

```


1. The remainder of your variable name may consist of letters, numbers and underscores.

```py
has_0_in_it = "Still Valid" 

```


1. Names are case sensitive.

```py
x = 9  
y = X*5   
=>NameError: name 'X' is not defined

```

Even though there's no need to specify a data type when declaring a variable in Python, while allocating the necessary area in memory for the variable, the Python interpreter automatically picks the most suitable [built-in type](http://stackoverflow.com/documentation/python/193/introduction-to-python/2926/datatypes#t=201610091728094101649) for it:

```py
a = 2
print(type(a))
# Output: <type 'int'>

b = 9223372036854775807
print(type(b))
# Output: <type 'int'>

pi = 3.14
print(type(pi))
# Output: <type 'float'>

c = 'A'
print(type(c))
# Output: <type 'str'>

name = 'John Doe'
print(type(name))
# Output: <type 'str'>

q = True
print(type(q))
# Output: <type 'bool'>

x = None
print(type(x))
# Output: <type 'NoneType'>

```

Now you know the basics of assignment, let's get this subtlety about assignment in python out of the way.

When you use `=` to do an assignment operation, what's on the left of `=` is a ****name**** for the ****object**** on the right. Finally, what `=` does is assign the ****reference**** of the object on the right to the ****name**** on the left.

That is:

```py
a_name = an_object  # "a_name" is now a name for the reference to the object "an_object"

```

So, from many assignment examples above, if we pick `pi = 3.14`, then `pi` is **a** name (not **the** name, since an object can have multiple names) for the object `3.14`. If you don't understand something below, come back to this point and read this again! Also, you can take a look at [this](http://effbot.org/zone/python-objects.htm) for a better understanding.

You can assign multiple values to multiple variables in one line. Note that there must be the same number of arguments on the right and left sides of the `=` operator:

```py
a, b, c = 1, 2, 3
print(a, b, c)
# Output: 1 2 3

a, b, c = 1, 2
=> Traceback (most recent call last):
=>   File "name.py", line N, in <module>
=>     a, b, c = 1, 2
=> ValueError: need more than 2 values to unpack

a, b = 1, 2, 3
=> Traceback (most recent call last):
=>   File "name.py", line N, in <module>
=>     a, b = 1, 2, 3
=> ValueError: too many values to unpack

```

The error in last example can be obviated by assigning remaining values to equal number of arbitrary variables. This dummy variable can have any name, but it is conventional to use the underscore (`_`) for assigning unwanted values:

```py
a, b, _ = 1, 2, 3
print(a, b)
# Output: 1, 2

```

Note that the number of _ and number of remaining values must be equal. Otherwise 'too many values to unpack error' is thrown as above:

```py
a, b, _ = 1,2,3,4
=>Traceback (most recent call last):
=>File "name.py", line N, in <module>
=>a, b, _ = 1,2,3,4
=>ValueError: too many values to unpack (expected 3)

```

You can also assign a single value to several variables simultaneously.

```py
a = b = c = 1
print(a, b, c)
# Output: 1 1 1

```

When using such cascading assignment, it is important to note that all three variables `a`, `b` and `c` refer to the **same object** in memory, an `int` object with the value of 1. In other words, `a`, `b` and `c` are three different names given to the same int object. Assigning a different object to one of them afterwards doesn't change the  others, just as expected:

```py
a = b = c = 1    # all three names a, b and c refer to same int object with value 1
print(a, b, c)
# Output: 1 1 1
b = 2            # b now refers to another int object, one with a value of 2
print(a, b, c)
# Output: 1 2 1  # so output is as expected.

```

The above is also true for mutable types (like `list`, `dict`, etc.) just as it is true for immutable types (like `int`, `string`, `tuple`, etc.):

```py
x = y = [7, 8, 9]   # x and y refer to the same list object just created, [7, 8, 9]
x = [13, 8, 9]      # x now refers to a different list object just created, [13, 8, 9]
print(y)            # y still refers to the list it was first assigned
# Output: [7, 8, 9]

```

So far so good. Things are a bit different when it comes to **modifying** the object (in contrast to **assigning** the name to a different object, which we did above) when the cascading assignment is used for mutable types. Take a look below, and you will see it first hand:

```py
x = y = [7, 8, 9]     # x and y are two different names for the same list object just created, [7, 8, 9]
x[0] = 13             # we are updating the value of the list [7, 8, 9] through one of its names, x in this case
print(y)              # printing the value of the list using its other name
# Output: [13, 8, 9]  # hence, naturally the change is reflected

```

Nested lists are also valid in python. This means that a list can contain another list as an element.

```py
x = [1, 2, [3, 4, 5], 6, 7] # this is nested list
print x[2]
# Output: [3, 4, 5]
print x[2][1]
# Output: 4

```

Lastly, variables in Python do not have to stay the same type as which they were first defined -- you can simply use `=` to assign a new value to a variable, even if that value is of a different type.

```py
a = 2 
print(a)
# Output: 2

a = "New value"
print(a)
# Output: New value

```

If this bothers you, think about the fact that what's on the left of `=` is just a name for an object. First you call the `int` object with value 2 `a`, then you change your mind and decide to give the name `a` to a `string` object, having value 'New value'. Simple, right?



## Block Indentation


Python uses indentation to define control and loop constructs. This contributes to Python's readability, however, it requires the programmer to pay close attention to the use of whitespace. Thus, editor miscalibration could result in code that behaves in unexpected ways.

Python uses the colon symbol (`:`) and indentation for showing where blocks of code begin and end (If you come from another language, do not confuse this with somehow being related to the [ternary operator](https://en.wikipedia.org/wiki/%3F:)). That is, blocks in Python, such as functions, loops, `if` clauses and other constructs, have no ending identifiers. All blocks start with a colon and then contain the indented lines below it.

For example:

```py
def my_function():    # This is a function definition. Note the colon (:)
    a = 2             # This line belongs to the function because it's indented
    return a          # This line also belongs to the same function
print(my_function())  # This line is OUTSIDE the function block

```

or

```py
if a > b:             # If block starts here
    print(a)          # This is part of the if block
else:                 # else must be at the same level as if
    print(b)          # This line is part of the else block

```

Blocks that contain exactly one single-line statement may be put on the same line, though this form is generally not considered good style:

```py
if a > b: print(a)
else: print(b)  

```

Attempting to do this with more than a single statement will **not** work:

```py
if x > y: y = x
    print(y) # IndentationError: unexpected indent

if x > y: while y != z: y -= 1  # SyntaxError: invalid syntax

```

An empty block causes an `IndentationError`. Use `pass` (a command that does nothing) when you have a block with no content:

```py
def will_be_implemented_later():
    pass

```

### Spaces vs. Tabs

In short: **always** use 4 spaces for indentation.

Using tabs exclusively is possible but [PEP 8](https://www.python.org/dev/peps/pep-0008/#tabs-or-spaces), the style guide for Python code, states that spaces are preferred.

Python 3 disallows mixing the use of tabs and spaces for indentation. In such case a compile-time error is generated: `Inconsistent use of tabs and spaces in indentation` and the program will not run.

Python 2 allows mixing tabs and spaces in indentation; this is strongly discouraged. The tab character completes the previous indentation to be a [multiple of **8** spaces](https://docs.python.org/2/reference/lexical_analysis.html#indentation). Since it is common that editors are configured to show tabs as multiple of **4** spaces, this can cause subtle bugs.

Citing [PEP 8](https://www.python.org/dev/peps/pep-0008/#tabs-or-spaces):

> 
When invoking the Python 2 command line interpreter with the `-t` option, it issues warnings about code that illegally mixes tabs and spaces. When using `-tt` these warnings become errors. These options are highly recommended!


Many editors have "tabs to spaces" configuration. When configuring the editor, one should differentiate between the tab **character** ('\t') and the <kbd>Tab</kbd> key.

- The tab **character** should be configured to show 8 spaces, to match the language semantics - at least in cases when (accidental) mixed indentation is possible. Editors can also automatically convert the tab character to spaces.
- However, it might be helpful to configure the editor so that pressing the <kbd>Tab</kbd> key will insert 4 spaces, instead of inserting a tab character.

Python source code written with a mix of tabs and spaces, or with non-standard number of indentation spaces can be made pep8-conformant using [autopep8](http://stackoverflow.com/questions/2625294/how-do-i-autoformat-some-python-code-to-be-correctly-formatted). (A less powerful alternative comes with most Python installations: [reindent.py](https://pypi.python.org/pypi/Reindent/0.1.0))



## Datatypes


### Built-in Types

### Booleans

`bool`: A boolean value of either `True` or `False`. Logical operations like `and`, `or`, `not` can be performed on booleans.

```py
x or y    # if x is False then y otherwise x 
x and y   # if x is False then x otherwise y
not x     # if x is True then False, otherwise True

```

In Python 2.x and in Python 3.x, a boolean is also an `int`. The `bool` type is a subclass of the `int` type and `True` and `False` are its only instances:

```py
issubclass(bool, int) # True

isinstance(True, bool) # True
isinstance(False, bool) # True

```

If boolean values are used in arithmetic operations, their integer values (`1` and `0` for `True` and `False`) will be used to return an integer result:

```py
True + False == 1 # 1 + 0 == 1
True * True  == 1 # 1 * 1 == 1

```

### Numbers

<li>
`int`: Integer number
<pre><code>a = 2
b = 100
c = 123456789
d = 38563846326424324
</code></pre>
Integers in Python are of arbitrary sizes.
Note: in older versions of Python, a `long` type was available and this was distinct from `int`. The two have been unified.
</li>
<li>
`float`: Floating point number; precision depends on the implementation and system architecture, for CPython the `float` datatype corresponds to a C double.
<pre><code>a = 2.0
b = 100.e0
c = 123456789.e1
</code></pre>
</li>
<li>
`complex`: Complex numbers
<pre><code>a = 2 + 1j
b = 100 + 10j
</code></pre>
</li>

The `<`, `<=`, `>` and `>=` operators will raise a `TypeError` exception when any operand is a complex number.

### Strings

- `str`: a **unicode string**. The type of `'hello'`
- `bytes`: a **byte string**. The type of `b'hello'`

- `str`: a **byte string**. The type of `'hello'`
- `bytes`: synonym for `str`
- `unicode`: a **unicode string**. The type of `u'hello'`

### Sequences and collections

Python differentiates between ordered sequences and unordered collections (such as `set` and `dict`).

<li>
strings (`str`, `bytes`, `unicode`) are sequences
</li>
<li>
`reversed`: A reversed order of `str` with `reversed` function
<pre><code>a = reversed('hello')
</code></pre>
</li>
<li>
`tuple`: An ordered collection of `n` values of any type (`n >= 0`).
<pre><code>a = (1, 2, 3)
b = ('a', 1, 'python', (1, 2))
b[2] = 'something else' # returns a TypeError
</code></pre>
Supports indexing; immutable; hashable if all its members are hashable
</li>
<li>
`list`: An ordered collection of `n` values (`n >= 0`)
<pre><code>a = [1, 2, 3]
b = ['a', 1, 'python', (1, 2), [1, 2]]
b[2] = 'something else' # allowed
</code></pre>
Not hashable; mutable.
</li>
<li>
`set`: An unordered collection of unique values. Items must be [hashable](https://docs.python.org/3.5/glossary.html).
<pre><code>a = {1, 2, 'a'}
</code></pre>
</li>
<li>
`dict`: An unordered collection of unique key-value pairs; keys must be [hashable](https://docs.python.org/3.5/glossary.html).
<pre><code>a = {1: 'one',
     2: 'two'}

b = {'a': [1, 2, 3],
     'b': 'a string'}
</code></pre>
</li>

> 
An object is hashable if it has a hash value which never changes during its lifetime (it needs a `__hash__()` method), and can be compared to other objects (it needs an `__eq__()` method). Hashable objects which compare equality must have the same hash value.


### Built-in constants

In conjunction with the built-in datatypes there are a small number of built-in constants in the built-in namespace:

- `True`: The true value of the built-in type `bool`
- `False`: The false value of the built-in type `bool`
- `None`: A singleton object used to signal that a value is absent.
- `Ellipsis` or `...`: used in core Python3+ anywhere and limited usage in Python2.7+ as part of array notation. `numpy` and related packages use this as a 'include everything' reference in arrays.
- `NotImplemented`: a singleton used to indicate to Python that a special method doesn't support the specific arguments, and Python will try alternatives if available.

```py
a = None # No value will be assigned. Any valid datatype can be assigned later

```

`None` doesn't have any natural ordering. Using ordering comparison operators (`<`, `<=`, `>=`, `>`) isn't supported anymore and will raise a `TypeError`.

`None` is always less than any number (`None < -32` evaluates to `True`).

### Testing the type of variables

In python, we can check the datatype of an object using the built-in function `type`.

```py
a = '123'
print(type(a))
# Out: <class 'str'>
b = 123
print(type(b))
# Out: <class 'int'>

```

In conditional statements it is possible to test the datatype with `isinstance`. However, it is usually not encouraged to rely on the type of the variable.

```py
i = 7
if isinstance(i, int):
    i += 1
elif isinstance(i, str):
    i = int(i)
    i += 1

```

For information on the differences between `type()` and `isinstance()` read: [Differences between isinstance and type in Python](https://stackoverflow.com/questions/1549801/differences-between-isinstance-and-type-in-python)

To test if something is of `NoneType`:

```py
x = None
if x is None:
    print('Not a surprise, I just defined x as None.')

```

### Converting between datatypes

You can perform explicit datatype conversion.

For example, '123' is of `str` type and it can be converted to integer using `int` function.

```py
a = '123'
b = int(a)

```

Converting from a float string such as '123.456' can be done using `float` function.

```py
a = '123.456'
b = float(a)
c = int(a)    # ValueError: invalid literal for int() with base 10: '123.456'
d = int(b)    # 123

```

You can also convert sequence or collection types

```py
a = 'hello'
list(a)  # ['h', 'e', 'l', 'l', 'o']
set(a)   # {'o', 'e', 'l', 'h'}
tuple(a) # ('h', 'e', 'l', 'l', 'o')

```

### Explicit string type at definition of literals

With one letter labels just in front of the quotes you can tell what type of string you want to define.

- `b'foo bar'`: results `bytes` in Python 3, `str` in Python 2
- `u'foo bar'`: results `str` in Python 3, `unicode` in Python 2
- `'foo bar'`: results `str`
- `r'foo bar'`: results so called raw string, where escaping special characters is not necessary, everything is taken verbatim as you typed

```py
normal  = 'foo\nbar'   # foo
                       # bar
escaped = 'foo\\nbar'  # foo\nbar   
raw     = r'foo\nbar'  # foo\nbar

```

### Mutable and Immutable Data Types

An object is called **mutable** if it can be changed. For example, when you pass a list to some function, the list can be changed:

```py
def f(m):
    m.append(3)  # adds a number to the list. This is a mutation.

x = [1, 2]
f(x)
x == [1, 2]  # False now, since an item was added to the list

```

An object is called **immutable** if it cannot be changed in any way. For example, integers are immutable, since there's no way to change them:

```py
def bar():
    x = (1, 2)
    g(x)
    x == (1, 2)  # Will always be True, since no function can change the object (1, 2)

```

Note that **variables** themselves are mutable, so we can reassign the **variable** `x`, but this does not change the object that `x` had previously pointed to. It only made `x` point to a new object.

Data types whose instances are mutable are called **mutable data types**, and similarly for immutable objects and datatypes.

Examples of immutable Data Types:

- `int`, `long`, `float`, `complex`
- `str`
- `bytes`
- `tuple`
- `frozenset`

Examples of mutable Data Types:

- `bytearray`
- `list`
- `set`
- `dict`



## Collection Types


There are a number of collection types in Python.  While types such as `int` and `str` hold a single value, collection types hold multiple values.

**Lists**

The `list` type is probably the most commonly used collection type in Python.  Despite its name, a list is more like an array in other languages, mostly JavaScript.  In Python, a list is merely an ordered collection of valid Python values. A list can be created by enclosing values, separated by commas, in square brackets:

```py
int_list = [1, 2, 3]
string_list = ['abc', 'defghi']

```

A list can be empty:

```py
empty_list = []

```

The elements of a list are not restricted to a single data type, which makes sense given that Python is a dynamic language:

```py
mixed_list = [1, 'abc', True, 2.34, None]

```

A list can contain another list as its element:

```py
nested_list = [['a', 'b', 'c'], [1, 2, 3]]

```

The elements of a list can be accessed via an **index**, or numeric representation of their position.  Lists in Python are **zero-indexed** meaning that the first element in the list is at index 0, the second element is at index 1 and so on:

```py
names = ['Alice', 'Bob', 'Craig', 'Diana', 'Eric']
print(names[0]) # Alice
print(names[2]) # Craig

```

Indices can also be negative which means counting from the end of the list (`-1` being the index of the last element). So, using the list from the above example:

```py
print(names[-1]) # Eric
print(names[-4]) # Bob

```

Lists are mutable, so you can change the values in a list:

```py
names[0] = 'Ann'
print(names)
# Outputs ['Ann', 'Bob', 'Craig', 'Diana', 'Eric']

```

Besides, it is possible to add and/or remove elements from a list:

Append object to end of list with `L.append(object)`, returns `None`.

```py
names = ['Alice', 'Bob', 'Craig', 'Diana', 'Eric']
names.append("Sia")
print(names) 
# Outputs ['Alice', 'Bob', 'Craig', 'Diana', 'Eric', 'Sia']

```

Add a new element to list at a specific index. `L.insert(index, object)`

```py
names.insert(1, "Nikki")
print(names)
# Outputs ['Alice', 'Nikki', 'Bob', 'Craig', 'Diana', 'Eric', 'Sia']

```

Remove the first occurrence of a value with `L.remove(value)`, returns `None`

```py
names.remove("Bob")
print(names) # Outputs ['Alice', 'Nikki', 'Craig', 'Diana', 'Eric', 'Sia']

```

Get the index in the list of the first item whose value is x. It will show an error if there is no such item.

```py
name.index("Alice")
0

```

Count length of list

```py
len(names)
6

```

count occurrence of any item in list

```py
a = [1, 1, 1, 2, 3, 4]
a.count(1)
3

```

Reverse the list

```py
a.reverse()
[4, 3, 2, 1, 1, 1]
# or
a[::-1]
[4, 3, 2, 1, 1, 1]

```

Remove and return item at index (defaults to the last item) with `L.pop([index])`, returns the item

```py
names.pop() # Outputs 'Sia'

```

You can iterate over the list elements like below:

```py
for element in my_list:
    print (element)

```

**Tuples**

A `tuple` is similar to a list except that it is fixed-length and immutable. So the values in the tuple cannot be changed nor the values be added to or removed from the tuple. Tuples are commonly used for small collections of values that will not need to change, such as an IP address and port. Tuples are represented with parentheses instead of square brackets:

```py
ip_address = ('10.20.30.40', 8080)

```

The same indexing rules for lists also apply to tuples.  Tuples can also be nested and the values can be any valid Python valid.

A tuple with only one member must be defined (note the comma) this way:

```py
one_member_tuple = ('Only member',)

```

or

```py
one_member_tuple = 'Only member',   # No brackets

```

or just using `tuple` syntax

```py
one_member_tuple = tuple(['Only member'])

```

**Dictionaries**

A `dictionary` in Python is a collection of key-value pairs. The dictionary is surrounded by curly braces. Each pair is separated by a comma and the key and value are separated by a colon. Here is an example:

```py
state_capitals = {
    'Arkansas': 'Little Rock',
    'Colorado': 'Denver',
    'California': 'Sacramento', 
    'Georgia': 'Atlanta'
}

```

To get a value, refer to it by its key:

```py
ca_capital = state_capitals['California']

```

You can also get all of the keys in a dictionary and then iterate over them:

```py
for k in state_capitals.keys():
    print('{} is the capital of {}'.format(state_capitals[k], k))

```

Dictionaries strongly resemble JSON syntax. The native `json` module in the Python standard library can be used to convert between JSON and dictionaries.

**set**

A `set` is a collection of elements with no repeats and without insertion order but sorted order. They are used in situations where it is only important that some things are grouped together, and not what order they were included.  For large groups of data, it is much faster to check whether or not an element is in a `set` than it is to do the same for a `list`.

Defining a `set` is very similar to defining a `dictionary`:

```py
first_names = {'Adam', 'Beth', 'Charlie'}

```

Or you can build a `set` using an existing `list`:

```py
my_list = [1,2,3]
my_set = set(my_list)

```

Check membership of the `set` using `in`:

```py
if name in first_names:
    print(name)

```

You can iterate over a `set` exactly like a list, but remember: the values will be in a arbitrary, implementation-defined order.

**defaultdict**

A `defaultdict` is a dictionary with a default value for keys, so that keys for which no value has been explicitly defined can be accessed without errors. `defaultdict` is especially useful when the values in the dictionary are collections (lists, dicts, etc) in the sense that it does not need to be initialized every time when a new key is used.

A `defaultdict` will never raise a KeyError. Any key that does not exist gets the default value returned.

For example, consider the following dictionary

```py
>>> state_capitals = {
    'Arkansas': 'Little Rock',
    'Colorado': 'Denver',
    'California': 'Sacramento', 
    'Georgia': 'Atlanta'
}

```

If we try to access a non-existent key, python returns us an error as follows

```py
>>> state_capitals['Alabama']
Traceback (most recent call last):

  File "<ipython-input-61-236329695e6f>", line 1, in <module>
    state_capitals['Alabama']

KeyError: 'Alabama'

```

Let us try with a `defaultdict`. It can be found in the collections module.

```py
>>> from collections import defaultdict
>>> state_capitals = defaultdict(lambda: 'Boston')

```

What we did here is to set a default value (**Boston**) in case the give key does not exist. Now populate the dict as before:

```py
>>> state_capitals['Arkansas'] = 'Little Rock'
>>> state_capitals['California'] = 'Sacramento'
>>> state_capitals['Colorado'] = 'Denver'
>>> state_capitals['Georgia'] = 'Atlanta'

```

If we try to access the dict with a non-existent key, python will return us the default value i.e. Boston

```py
>>> state_capitals['Alabama']
'Boston'

```

and returns the created values for existing key just like a normal `dictionary`

```py
>>> state_capitals['Arkansas']
'Little Rock'

```



## IDLE - Python GUI


IDLE is Python’s Integrated Development and Learning Environment and is an alternative to the command line. As the name may imply, IDLE is very useful for developing new code or learning python. On Windows this comes with the Python interpreter, but in other operating systems you may need to install it through your package manager.

The main purposes of IDLE are:

- Multi-window text editor with syntax highlighting, autocompletion, and smart indent
- Python shell with syntax highlighting
<li>Integrated debugger with stepping, persistent breakpoints, and call
stack visibility</li>
- Automatic indentation (useful for beginners learning about Python's indentation)
- Saving the Python program as .py files and run them and edit them later at any them using IDLE.

In IDLE, hit `F5` or `run Python Shell` to launch an interpreter. Using IDLE can be a better learning experience for new users because code is interpreted as the user writes.

Note that there are lots of alternatives, see for example [this discussion](http://stackoverflow.com/questions/81584/what-ide-to-use-for-python) or [this list](https://wiki.python.org/moin/PythonEditors).

### Troubleshooting

<li>
**Windows**
If you're on Windows, the default command is `python`. If you receive a `"'python' is not recognized"` error, the most likely cause is that Python's location is not in your system's `PATH` environment variable. This can be accessed by right-clicking on 'My Computer' and selecting 'Properties' or by navigating to 'System' through 'Control Panel'. Click on 'Advanced system settings' and then 'Environment Variables...'. Edit the `PATH` variable to include the directory of your Python installation, as well as the Script folder (usually `C:\Python27;C:\Python27\Scripts`). This requires administrative privileges and may require a restart.
When using multiple versions of Python on the same machine, a possible solution is to rename one of the `python.exe` files. For example, naming one version `python27.exe` would cause `python27` to become the Python command for that version.
You can also use the Python Launcher for Windows, which is available through the installer and comes by default. It allows you to select the version of Python to run by using `py -[x.y]` instead of `python[x.y]`. You can use the latest version of Python 2 by running scripts with `py -2` and the latest version of Python 3 by running scripts with `py -3`.
</li>

<li>
**Debian/Ubuntu/MacOS**
This section assumes that the location of the `python` executable has been added to the `PATH` environment variable.
If you're on Debian/Ubuntu/MacOS, open the terminal and type `python` for Python 2.x or `python3` for Python 3.x.
Type `which python` to see which Python interpreter will be used.
</li>

<li>
**Arch Linux**
The default Python on Arch Linux (and descendants) is Python 3, so use `python` or `python3` for Python 3.x and `python2` for Python 2.x.
</li>

<li>
**Other systems**
Python 3 is sometimes bound to `python` instead of `python3`. To use Python 2 on these systems where it is installed, you can use `python2`.
</li>



## User Input


**Interactive input**

To get input from the user, use the `input` function (**note**: in Python 2.x, the function is called `raw_input` instead, although Python 2.x has its own version of [`input`](https://docs.python.org/2/library/functions.html#input) that is completely different):

```py
name = raw_input("What is your name? ")
# Out: What is your name? _

```

> 
**Security Remark** Do not use `input()` in Python2 - the entered text will be evaluated as if it were a Python expression (equivalent to `eval(input())` in Python3), which might easily become a vulnerability. See [this article](https://medium.com/@GallegoDor/python-exploitation-1-input-ac10d3f4491f#.cr6w4z7q8) for further information on the risks of using this function.


```py
name = input("What is your name? ")
# Out: What is your name? _

```

The remainder of this example will be using Python 3 syntax.

The function takes a string argument, which displays it as a prompt and returns a string. The above code provides a prompt, waiting for the user to input.

```py
name = input("What is your name? ")
# Out: What is your name?

```

If the user types "Bob" and hits enter, the variable `name` will be assigned to the string `"Bob"`:

```py
name = input("What is your name? ")
# Out: What is your name? Bob
print(name)
# Out: Bob

```

Note that the `input` is always of type `str`, which is important if you want the user to enter numbers. Therefore, you need to convert the `str` before trying to use it as a number:

```py
x = input("Write a number:")
# Out: Write a number: 10
x / 2
# Out: TypeError: unsupported operand type(s) for /: 'str' and 'int'
float(x) / 2
# Out: 5.0

```

NB: It's recommended to use [`try`/`except` blocks](http://stackoverflow.com/documentation/python/1788/exceptions/5530/catching-exceptions#t=201607241956183569764) to [catch exceptions when dealing with user inputs](http://stackoverflow.com/documentation/python/1788/exceptions/8484/practical-examples-of-exception-handling#t=201607241956183569764). For instance, if your code wants to cast a `raw_input` into an `int`, and what the user writes is uncastable, it raises a `ValueError`.



## Built in Modules and Functions


A module is a file containing Python definitions and statements. Function is a piece of code which execute some logic.

```py
>>> pow(2,3)    #8

```

To check the built in function in python we can use `dir().` If called without an argument, return the names in the current scope. Else, return an alphabetized list of names comprising (some of) the attribute of the given object, and of attributes reachable from it.

```py
>>> dir(__builtins__)
[
    'ArithmeticError', 
    'AssertionError', 
    'AttributeError', 
    'BaseException', 
    'BufferError', 
    'BytesWarning', 
    'DeprecationWarning', 
    'EOFError', 
    'Ellipsis', 
    'EnvironmentError', 
    'Exception', 
    'False', 
    'FloatingPointError', 
    'FutureWarning', 
    'GeneratorExit', 
    'IOError', 
    'ImportError', 
    'ImportWarning', 
    'IndentationError', 
    'IndexError', 
    'KeyError', 
    'KeyboardInterrupt', 
    'LookupError', 
    'MemoryError', 
    'NameError', 
    'None', 
    'NotImplemented', 
    'NotImplementedError', 
    'OSError', 
    'OverflowError', 
    'PendingDeprecationWarning', 
    'ReferenceError', 
    'RuntimeError', 
    'RuntimeWarning', 
    'StandardError', 
    'StopIteration', 
    'SyntaxError', 
    'SyntaxWarning', 
    'SystemError', 
    'SystemExit', 
    'TabError', 
    'True', 
    'TypeError', 
    'UnboundLocalError', 
    'UnicodeDecodeError', 
    'UnicodeEncodeError', 
    'UnicodeError', 
    'UnicodeTranslateError', 
    'UnicodeWarning', 
    'UserWarning', 
    'ValueError', 
    'Warning', 
    'ZeroDivisionError', 
    '__debug__', 
    '__doc__', 
    '__import__', 
    '__name__', 
    '__package__', 
    'abs', 
    'all', 
    'any', 
    'apply', 
    'basestring', 
    'bin', 
    'bool', 
    'buffer', 
    'bytearray', 
    'bytes', 
    'callable', 
    'chr', 
    'classmethod', 
    'cmp', 
    'coerce', 
    'compile', 
    'complex', 
    'copyright', 
    'credits', 
    'delattr', 
    'dict', 
    'dir', 
    'divmod', 
    'enumerate', 
    'eval', 
    'execfile', 
    'exit', 
    'file', 
    'filter', 
    'float', 
    'format', 
    'frozenset', 
    'getattr', 
    'globals', 
    'hasattr', 
    'hash', 
    'help', 
    'hex', 
    'id', 
    'input', 
    'int', 
    'intern', 
    'isinstance', 
    'issubclass', 
    'iter', 
    'len', 
    'license', 
    'list', 
    'locals', 
    'long', 
    'map', 
    'max', 
    'memoryview', 
    'min', 
    'next', 
    'object', 
    'oct', 
    'open', 
    'ord', 
    'pow', 
    'print', 
    'property', 
    'quit', 
    'range', 
    'raw_input', 
    'reduce', 
    'reload', 
    'repr', 
    'reversed', 
    'round', 
    'set', 
    'setattr', 
    'slice', 
    'sorted', 
    'staticmethod', 
    'str', 
    'sum', 
    'super', 
    'tuple', 
    'type', 
    'unichr', 
    'unicode', 
    'vars', 
    'xrange', 
    'zip'
]

```

To know the functionality of any function, we can use built in function `help` .

```py
>>> help(max)
Help on built-in function max in module __builtin__:
max(...)
    max(iterable[, key=func]) -> value
    max(a, b, c, ...[, key=func]) -> value
    With a single iterable argument, return its largest item.
    With two or more arguments, return the largest argument.

```

Built in modules contains extra functionalities.For example to get square root of a number we need to include `math` module.

```py
>>> import math
>>> math.sqrt(16) # 4.0

```

To know all the functions in a module we can assign the functions list to a variable, and then print the variable.

```py
>>> import math
>>> dir(math)

   ['__doc__', '__name__', '__package__', 'acos', 'acosh', 
   'asin', 'asinh', 'atan', 'atan2', 'atanh', 'ceil', 'copysign', 
   'cos', 'cosh', 'degrees', 'e', 'erf', 'erfc', 'exp', 'expm1', 
   'fabs', 'factorial', 'floor', 'fmod', 'frexp', 'fsum', 'gamma', 
   'hypot', 'isinf', 'isnan', 'ldexp', 'lgamma', 'log', 'log10', 
   'log1p', 'modf', 'pi', 'pow', 'radians', 'sin', 'sinh', 'sqrt', 
   'tan', 'tanh', 'trunc']

```

it seems `__doc__` is useful to provide some documentation in, say, functions

```py
>>> math.__doc__
'This module is always available.  It provides access to the\nmathematical
 functions defined by the C standard.'

```

In addition to functions, documentation can also be provided in modules. So, if you have a file named `helloWorld.py` like this:

```py
"""This is the module docstring."""

def sayHello():
    """This is the function docstring."""
    return 'Hello World'

```

You can access its docstrings like this:

```py
>>> import helloWorld
>>> helloWorld.__doc__
'This is the module docstring.'
>>> helloWorld.sayHello.__doc__
'This is the function docstring.'

```


<li>For any user defined type, its attributes, its class's attributes, and
recursively the attributes of its class's base classes can be retrieved using dir()</li>

```py
>>> class MyClassObject(object):
...     pass
... 
>>> dir(MyClassObject)
['__class__', '__delattr__', '__dict__', '__doc__', '__format__', '__getattribute__', '__hash__', '__init__', '__module__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__', '__weakref__']

```

Any data type can be simply converted to string using a builtin function called `str`. This function is called by default when a data type is passed to `print`

```py
>>> str(123)    # "123"

```



## Creating a module


A module is an importable file containing definitions and statements.

A module can be created by creating a `.py` file.

```py
# hello.py
def say_hello():
    print("Hello!")

```

Functions in a module can be used by importing the module.

For modules that you have made, they will need to be in the same directory as the file that you are importing them into. (However, you can also put them into the Python lib directory with the pre-included modules, but should be avoided if possible.)

```py
$ python
>>> import hello
>>> hello.say_hello()
=> "Hello!"

```

Modules can be imported by other modules.

```py
# greet.py
import hello
hello.say_hello()

```

Specific functions of a module can be imported.

```py
# greet.py
from hello import say_hello
say_hello()

```

Modules can be aliased.

```py
# greet.py
import hello as ai
ai.say_hello()

```

A module can be stand-alone runnable script.

```py
# run_hello.py
if __name__ == '__main__':
    from hello import say_hello
    say_hello()

```

Run it!

```py
$ python run_hello.py
=> "Hello!"

```

If the module is inside a directory and needs to be detected by python, the directory should contain a file named `__init__.py`.



## Installation of Python 2.7.x and 3.x


> 
**Note**: Following instructions are written for Python 2.7 (unless specified): instructions for Python 3.x are similar.


**WINDOWS**

First, download the latest version of Python 2.7 from the official Website ([https://www.python.org/downloads/)](https://www.python.org/downloads/)). Version is provided as an MSI package. To install it manually, just double-click the file.

By default, Python installs to a directory:

```

C:\Python27\

```

Warning: installation does not automatically modify the PATH environment variable.

Assuming that your Python installation is in C:\Python27, add this to your PATH:

```py
C:\Python27\;C:\Python27\Scripts\

```

Now to check if Python installation is valid write in cmd:

```py
python --version

```

**Python 2.x and 3.x Side-By-Side**

To install and use both Python 2.x and 3.x side-by-side on a Windows machine:

<li>
Install Python 2.x using the MSI installer.
<ul>
1. Ensure Python is installed for all users.
1. Optional: add Python to `PATH` to make Python 2.x callable from the command-line using `python`.
</ul>
</li>
<li>
Install Python 3.x using its respective installer.
<ul>
1. Again, ensure Python is installed for all users.
1. Optional: add Python to `PATH` to make Python 3.x callable from the command-line using `python`. This may override Python 2.x `PATH` settings, so double-check your `PATH` and ensure it's configured to your preferences.
1. Make sure to install the `py launcher` for all users.
</ul>
</li>

- Again, ensure Python is installed for all users.
- Optional: add Python to `PATH` to make Python 3.x callable from the command-line using `python`. This may override Python 2.x `PATH` settings, so double-check your `PATH` and ensure it's configured to your preferences.
- Make sure to install the `py launcher` for all users.

Python 3 will install the Python launcher which can be used to launch Python 2.x and Python 3.x interchangeably from the command-line:

```py
P:\>py -3
Python 3.6.1 (v3.6.1:69c0db5, Mar 21 2017, 17:54:52) [MSC v.1900 32 bit (Intel)] on win32
Type "help", "copyright", "credits" or "license" for more information.
>>>

C:\>py -2
Python 2.7.13 (v2.7.13:a06454b1afa1, Dec 17 2016, 20:42:59) [MSC v.1500 32 Intel)] on win32
Type "help", "copyright", "credits" or "license" for more information.
>>>

```

To use the corresponding version of `pip` for a specific Python version, use:

```py
C:\>py -3 -m pip -V
pip 9.0.1 from C:\Python36\lib\site-packages (python 3.6)

C:\>py -2 -m pip -V
pip 9.0.1 from C:\Python27\lib\site-packages (python 2.7)

```

**LINUX**

The latest versions of CentOS, Fedora, Redhat Enterprise (RHEL) and Ubuntu come with Python 2.7.

To install Python 2.7 on linux manually, just do the following in terminal:

```py
wget --no-check-certificate https://www.python.org/ftp/python/2.7.X/Python-2.7.X.tgz
tar -xzf Python-2.7.X.tgz  
cd Python-2.7.X
./configure  
make  
sudo make install

```

Also add the path of new python in PATH environment variable. If new python is in `/root/python-2.7.X` then run `export PATH = $PATH:/root/python-2.7.X`

Now to check if Python installation is valid write in terminal:

```py
python --version

```

**Ubuntu (From Source)**

If you need Python 3.6 you can install it from source as shown below (Ubuntu 16.10 and 17.04 have 3.6 version in the universal repository). Below steps have to be followed for Ubuntu 16.04 and lower versions:

```py
sudo apt install build-essential checkinstall
sudo apt install libreadline-gplv2-dev libncursesw5-dev libssl-dev libsqlite3-dev tk-dev libgdbm-dev libc6-dev libbz2-dev
wget https://www.python.org/ftp/python/3.6.1/Python-3.6.1.tar.xz
tar xvf Python-3.6.1.tar.xz 
cd Python-3.6.1/
./configure --enable-optimizations
sudo make altinstall

```

**macOS**

As we speak, macOS comes installed with Python 2.7.10, but this version is outdated and slightly modified from the regular Python.

> 
<p>The version of Python that ships with OS X is great for learning but
it’s not good for development. The version shipped with OS X may be
out of date from the official current Python release, which is
considered the stable production version. ([source](http://docs.python-guide.org/en/latest/starting/install/osx/))</p>


Install [Homebrew](https://brew.sh/):

```py
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

```

Install Python 2.7:

```py
brew install python

```

For Python 3.x, use the command `brew install python3` instead.



## String function - str() and repr()


There are two functions that can be used to obtain a readable representation of an object.

`repr(x)` calls `x.__repr__()`: a representation of `x`. `eval` will usually convert the result of this function back to the original object.

`str(x)` calls `x.__str__()`: a human-readable string that describes the object. This may elide some technical detail.

### **repr()**

For many types, this function makes an attempt to return a string that would yield an object with the same value when passed to `eval()`. Otherwise, the representation is a string enclosed in angle brackets that contains the name of the type of the object along with additional information. This often includes the name and address of the object.

### **str()**

For strings, this returns the string itself. The difference between this and `repr(object)` is that `str(object)` does not always attempt to return a string that is acceptable to `eval()`. Rather, its goal is to return a printable or 'human readable' string. If no argument is given, this returns the empty string, `''`.

Example 1:

```py
s = """w'o"w"""
repr(s) # Output: '\'w\\\'o"w\''  
str(s)  # Output: 'w\'o"w'
eval(str(s)) == s  # Gives a SyntaxError 
eval(repr(s)) == s # Output: True

```

Example 2:

```py
import datetime
today = datetime.datetime.now()
str(today)  # Output: '2016-09-15 06:58:46.915000'
repr(today) # Output: 'datetime.datetime(2016, 9, 15, 6, 58, 46, 915000)'

```

When writing a class, you can override these methods to do whatever you want:

```py
class Represent(object):

    def __init__(self, x, y):
        self.x, self.y = x, y

    def __repr__(self):
        return "Represent(x={},y=\"{}\")".format(self.x, self.y)

    def __str__(self):
        return "Representing x as {} and y as {}".format(self.x, self.y)

```

Using the above class we can see the results:

```py
r = Represent(1, "Hopper")
print(r)  # prints __str__
print(r.__repr__)  # prints __repr__: '<bound method Represent.__repr__ of Represent(x=1,y="Hopper")>'
rep = r.__repr__()  # sets the execution of __repr__ to a new variable
print(rep)  # prints 'Represent(x=1,y="Hopper")'
r2 = eval(rep) # evaluates rep
print(r2)  # prints __str__ from new object
print(r2 == r)  # prints 'False' because they are different objects

```



## Installing external modules using pip


`pip` is your friend when you need to install any package from the plethora of choices available at the python package index (PyPI). `pip` is already installed if you're using Python 2 >= 2.7.9 or Python 3 >= 3.4 downloaded from python.org. For computers running Linux or another *nix with a native package manager, `pip` must often be [manually installed.](https://pip.pypa.io/en/stable/installing/)

On instances with both Python 2 and Python 3 installed, `pip` often refers to Python 2 and `pip3` to Python 3. Using `pip` will only install packages for Python 2 and `pip3` will only install packages for Python 3.

### Finding / installing a package

Searching for a package is as simple as typing

```py
$ pip search <query>
# Searches for packages whose name or summary contains <query>

```

Installing a package is as simple as typing **(in a terminal / command-prompt, not in the Python interpreter)**

```py
$ pip install [package_name]           # latest version of the package

$ pip install [package_name]==x.x.x    # specific version of the package

$ pip install '[package_name]>=x.x.x'  # minimum version of the package

```

where `x.x.x` is the version number of the package you want to install.

When your server is behind proxy, you can install package by using below command:

```py
$ pip --proxy http://<server address>:<port> install

```

### Upgrading installed packages

When new versions of installed packages appear they are not automatically installed to your system. To get an overview of which of your installed packages have become outdated, run:

```py
$ pip list --outdated

```

To upgrade a specific package use

```py
$ pip install [package_name] --upgrade

```

Updating all outdated packages is not a standard functionality of `pip`.

### Upgrading pip

You can upgrade your existing pip installation by using the following commands

<li>
On Linux or macOS X:
<pre><code>$ pip install -U pip
</code></pre>
You may need to use `sudo` with pip on some Linux Systems
</li>
<li>
On Windows:
<pre><code>py -m pip install -U pip
</code></pre>
or
<pre><code>python -m pip install -U pip
</code></pre>
</li>

For more information regarding pip do [read here](https://pip.pypa.io/en/stable/).



## Help Utility


Python has several functions built into the interpreter.
If you want to get information of keywords, built-in functions, modules or topics open a Python console and enter:

```py
>>> help()

```

You will receive information by entering keywords directly:

```py
>>> help(help)

```

**or** within the utility:

```py
help> help

```

which will show an explanation:

```py
Help on _Helper in module _sitebuiltins object:

class _Helper(builtins.object)
 |  Define the builtin 'help'.
 |  
 |  This is a wrapper around pydoc.help that provides a helpful message
 |  when 'help' is typed at the Python interactive prompt.
 |  
 |  Calling help() at the Python prompt starts an interactive help session.
 |  Calling help(thing) prints help for the python object 'thing'.
 |  
 |  Methods defined here:
 |  
 |  __call__(self, *args, **kwds)
 |  
 |  __repr__(self)
 |  
 |  ----------------------------------------------------------------------
 |  Data descriptors defined here:
 |  
 |  __dict__
 |      dictionary for instance variables (if defined)
 |  
 |  __weakref__
 |      list of weak references to the object (if defined)

```

You can also request subclasses of modules:

```py
help(pymysql.connections)

```

You can use help to access the docstrings of the different modules you have imported, e.g., try the following:

```py
>>> help(math)

```

and you'll get an error

```py
>>> import math
>>> help(math)

```

And now you will get a list of the available methods in the module, but only AFTER you have imported it.

Close the helper with `quit`



#### Remarks


<a href="https://www.python.org/" rel="nofollow noreferrer"><img src="https://i.stack.imgur.com/Bews8.png" alt="Python logo" /><br />
Python</a> is a widely used programming language. It is:

<li>
**High-level**: Python automates low-level operations such as memory management. It leaves the programmer with a bit less control but has many benefits including code readability and minimal code expressions.
</li>
<li>
**General-purpose**: Python is built to be used in all contexts and environments. An example for a non-general-purpose language is PHP: it is designed specifically as a server-side web-development scripting language. In contrast, Python **can** be used for server-side web-development, but also for building desktop applications.
</li>
<li>
**Dynamically typed**: Every variable in Python can reference any type of data. A single expression may evaluate to data of different types at different times. Due to that, the following code is possible:
<pre><code>if something:
    x = 1
else:
    x = 'this is a string'
print(x)
</code></pre>
</li>
<li>
**Strongly typed**: During program execution, you are not allowed to do anything that's incompatible with the type of data you're working with. For example, there are no hidden conversions from strings to numbers; a string made out of digits will never be treated as a number unless you convert it explicitly:
<pre><code>1 + '1'  # raises an error
1 + int('1')  # results with 2
</code></pre>
</li>
<li>
**Beginner friendly :)**: Python's syntax and structure are very intuitive. It is high level and provides constructs intended to enable writing clear programs on both a small and large scale. Python supports multiple programming paradigms, including object-oriented, imperative and functional programming or procedural styles. It has a large, comprehensive standard library and many easy-to-install 3rd party libraries.
</li>

Its design principles are outlined in [**The Zen of Python**](https://www.python.org/dev/peps/pep-0020/).

Currently, there are two major release branches of Python which have some significant differences. Python 2.x is the legacy version though it still sees widespread use. Python 3.x makes a set of backwards-incompatible changes which aim to reduce feature duplication. For help deciding which version is best for you, see [this article](https://wiki.python.org/moin/Python2orPython3).

The [official Python documentation](https://docs.python.org) is also a comprehensive and useful resource, containing documentation for all versions of Python as well as tutorials to help get you started.

There is one official implementation of the language supplied by Python.org, generally referred to as CPython, and several alternative implementations of the language on other runtime platforms. These include [IronPython](http://ironpython.net/) (running Python on the .NET platform), [Jython](http://www.jython.org/) (on the Java runtime) and [PyPy](http://pypy.org/) (implementing Python in a subset of itself).

