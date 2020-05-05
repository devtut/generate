---
metaTitle: "Python - String Methods"
description: "Changing the capitalization of a string, str.translate: Translating characters in a string, str.format and f-strings: Format values into a string, String module's useful constants, Split a string based on a delimiter into a list of strings, Replace all occurrences of one substring with another substring, Testing what a string is composed of, Stripping unwanted leading/trailing characters from a string, Reversing a string, Join a list of strings into one string, String Contains, Counting number of times a substring appears in a string, Case insensitive string comparisons, Test the starting and ending characters of a string, Justify strings, Conversion between str or bytes data and unicode characters"
---

# String Methods



## Changing the capitalization of a string


Python's string type provides many functions that act on the capitalization of a string. These include :

- `str.casefold`
- `str.upper`
- `str.lower`
- `str.capitalize`
- `str.title`
- `str.swapcase`

With unicode strings (the default in Python 3), these operations are **not** 1:1 mappings or reversible. Most of these operations are intended for display purposes, rather than normalization.

### `str.casefold()`

`str.casefold` creates a lowercase string that is suitable for case insensitive comparisons. This is more aggressive than `str.lower` and may modify strings that are already in lowercase or cause strings to grow in length, and is not intended for display purposes.

```py
"XßΣ".casefold()
# 'xssσ'

"XßΣ".lower()
# 'xßς'

```

The transformations that take place under casefolding are defined by the Unicode Consortium in the CaseFolding.txt file on their website.

### `str.upper()`

`str.upper` takes every character in a string and converts it to its uppercase equivalent, for example:

```py
"This is a 'string'.".upper()
# "THIS IS A 'STRING'."

```

### `str.lower()`

`str.lower` does the opposite; it takes every character in a string and converts it to its lowercase equivalent:

```py
"This IS a 'string'.".lower()
# "this is a 'string'."

```

### `str.capitalize()`

`str.capitalize` returns a capitalized version of the string, that is, it makes the first character have upper case and the rest lower:

```py
"this Is A 'String'.".capitalize() # Capitalizes the first character and lowercases all others
# "This is a 'string'."

```

### `str.title()`

`str.title` returns the title cased version of the string, that is, every letter in the beginning of a word is made upper case and all others are made lower case:

```py
"this Is a 'String'".title()
# "This Is A 'String'"

```

### `str.swapcase()`

`str.swapcase` returns a new string object in which all lower case characters are swapped to upper case and all upper case characters to lower:

```py
"this iS A STRiNG".swapcase() #Swaps case of each character
# "THIS Is a strIng"

```

### Usage as `str` class methods

It is worth noting that these methods may be called either on string objects (as shown above) or as a class method of the `str` class (with an explicit call to `str.upper`, etc.)

```py
str.upper("This is a 'string'")
# "THIS IS A 'STRING'"

```

This is most useful when applying one of these methods to many strings at once in say, a **[`map`](http://stackoverflow.com/documentation/python/333/map-function)**  function.

```py
map(str.upper,["These","are","some","'strings'"])
# ['THESE', 'ARE', 'SOME', "'STRINGS'"]

```



## str.translate: Translating characters in a string


Python supports a `translate` method on the `str` type which allows you to specify the translation table (used for replacements) as well as any characters which should be deleted in the process.

```py
table</code></td>|It is a lookup table that defines the mapping from one character to another.</tr>|`deletechars`|A list of characters which are to be removed from the string.
</tbody></table>
The `maketrans` method (`str.maketrans` in Python 3 and `string.maketrans` in Python 2) allows you to generate a translation table.

```py
>>> translation_table = str.maketrans("aeiou", "12345")
>>> my_string = "This is a string!"
>>> translated = my_string.translate(translation_table)
'Th3s 3s 1 str3ng!'

```

The `translate` method returns a string which is a translated copy of the original string.

You can set the `table` argument to `None` if you only need to delete characters.

```py
>>> 'this syntax is very useful'.translate(None, 'aeiou')
'ths syntx s vry sfl'

```



## str.format and f-strings: Format values into a string


Python provides string interpolation and formatting functionality through the `str.format` function, introduced in version 2.6 and f-strings introduced in version 3.6.

Given the following variables:

```py
i = 10
f = 1.5
s = "foo"
l = ['a', 1, 2]
d = {'a': 1, 2: 'foo'}

```

The following statements are all equivalent

```py
"10 1.5 foo ['a', 1, 2] {'a': 1, 2: 'foo'}"

```

```py
>>> "{} {} {} {} {}".format(i, f, s, l, d)

>>> str.format("{} {} {} {} {}", i, f, s, l, d)

>>> "{0} {1} {2} {3} {4}".format(i, f, s, l, d)

>>> "{0:d} {1:0.1f} {2} {3!r} {4!r}".format(i, f, s, l, d)

>>> "{i:d} {f:0.1f} {s} {l!r} {d!r}".format(i=i, f=f, s=s, l=l, d=d)

```

```py
>>> f"{i} {f} {s} {l} {d}"

>>> f"{i:d} {f:0.1f} {s} {l!r} {d!r}"

```

For reference, Python also supports C-style qualifiers for string formatting. The examples below are equivalent to those above, but the `str.format` versions are preferred due to benefits in flexibility, consistency of notation, and extensibility:

```py
"%d %0.1f %s %r %r" % (i, f, s, l, d)

"%(i)d %(f)0.1f %(s)s %(l)r %(d)r" % dict(i=i, f=f, s=s, l=l, d=d)

```

The braces uses for interpolation in `str.format` can also be numbered to reduce duplication when formatting strings. For example, the following are equivalent:

```py
"I am from Australia. I love cupcakes from Australia!"

```

```py
>>> "I am from {}. I love cupcakes from {}!".format("Australia", "Australia")

>>> "I am from {0}. I love cupcakes from {0}!".format("Australia")

```

While the official python documentation is, as usual, thorough enough, [pyformat.info](http://pyformat.info) has a great set of examples with detailed explanations.

Additionally, the `{` and `}` characters can be escaped by using double brackets:

```py
"{'a': 5, 'b': 6}"

```

```py
>>> "{{'{}': {}, '{}': {}}}".format("a", 5, "b", 6)

>>> f"{{'{'a'}': {5}, '{'b'}': {6}}"

```

See [String Formatting](http://stackoverflow.com/documentation/python/1019/string-formatting) for additional information. `str.format()` was proposed in [PEP 3101](https://www.python.org/dev/peps/pep-3101/) and f-strings in [PEP 498](https://www.python.org/dev/peps/pep-0498/).



## String module's useful constants


Python's `string` module provides constants for string related operations. To use them, import the `string` module:

```py
>>> import string

```

### `string.ascii_letters`:

Concatenation of `ascii_lowercase` and `ascii_uppercase`:

```py
>>> string.ascii_letters
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

```

### `string.ascii_lowercase`:

Contains all lower case ASCII characters:

```py
>>> string.ascii_lowercase
'abcdefghijklmnopqrstuvwxyz'

```

### `string.ascii_uppercase`:

Contains all upper case ASCII characters:

```py
>>> string.ascii_uppercase
'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

```

### `string.digits`:

Contains all decimal digit characters:

```py
>>> string.digits
'0123456789'

```

### `string.hexdigits`:

Contains all hex digit characters:

```py
>>> string.hexdigits
'0123456789abcdefABCDEF'

```

### `string.octaldigits`:

Contains all octal digit characters:

```py
>>> string.octaldigits
'01234567'

```

### `string.punctuation`:

Contains all characters which are considered punctuation in the `C` locale:

```py
>>> string.punctuation
'!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~'

```

### `string.whitespace`:

Contains all ASCII characters considered whitespace:

```py
>>> string.whitespace
' \t\n\r\x0b\x0c'

```

In script mode, `print(string.whitespace)` will print the actual characters, use `str` to get the string returned above.

### `string.printable`:

Contains all characters which are considered printable; a combination of `string.digits`, `string.ascii_letters`, `string.punctuation`, and `string.whitespace`.

```py
>>> string.printable
'0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c'

```



## Split a string based on a delimiter into a list of strings


### `str.split(sep=None, maxsplit=-1)`

`str.split` takes a string and returns a list of substrings of the original string. The behavior differs depending on whether the `sep` argument is provided or omitted.

If `sep` isn't provided, or is `None`, then the splitting takes place wherever there is whitespace. However, leading and trailing whitespace is ignored, and multiple consecutive whitespace characters are treated the same as a single whitespace character:

```py
>>> "This is a sentence.".split()
['This', 'is', 'a', 'sentence.']

>>> " This is    a sentence.  ".split()
['This', 'is', 'a', 'sentence.']

>>> "            ".split()
[]

```

The `sep` parameter can be used to define a delimiter string. The original string is split where the delimiter string occurs, and the delimiter itself is discarded. Multiple consecutive delimiters are **not** treated the same as a single occurrence, but rather cause empty strings to be created.

```py
>>> "This is a sentence.".split(' ')
['This', 'is', 'a', 'sentence.']

>>> "Earth,Stars,Sun,Moon".split(',')
['Earth', 'Stars', 'Sun', 'Moon']

>>> " This is    a sentence.  ".split(' ')
['', 'This', 'is', '', '', '', 'a', 'sentence.', '', '']

>>> "This is a sentence.".split('e')
['This is a s', 'nt', 'nc', '.']

>>> "This is a sentence.".split('en')
['This is a s', 't', 'ce.']

```

The default is to split on **every** occurrence of the delimiter, however the `maxsplit` parameter limits the number of splittings that occur. The default value of `-1` means no limit:

```py
>>> "This is a sentence.".split('e', maxsplit=0)
['This is a sentence.']

>>> "This is a sentence.".split('e', maxsplit=1)
['This is a s', 'ntence.']

>>> "This is a sentence.".split('e', maxsplit=2)
['This is a s', 'nt', 'nce.']

>>> "This is a sentence.".split('e', maxsplit=-1)
['This is a s', 'nt', 'nc', '.']

```

### `str.rsplit(sep=None, maxsplit=-1)`

`str.rsplit` ("right split") differs from `str.split` ("left split") when `maxsplit` is specified. The splitting starts at the end of the string rather than at the beginning:

```py
>>> "This is a sentence.".rsplit('e', maxsplit=1)
['This is a sentenc', '.']

>>> "This is a sentence.".rsplit('e', maxsplit=2)
['This is a sent', 'nc', '.']

```

**Note**: Python specifies the maximum number of **splits** performed, while most other programming languages specify the maximum number of **substrings** created. This may create confusion when porting or comparing code.



## Replace all occurrences of one substring with another substring


Python's `str` type also has a method for replacing occurences of one sub-string with another sub-string in a given string. For more demanding cases, one can use [`re.sub`](http://stackoverflow.com/documentation/python/632/regular-expressions/2068/replacing-with-a-string#t=201607231554255817393).

### `str.replace(old, new[, count])`:

`str.replace` takes two arguments `old` and `new` containing the `old` sub-string which is to be replaced by the `new` sub-string. The optional argument `count` specifies the number of replacements to be made:

For example, in order to replace `'foo'` with `'spam'` in the following string,  we can call `str.replace` with `old = 'foo'` and `new = 'spam'`:

```py
>>> "Make sure to foo your sentence.".replace('foo', 'spam')
"Make sure to spam your sentence."

```

If the given string contains multiple examples that match the `old` argument, **all** occurrences are replaced with the value supplied in `new`:

```py
>>> "It can foo multiple examples of foo if you want.".replace('foo', 'spam')
"It can spam multiple examples of spam if you want."

```

unless, of course, we supply a value for `count`. In this case `count` occurrences are going to get replaced:

```py
>>> """It can foo multiple examples of foo if you want, \
... or you can limit the foo with the third argument.""".replace('foo', 'spam', 1)
'It can spam multiple examples of foo if you want, or you can limit the foo with the third argument.'

```



## Testing what a string is composed of


Python's `str` type also features a number of methods that can be used to evaluate the contents of a string. These are `str.isalpha`, `str.isdigit`, `str.isalnum`, `str.isspace`. Capitalization can be tested with `str.isupper`, `str.islower` and `str.istitle`.

### `str.isalpha`

`str.isalpha` takes no arguments and returns `True` if the all characters in a given string are alphabetic, for example:

```py
>>> "Hello World".isalpha()  # contains a space
False
>>> "Hello2World".isalpha()  # contains a number
False
>>> "HelloWorld!".isalpha()  # contains punctuation
False
>>> "HelloWorld".isalpha()
True

```

As an edge case, the empty string evaluates to `False` when used with `"".isalpha()`.

### `str.isupper`, `str.islower`, `str.istitle`

These methods test the capitalization in a given string.

`str.isupper` is a method that returns `True` if all characters in a given string are uppercase and `False` otherwise.

```py
>>> "HeLLO WORLD".isupper()
False
>>> "HELLO WORLD".isupper()
True
>>> "".isupper()
False

```

Conversely, `str.islower` is a method that returns `True` if all characters in a given string are lowercase and `False` otherwise.

```py
>>> "Hello world".islower()
False
>>> "hello world".islower()
True
>>> "".islower()
False

```

`str.istitle` returns `True` if the given string is title cased; that is, every word begins with an uppercase character followed by lowercase characters.

```py
>>> "hello world".istitle()
False
>>> "Hello world".istitle()
False
>>> "Hello World".istitle()
True
>>> "".istitle()
False

```

### `str.isdecimal`, `str.isdigit`, `str.isnumeric`

`str.isdecimal` returns whether the string is a sequence of decimal digits, suitable for representing a decimal number.

`str.isdigit` includes digits not in a form suitable for representing a decimal
number, such as superscript digits.

`str.isnumeric` includes any number values, even if not digits, such as values outside the range 0-9.

```

           isdecimal    isdigit   isnumeric

12345        True        True       True
១2߃໔5        True        True       True
①²³🄅₅       False       True       True
⑩⒓          False       False      True
Five         False       False      False

```

Bytestrings (`bytes` in Python 3, `str` in Python 2), only support `isdigit`, which only checks for basic ASCII digits.

As with `str.isalpha`, the empty string evaluates to `False`.

### `str.isalnum`

This is a combination of `str.isalpha` and `str.isnumeric`, specifically it evaluates to `True` if all characters in the given string are **alphanumeric**, that is, they consist of alphabetic **or** numeric characters:

```py
>>> "Hello2World".isalnum()
True
>>> "HelloWorld".isalnum()
True
>>> "2016".isalnum()
True
>>> "Hello World".isalnum()  # contains whitespace
False

```

### `str.isspace`

Evaluates to `True` if the string contains only whitespace characters.

```py
>>> "\t\r\n".isspace()
True
>>> " ".isspace()
True

```

Sometimes a string looks “empty” but we don't know whether it's because it contains just whitespace or no character at all

```py
>>> "".isspace()
False

```

To cover this case we need an additional test

```py
>>> my_str = ''
>>> my_str.isspace()
False
>>> my_str.isspace() or not my_str
True

```

But the shortest way to test if a string is empty or just contains whitespace characters is to use [`strip`](http://stackoverflow.com/documentation/python/278/string-methods/8777/stripping-unwanted-leading-trailing-characters-from-a-string#t=201608150741303855198)(with no arguments it removes all leading and trailing whitespace characters)

```py
>>> not my_str.strip()
True

```



## Stripping unwanted leading/trailing characters from a string


Three methods are provided that offer the ability to strip leading and trailing characters from a string: `str.strip`, `str.rstrip` and `str.lstrip`. All three methods have the same signature and all three return a new string object with unwanted characters removed.

### `str.strip([chars])`

`str.strip` acts on a given string and removes (strips) any leading or trailing characters contained in the argument `chars`; if `chars` is not supplied or is `None`, all white space characters are removed by default. For example:

```py
>>> "    a line with leading and trailing space     ".strip() 
'a line with leading and trailing space'

```

If `chars` is supplied, all characters contained in it are removed from the string, which is returned. For example:

```py
>>> ">>> a Python prompt".strip('> ')  # strips '>' character and space character 
'a Python prompt'

```

### `str.rstrip([chars])` and `str.lstrip([chars])`

These methods have similar semantics and arguments with `str.strip()`, their difference lies in the direction from which they start. `str.rstrip()` starts from the end of the string while `str.lstrip()` splits from the start of the string.

For example, using `str.rstrip`:

```py
>>> "     spacious string      ".rstrip()
'     spacious string'

```

While, using `str.lstrip`:

```py
>>> "     spacious string      ".rstrip()
'spacious string      '

```



## Reversing a string


A string can reversed using the built-in `reversed()` function, which takes a string and returns an iterator in reverse order.

```py
>>> reversed('hello')
<reversed object at 0x0000000000000000>
>>> [char for char in reversed('hello')]
['o', 'l', 'l', 'e', 'h']

```

`reversed()` can be wrapped in a call to [`''.join()` to make a string](http://stackoverflow.com/documentation/python/278/string-methods/10177/join-a-list-of-strings-into-one-string) from the iterator.

```py
>>> ''.join(reversed('hello'))
'olleh'

```

While using `reversed()` might be more readable to uninitiated Python users,  using extended [slicing](https://stackoverflow.com/documentation/python/289/indexing-and-slicing/1042/basic-slicing) with a step of `-1` is faster and more concise. Here , try to implement it as function:

```py
>>> def reversed_string(main_string):
...     return main_string[::-1]
...
>>> reversed_string('hello')
'olleh'

```



## Join a list of strings into one string


A string can be used as a separator to join a list of strings together into a
single string using the `join()` method. For example you can create a string
where each element in a list is separated by a space.

```py
>>> " ".join(["once","upon","a","time"])
"once upon a time"

```

The following example separates the string elements with three hyphens.

```py
>>> "---".join(["once", "upon", "a", "time"])
"once---upon---a---time"

```



## String Contains


Python makes it extremely intuitive to check if a string contains a given substring. Just use the `in` operator:

```py
>>> "foo" in "foo.baz.bar"
True

```

Note: testing an empty string will always result in `True`:

```py
>>> "" in "test"
True

```



## Counting number of times a substring appears in a string


One method is available for counting the number of occurrences of a sub-string in another string, `str.count`.

### `str.count(sub[, start[, end]])`

`str.count` returns an `int` indicating the number of non-overlapping occurrences of the sub-string `sub` in another string. The optional arguments `start` and `end` indicate the beginning and the end in which the search will take place. By default `start = 0` and `end = len(str)` meaning the whole string will be searched:

```py
>>> s = "She sells seashells by the seashore."
>>> s.count("sh")
2
>>> s.count("se")
3
>>> s.count("sea")
2
>>> s.count("seashells")
1

```

By specifying a different value for `start`, `end` we can get a more localized search and count, for example, if `start` is equal to `13` the call to:

```py
>>> s.count("sea", start)
1

```

is equivalent to:

```py
>>> t = s[start:]
>>> t.count("sea")
1

```



## Case insensitive string comparisons


Comparing string in a case insensitive way seems like something that's trivial, but it's not. This section only considers unicode strings (the default in Python 3). Note that Python 2 may have subtle weaknesses relative to Python 3 - the later's unicode handling is much more complete.

The first thing to note it that case-removing conversions in unicode aren't trivial. There is text for which `text.lower() != text.upper().lower()`, such as `"ß"`:

```py
>>> "ß".lower()
'ß'

>>> "ß".upper().lower()
'ss'

```

But let's say you wanted to caselessly compare `"BUSSE"` and `"Buße"`. Heck, you probably also want to compare `"BUSSE"` and `"BUẞE"` equal - that's the newer capital form. The recommended way is to use `casefold`:

```py
>>> help(str.casefold)
"""
Help on method_descriptor:

casefold(...)
      S.casefold() -> str
    
     Return a version of S suitable for caseless comparisons.
"""

```

Do not just use `lower`. If `casefold` is not available, doing `.upper().lower()` helps (but only somewhat).

Then you should consider accents. If your font renderer is good, you probably think `"ê" == "ê"` - but it doesn't:

```py
>>> "ê" == "ê"
False

```

This is because they are actually

```py
>>> import unicodedata

>>> [unicodedata.name(char) for char in "ê"]
['LATIN SMALL LETTER E WITH CIRCUMFLEX']

>>> [unicodedata.name(char) for char in "ê"]
['LATIN SMALL LETTER E', 'COMBINING CIRCUMFLEX ACCENT']

```

The simplest way to deal with this is `unicodedata.normalize`. You probably want to use **NFKD** normalization, but feel free to check the documentation. Then one does

```py
>>> unicodedata.normalize("NFKD", "ê") == unicodedata.normalize("NFKD", "ê")
True

```

To finish up, here this is expressed in functions:

```py
import unicodedata

def normalize_caseless(text):
    return unicodedata.normalize("NFKD", text.casefold())

def caseless_equal(left, right):
    return normalize_caseless(left) == normalize_caseless(right)

```



## Test the starting and ending characters of a string


In order to test the beginning and ending of a given string in Python, one can use the methods `str.startswith()` and `str.endswith()`.

### `str.startswith(prefix[, start[, end]])`

As it's name implies, `str.startswith` is used to test whether a given string starts with the given characters in `prefix`.

```py
>>> s = "This is a test string"
>>> s.startswith("T")
True
>>> s.startswith("Thi")
True
>>> s.startswith("thi")  
False

```

The optional arguments `start` and `end` specify the start and end points from which the testing will start and finish. In the following example, by specifying a start value of `2` our string will be searched from position `2` and afterwards:

```py
>>> s.startswith("is", 2)
True

```

This yields `True` since `s[2] == 'i'` and `s[3] == 's'`.

You can also use a `tuple` to check if it starts with any of a set of strings

```py
>>> s.startswith(('This', 'That'))
True
>>> s.startswith(('ab', 'bc'))
False

```

### `str.endswith(prefix[, start[, end]])`

`str.endswith` is exactly similar to `str.startswith` with the only difference being that it searches for ending characters and not starting characters. For example, to test if a string ends in a full stop, one could write:

```py
>>> s = "this ends in a full stop."
>>> s.endswith('.')
True
>>> s.endswith('!')
False

```

as with `startswith` more than one characters can used as the ending sequence:

```py
>>> s.endswith('stop.')
True
>>> s.endswith('Stop.')
False

```

You can also use a `tuple` to check if it ends with any of a set of strings

```py
>>> s.endswith(('.', 'something'))
True
>>> s.endswith(('ab', 'bc'))
False

```



## Justify strings


Python provides functions for justifying strings, enabling text padding to make aligning various strings much easier.

Below is an example of `str.ljust` and `str.rjust`:

```py
interstates_lengths = {
    5: (1381, 2222),
    19: (63, 102),
    40: (2555, 4112),
    93: (189,305),
}
for road, length in interstates_lengths.items():
    miles,kms = length
    print('{} -> {} mi. ({} km.)'.format(str(road).rjust(4), str(miles).ljust(4), str(kms).ljust(4)))

```

```py
  40 -> 2555 mi. (4112 km.)
  19 -> 63   mi. (102  km.)
   5 -> 1381 mi. (2222 km.)
  93 -> 189  mi. (305  km.)

```

`ljust` and `rjust` are very similar.  Both have a `width` parameter and an optional `fillchar` parameter.  Any string created by these functions is at least as long as the `width` parameter that was passed into the function.  If the string is longer than `width` alread, it is not truncated.  The `fillchar` argument, which defaults to the space character `' '` must be a single character, not a multicharacter string.

The `ljust` function pads the end of the string it is called on with the `fillchar` until it is `width` characters long.  The `rjust` function pads the beginning of the string in a similar fashion.  Therefore, the `l` and `r` in the names of these functions refer to the side that the original string, **not the `fillchar`**, is positioned in the output string.



## Conversion between str or bytes data and unicode characters


The contents of files and network messages may represent encoded characters. They often need to be converted to unicode for proper display.

In Python 2, you may need to convert str data to Unicode characters. The default (`''`, `""`, etc.) is an ASCII string, with any values outside of ASCII range displayed as escaped values. Unicode strings are `u''` (or `u""`, etc.).

```py
# You get "© abc" encoded in UTF-8 from a file, network, or other data source

s = '\xc2\xa9 abc'  # s is a byte array, not a string of characters
                    # Doesn't know the original was UTF-8
                    # Default form of string literals in Python 2
s[0]                # '\xc2' - meaningless byte (without context such as an encoding)
type(s)             # str - even though it's not a useful one w/o having a known encoding

u = s.decode('utf-8')  # u'\xa9 abc'
                       # Now we have a Unicode string, which can be read as UTF-8 and printed properly
                       # In Python 2, Unicode string literals need a leading u
                       # str.decode converts a string which may contain escaped bytes to a Unicode string
u[0]                # u'\xa9' - Unicode Character 'COPYRIGHT SIGN' (U+00A9) '©'
type(u)             # unicode

u.encode('utf-8')   # '\xc2\xa9 abc'
                    # unicode.encode produces a string with escaped bytes for non-ASCII characters

```

In Python 3 you may need to convert arrays of bytes (referred to as a 'byte literal') to strings of Unicode characters. The default is now a Unicode string, and bytestring literals must now be entered as `b''`, `b""`, etc. A byte literal will return `True` to `isinstance(some_val, byte)`, assuming `some_val` to be a string that might be encoded as bytes.

```py
# You get from file or network "© abc" encoded in UTF-8

s = b'\xc2\xa9 abc' # s is a byte array, not characters
                    # In Python 3, the default string literal is Unicode; byte array literals need a leading b
s[0]                # b'\xc2' - meaningless byte (without context such as an encoding)
type(s)             # bytes - now that byte arrays are explicit, Python can show that.

u = s.decode('utf-8')  # '© abc' on a Unicode terminal
                       # bytes.decode converts a byte array to a string (which will, in Python 3, be Unicode)
u[0]                # '\u00a9' - Unicode Character 'COPYRIGHT SIGN' (U+00A9) '©'
type(u)             # str
                    # The default string literal in Python 3 is UTF-8 Unicode

u.encode('utf-8')   # b'\xc2\xa9 abc'
                    # str.encode produces a byte array, showing ASCII-range bytes as unescaped characters.

```



#### Syntax


- str.capitalize() -> str
- str.casefold() -> str             [only for Python > 3.3]
- str.center(width[, fillchar]) -> str
- str.count(sub[, start[, end]]) -> int
- str.decode(encoding="utf-8"[, errors]) -> unicode [only in Python 2.x]
- str.encode(encoding="utf-8", errors="strict") -> bytes
- str.endswith(suffix[, start[, end]]) -> bool
- str.expandtabs(tabsize=8) -> str
- str.find(sub[, start[, end]]) -> int
- str.format(*args, **kwargs) -> str
- str.format_map(mapping) -> str
- str.index(sub[, start[, end]]) -> int
- str.isalnum() -> bool
- str.isalpha() -> bool
- str.isdecimal() -> bool
- str.isdigit() -> bool
- str.isidentifier() -> bool
- str.islower() -> bool
- str.isnumeric() -> bool
- str.isprintable() -> bool
- str.isspace() -> bool
- str.istitle() -> bool
- str.isupper() -> bool
- str.join(iterable) -> str
- str.ljust(width[, fillchar]) -> str
- str.lower() -> str
- str.lstrip([chars]) -> str
- static str.maketrans(x[, y[, z]])
- str.partition(sep) -> (head, sep, tail)
- str.replace(old, new[, count]) -> str
- str.rfind(sub[, start[, end]]) -> int
- str.rindex(sub[, start[, end]]) -> int
- str.rjust(width[, fillchar]) -> str
- str.rpartition(sep) -> (head, sep, tail)
- str.rsplit(sep=None, maxsplit=-1) -> list of strings
- str.rstrip([chars]) -> str
- str.split(sep=None, maxsplit=-1) -> list of strings
- str.splitlines([keepends]) -> list of strings
- str.startswith(prefix[, start[, end]]) -> book
- str.strip([chars]) -> str
- str.swapcase() -> str
- str.title() -> str
- str.translate(table) -> str
- str.upper() -> str
- str.zfill(width) -> str



#### Remarks


String objects are immutable, meaning that they can't be modified in place the way a list can. Because of this, methods on the built-in type `str` always return a **new** `str` object, which contains the result of the method call.

