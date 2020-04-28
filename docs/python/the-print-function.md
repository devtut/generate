---
metaTitle: "The Print Function"
description: "Print basics, Print parameters"
---

# The Print Function




## Print basics


In Python 3 and higher, `print` is a function rather than a keyword.

```py
print('hello world!')
# out: hello world!

foo = 1
bar = 'bar'
baz = 3.14

print(foo)    
# out: 1
print(bar)    
# out: bar
print(baz)
# out: 3.14

```

You can also pass a number of parameters to `print`:

```py
print(foo, bar, baz)
# out: 1 bar 3.14

```

Another way to `print` multiple parameters is by using a `+`

```py
print(str(foo) + " " + bar + " " + str(baz))
# out: 1 bar 3.14

```

What you should be careful about when using `+` to print multiple parameters, though, is that the type of the parameters should be the same. Trying to print the above example without the cast to `string` first would result in an error, because it would try to add the number `1` to the string `"bar"` and add that to the number `3.14`.

```py
# Wrong:
# type:int  str  float
print(foo + bar + baz)
# will result in an error

```

This is because the content of `print` will be evaluated first:

```py
print(4 + 5)
# out: 9
print("4" + "5")
# out: 45
print([4] + [5])
# out: [4, 5]

```

Otherwise, using a `+` can be very helpful for a user to read output of variables
In the example below the output is very easy to read!

The script below demonstrates this

```py
import random 
#telling python to include a function to create random numbers
randnum = random.randint(0, 12) 
#make a random number between 0 and 12 and assign it to a variable
print("The randomly generated number was - " + str(randnum))

```

You can prevent the `print` function from automatically printing a newline by using the `end` parameter:

```py
print("this has no newline at the end of it... ", end="")
print("see?")
# out: this has no newline at the end of it... see?

```

If you want to write to a file, you can pass it as the parameter `file`:

```py
with open('my_file.txt', 'w+') as my_file:
    print("this goes to the file!", file=my_file)

```

> 
this goes to the file!




## Print parameters


You can do more than just print text. `print` also has several parameters to help you.

Argument `sep`: place a string between arguments.

Do you need to print a list of words separated by a comma or some other string?

```py
>>> print('apples','bannas', 'cherries', sep=', ')
apple, bannas, cherries
>>> print('apple','banna', 'cherries', sep=', ')
apple, banna, cherries
>>>

```

Argument `end`: use something other than a newline at the end

Without the `end` argument, all `print()` functions write a line and then go to the beginning of the next line.  You can change it to do nothing (use an empty string of ''), or double spacing between paragraphs by using two newlines.

```py
>>> print("<a", end=''); print(" class='jidn'" if 1 else "", end=''); print("/>")
<a class='jidn'/>
>>> print("paragraph1", end="\n\n"); print("paragraph2")
paragraph1

paragraph2
>>>

```

Argument `file`: send output to someplace other than sys.stdout.

Now you can send your text to either stdout, a file, or StringIO and not care which you are given.  If it quacks like a file, it works like a file.

```py
>>> def sendit(out, *values, sep=' ', end='\n'):
...     print(*values, sep=sep, end=end, file=out)
... 
>>> sendit(sys.stdout, 'apples', 'bannas', 'cherries', sep='\t')
apples    bannas    cherries
>>> with open("delete-me.txt", "w+") as f:
...    sendit(f, 'apples', 'bannas', 'cherries', sep=' ', end='\n')
... 
>>> with open("delete-me.txt", "rt") as f:
...     print(f.read())
... 
apples bannas cherries

>>>

```

There is a fourth parameter `flush` which will forcibly flush the stream.

