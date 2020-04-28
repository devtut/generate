---
metaTitle: "Type juggling and Non-Strict Comparison Issues"
description: "What is Type Juggling?, Reading from a file, Switch surprises, Strict typing"
---

# Type juggling and Non-Strict Comparison Issues



## What is Type Juggling?


PHP is a loosely typed language.  This means that, by default, it doesn't require operands in an expression to be of the same (or compatible) types.  For example, you can append a number to a string and expect it to work.

```
var_dump ("This is example number " . 1);

```

The output will be:

> 
string(24) "This is example number 1"


PHP accomplishes this by automatically casting incompatible variable types into types that allow the requested operation to take place.  In the case above, it will cast the integer literal 1 into a string, meaning that it can be concatenated onto the preceding string literal.  This is referred to as type juggling.  This is a very powerful feature of PHP, but it is also a feature that can lead you to a lot of hair-pulling if you are not aware of it, and can even lead to security problems.

Consider the following:

```
if (1 == $variable) {
    // do something
}

```

The intent appears to be that the programmer is checking that a variable has a value of 1.  But what happens if $variable has a value of "1 and a half" instead?  The answer might surprise you.

```
$variable = "1 and a half";
var_dump (1 == $variable);

```

The result is:

> 
bool(true)


Why has this happened?  It's because PHP realised that the string "1 and a half" isn't an integer, but it needs to be in order to compare it to integer 1.  Instead of failing, PHP initiates type juggling and, attempts to convert the variable into an integer.  It does this by taking all the characters at the start of the string that can be cast to integer and casting them.  It stops as soon as it encounters a character that can't be treated as a number.  Therefore "1 and a half" gets cast to integer 1.

Granted, this is a very contrived example, but it serves to demonstrate the issue.  The next few examples will cover some cases where I've run into errors caused by type juggling that happened in real software.



## Reading from a file


When reading from a file, we want to be able to know when we've reached the end of that file. Knowing that `fgets()` returns false at the end of the file, we might use this as the condition for a loop. However, if the data returned from the last read happens to be something that evaluates as boolean `false`, it can cause our file read loop to terminate prematurely.

```
$handle = fopen ("/path/to/my/file", "r");

if ($handle === false) {
    throw new Exception ("Failed to open file for reading");
}

while ($data = fgets($handle)) {
    echo ("Current file line is $data\n");
}

fclose ($handle);

```

If the file being read contains a blank line, the `while` loop will be terminated at that point, because the empty string evaluates as boolean `false`.

Instead, we can check for the boolean `false` value explicitly, using [strict equality operators](http://stackoverflow.com/documentation/php/1687/operators/6231/comparison-operators):

```
while (($data = fgets($handle)) !== false) {
    echo ("Current file line is $data\n");
}

```

Note this is a contrived example; in real life we would use the following loop:

```
while (!feof($handle)) {
    $data = fgets($handle);
    echo ("Current file line is $data\n");
}

```

Or replace the whole thing with:

```
$filedata = file("/path/to/my/file");
foreach ($filedata as $data) {
    echo ("Current file line is $data\n");
}

```



## Switch surprises


Switch statements use non-strict comparison to determine matches.  This can lead to some [nasty surprises](http://stackoverflow.com/questions/4098104/odd-behaviour-in-a-switch-statement).  For example, consider the following statement:

```
switch ($name) {
    case 'input 1':
        $mode = 'output_1';
        break;
    case 'input 2':
        $mode = 'output_2';
        break;
    default:
        $mode = 'unknown';
        break;
}

```

This is a very simple statement, and works as expected when `$name` is a string, but can cause problems otherwise.  For example, if `$name` is integer `0`, then type-juggling will happen during the comparison. However, it's the literal value in the case statement that gets juggled, not the condition in the switch statement. The string `"input 1"` is converted to integer `0` which matches the input value of integer `0`. The upshot of this is if you provide a value of integer `0`, the first case always executes.

There are a few solutions to this problem:

### Explicit casting

The value can be [typecast](http://stackoverflow.com/documentation/php/232/types/3880/type-casting) to a string before comparison:

```
switch ((string)$name) {
...
}

```

Or a function known to return a string can also be used:

```
switch (strval($name)) {
...
}

```

Both of these methods ensure the value is of the same type as the value in the `case` statements.

### Avoid `switch`

Using an `if` statement will provide us with control over how the comparison is done, allowing us to use [strict comparison operators](http://stackoverflow.com/documentation/php/1687/operators/6231/comparison-operators):

```
if ($name === "input 1") {
    $mode = "output_1";
} elseif ($name === "input 2") {
    $mode = "output_2";
} else {
    $mode = "unknown";
}

```



## Strict typing


Since PHP 7.0, some of the harmful effects of type juggling can be mitigated with [strict typing](http://php.net/manual/en/functions.arguments.php#functions.arguments.type-declaration.strict). By including this `declare` statement as the first line of the file, PHP will enforce parameter type declarations and return type declarations by throwing a `TypeError` exception.

```
declare(strict_types=1);

```

For example, this code, using parameter type definitions, will throw a catchable exception of type `TypeError` when run:

```
<?php
declare(strict_types=1);

function sum(int $a, int $b) {
    return $a + $b;
}

echo sum("1", 2);

```

Likewise, this code uses a return type declaration; it will also throw an exception if it tries to return anything other than an integer:

```
<?php
declare(strict_types=1);

function returner($a): int {
    return $a;
}

returner("this is a string");

```

