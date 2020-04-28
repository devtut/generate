---
metaTitle: "Outputting the Value of a Variable"
description: "echo and print, Outputting a structured view of arrays and objects, String concatenation with echo, printf vs sprintf, Outputting large integers, Output a Multidimensional Array with index and value and print into the table"
---

# Outputting the Value of a Variable


To build a dynamic and interactive PHP program, it is useful to output variables and their values. The PHP language allows for multiple methods of value output. This topic covers the standard methods of printing a value in PHP and where these methods can be used.



## echo and print


[`echo`](http://php.net/manual/en/function.echo.php) and [`print`](http://php.net/manual/en/function.print.php) are language constructs, not functions. This means that they don't require parentheses around the argument like a function does (although one can always add parentheses around almost any PHP expression and thus `echo("test")` won't do any harm either). They output the string representation of a variable, constant, or expression. They can't be used to print arrays or objects.

<li>
Assign the string `Joel` to the variable `$name`
<pre><code>$name = "Joel";
</code></pre>
</li>
<li>
Output the value of $name using `echo` & `print`
<pre><code>echo $name;   #> Joel
print $name;  #> Joel
</code></pre>
</li>
<li>
Parentheses are not required, but can be used
<pre><code>echo($name);  #> Joel
print($name); #> Joel
</code></pre>
</li>
<li>
Using multiple parameters (only `echo`)
<pre><code>echo $name, "Smith";       #> JoelSmith
echo($name, " ", "Smith"); #> Joel Smith
</code></pre>
</li>

<li>
`print`, unlike `echo`, is an expression (it returns `1`),  and thus can be used in more places:
<pre><code>print("hey") && print(" ") && print("you"); #> you11
</code></pre>
</li>
<li>
The above is equivalent to:
<pre><code>print ("hey" && (print (" " && print "you"))); #> you11
</code></pre>
</li>

### Shorthand notation for `echo`

When [outside of PHP tags](http://php.net/manual/en/language.basic-syntax.phpmode.php), a shorthand notation for `echo` is available by default, using `<?=` to begin output and `?>` to end it. For example:

```
<p><?=$variable?></p>    
<p><?= "This is also PHP" ?></p>

```

Note that there is no terminating `;`. This works because the closing PHP tag acts as the terminator for the single statement. So, it is conventional to omit the semicolon in this shorthand notation.

### **Priority of `print`**

Although the `print` is language construction it has priority like operator. It places between `=` `+=` `-=` `*=` `**=` `/=` `.=` `%=` `&=` and `and` operators and has left association.
Example:

```
echo '1' . print '2' + 3; //output 511

```

Same example with brackets:

```
echo '1' . print ('2' + 3); //output 511

```

### **Differences between `echo` and `print`**

In short, there are two main differences:

- `print` only takes one parameter, while `echo` can have multiple parameters.
- `print` returns a value, so can be used as an expression.



## Outputting a structured view of arrays and objects


### [`print_r()`](http://php.net/manual/en/function.print-r.php) - Outputting **Arrays** and **Objects** **for debugging**

[`print_r`](http://php.net/manual/en/function.print-r.php) will output a human readable format of an array or object.

You may have a variable that is an array or object. Trying to output it with an `echo` will throw the error:<br />
`Notice: Array to string conversion`. You can instead use the `print_r` function to dump a human readable format of this variable.

> 
You can pass **true** as the second parameter to return the content as a string.


```
$myobject = new stdClass();
$myobject->myvalue = 'Hello World';
$myarray = [ "Hello", "World" ];
$mystring = "Hello World";
$myint = 42;

// Using print_r we can view the data the array holds.
print_r($myobject);
print_r($myarray);
print_r($mystring);
print_r($myint);

```

This outputs the following:

```
stdClass Object
(
    [myvalue] => Hello World
)
Array
(
    [0] => Hello
    [1] => World
)
Hello World
42

```

Further, the output from `print_r` can be captured as a string, rather than simply echoed. For instance, the following code will dump the formatted version of `$myarray` into a new variable:

```
$formatted_array = print_r($myarray, true);

```

Note that if you are viewing the output of PHP in a browser, and it is interpreted as HTML, then the line breaks will not be shown and the output will be much less legible unless you do something like

```
echo '<pre>' . print_r($myarray, true) . '</pre>';

```

> 
Opening the source code of a page will also format your variable in the same way without the use of the `<pre>` tag.


Alternatively you can tell the browser that what you're outputting is plain text, and not HTML:

```
header('Content-Type: text/plain; charset=utf-8');
print_r($myarray);

```

### [`var_dump()`](http://php.net/manual/en/function.var-dump.php) - Output human-readable debugging information about content of the argument(s) including its type and value

The output is more detailed as [compared](http://stackoverflow.com/questions/3406171/php-var-dump-vs-print-r/3406224#3406224) to `print_r` because it also outputs the **type** of the variable along with its **value** and other information like object IDs, array sizes, string lengths, reference markers, etc.

You can use [`var_dump`](http://php.net/manual/en/function.var-dump.php) to output a more detailed version for debugging.

```
var_dump($myobject, $myarray, $mystring, $myint);

```

Output is more detailed:

```
object(stdClass)#12 (1) {
  ["myvalue"]=>
  string(11) "Hello World"
}
array(2) {
  [0]=>
  string(5) "Hello"
  [1]=>
  string(5) "World"
}
string(11) "Hello World"
int(42)

```

**Note**: If you are using xDebug in your development environment, the output of var_dump is limited / truncated by default. See the [official documentation](https://xdebug.org/docs/display) for more info about the options to change this.

### [`var_export()`](http://php.net/manual/en/function.var-export.php) - Output valid `PHP` Code

[`var_export()`](http://php.net/manual/en/function.var-export.php) dumps a `PHP` parseable representation of the item.

> 
You can pass **true** as the second parameter to return the contents into a variable.


```
var_export($myarray);
var_export($mystring);
var_export($myint);

```

Output is valid PHP code:

```
array (
  0 => 'Hello',
  1 => 'World',
)
'Hello World'
42

```

To put the content into a variable, you can do this:

```
$array_export = var_export($myarray, true);
$string_export = var_export($mystring, true);
$int_export = var_export($myint, 1); // any `Truthy` value

```

After that, you can output it like this:

```
printf('$myarray = %s; %s', $array_export, PHP_EOL);
printf('$mystring = %s; %s', $string_export, PHP_EOL);
printf('$myint = %s; %s', $int_export, PHP_EOL);

```

This will produce the following output:

```
$myarray = array (
  0 => 'Hello',
  1 => 'World',
);
$mystring = 'Hello World';
$myint = 42;

```



## String concatenation with echo


You can use [concatenation to join strings](https://secure.php.net/manual/en/language.operators.string.php) "end to end" while outputting them (with `echo` or `print` for example).

You can concatenate variables using a `.` (period/dot).

```
// String variable
$name = 'Joel';

// Concatenate multiple strings (3 in this example) into one and echo it once done.
//      1. ↓        2. ↓            3. ↓    - Three Individual string items
echo '<p>Hello ' . $name . ', Nice to see you.</p>';
//               ↑       ↑                  - Concatenation Operators

#> "<p>Hello Joel, Nice to see you.</p>"

```

Similar to concatenation, `echo` (when used without parentheses) can be used to combine strings and variables together (along with other arbitrary expressions) using a comma (,).

```
$itemCount = 1;

echo 'You have ordered ', $itemCount, ' item', $itemCount === 1 ? '' : 's';
//                      ↑           ↑        ↑                - Note the commas

#> "You have ordered 1 item"

```

### String concatenation vs passing multiple arguments to echo

Passing multiple arguments to the echo command is more advantageous than string concatenation in some circumstances. The arguments are written to the output in the same order as they are passed in.

```
echo "The total is: ", $x + $y;

```

The problem with the concatenation is that the period `.` takes precedence in the expression. If concatenated, the above expression needs extra parentheses for the correct behavior. The precedence of the period affects ternary operators too.

```
echo "The total is: " . ($x + $y);

```



## printf vs sprintf


[`printf`](http://php.net/manual/en/function.printf.php) will **output** a formatted string using placeholders

[`sprintf`](http://php.net/manual/en/function.sprintf.php) will **return** the formatted string

```
$name = 'Jeff';

// The `%s` tells PHP to expect a string
//            ↓  `%s` is replaced by  ↓
printf("Hello %s, How's it going?", $name);
#> Hello Jeff, How's it going?

// Instead of outputting it directly, place it into a variable ($greeting)
$greeting = sprintf("Hello %s, How's it going?", $name);
echo $greeting;
#> Hello Jeff, How's it going?

```

It is also possible to format a number with these 2 functions. This can be used to format a decimal value used to represent money so that it always has 2 decimal digits.

```
$money = 25.2;
printf('%01.2f', $money);
#> 25.20

```

The two functions [`vprintf`](http://php.net/manual/en/function.vprintf.php) and [`vsprintf`](http://php.net/manual/en/function.vsprintf.php) operate as [`printf`](http://php.net/manual/en/function.printf.php) and [`sprintf`](http://php.net/manual/en/function.sprintf.php), but accept a format string and an array of values, instead of individual variables.



## Outputting large integers


On 32-bits systems, integers larger than `PHP_INT_MAX` are automatically converted to float. Outputting these as integer values (i.e. non-scientific notation) can be done with `printf`, using the `float` representation, as illustrated below:

```
foreach ([1, 2, 3, 4, 5, 6, 9, 12] as $p) {
    $i = pow(1024, $p);
    printf("pow(1024, %d) > (%7s) %20s %38.0F", $p, gettype($i), $i, $i);
    echo "  ", $i, "\n";
}
// outputs:
pow(1024,  1)  integer                 1024                                   1024  1024
pow(1024,  2)  integer              1048576                                1048576  1048576
pow(1024,  3)  integer           1073741824                             1073741824  1073741824
pow(1024,  4)   double        1099511627776                          1099511627776  1099511627776
pow(1024,  5)   double  1.1258999068426E+15                       1125899906842624  1.1258999068426E+15
pow(1024,  6)   double  1.1529215046068E+18                    1152921504606846976  1.1529215046068E+18
pow(1024,  9)   double  1.2379400392854E+27           1237940039285380274899124224  1.2379400392854E+27
pow(1024, 12)   double  1.3292279957849E+36  1329227995784915872903807060280344576  1.3292279957849E+36

```

> 
Note: watch out for float precision, which is not infinite!


While this looks nice, in this contrived example the numbers can all be represented as a binary number since they are all powers of 1024 (and thus 2). See for example:

```
$n = pow(10, 27);
printf("%s %.0F\n", $n, $n);
// 1.0E+27 1000000000000000013287555072

```



## Output a Multidimensional Array with index and value and print into the table


```
Array 
( 
    [0] => Array 
        ( 
            [id] => 13 
            [category_id] => 7 
            [name] => Leaving Of Liverpool 
            [description] => Leaving Of Liverpool 
            [price] => 1.00 
            [virtual] => 1 
            [active] => 1 
            [sort_order] => 13 
            [created] => 2007-06-24 14:08:03 
            [modified] => 2007-06-24 14:08:03 
            [image] => NONE 
        ) 

    [1] => Array 
        ( 
            [id] => 16 
            [category_id] => 7 
            [name] => Yellow Submarine 
            [description] => Yellow Submarine 
            [price] => 1.00 
            [virtual] => 1 
            [active] => 1 
            [sort_order] => 16 
            [created] => 2007-06-24 14:10:02 
            [modified] => 2007-06-24 14:10:02 
            [image] => NONE 
        ) 

)  

```

Output Multidimensional Array with index and value in table

```
<table>
<?php 
foreach ($products as $key => $value) {
    foreach ($value as $k => $v) {
        echo "<tr>";
        echo "<td>$k</td>"; // Get index.
        echo "<td>$v</td>"; // Get value.
        echo "</tr>";
    }
}
?>
</table>

```



#### Remarks


Variables in PHP come in a variety of types. Depending on the use case, you may want to output them to the browser as rendered HTML, output them for debugging, or output them to the terminal (if running an application via the command line).

Below are some of the most commonly used methods and language constructs to output variables:

- [`echo`](http://php.net/manual/en/function.echo.php) - Outputs one or more strings
- [`print`](http://php.net/manual/en/function.print.php) - Outputs a string and returns `1` (always)
- [`printf`](http://php.net/manual/en/function.printf.php) - Outputs a formatted string and returns the length of the outputted string
- [`sprintf`](http://php.net/manual/en/function.sprintf.php) - Formats a string and returns the formatted string
- [`print_r`](http://php.net/manual/en/function.print-r.php) - Outputs or returns content of the argument as a human-readable string
- [`var_dump`](http://php.net/manual/en/function.var-dump.php) - Outputs human-readable debugging information about the content of the argument(s) including its type and value
- [`var_export`](http://php.net/manual/en/function.var-export.php) - Outputs or returns a string rendering of the variable as valid PHP code, which can be used to recreate the value.

> 
**Note:** When trying to output an object as a string, PHP will try to convert it into a string (by calling  [`__toString()`](http://www.php.net/manual/en/language.oop5.magic.php#language.oop5.magic.tostring) - if the object has such a method). If unavailable, an error similar to `Object of class [CLASS] could not be converted to string` will be shown. In this case, you'll have to inspect the object further, see: [outputting-a-structured-view-of-arrays-and-objects](http://stackoverflow.com/documentation/php/194/variables/772/outputting-a-structured-view-of-arrays-and-objects-for-debugging).


