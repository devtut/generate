---
metaTitle: "References"
description: "Assign by Reference, Return by Reference, Pass by Reference"
---

# References



## Assign by Reference


This is the first phase of referencing. Essentially when you [assign by reference](http://php.net/manual/en/language.references.whatdo.php#language.references.whatdo.assign), you're allowing two variables to share the same value as such.

```
$foo = &$bar;

```

`$foo` and `$bar` are equal here. They **do not** point to one another. They point to the same place (**the "value"**).

You can also assign by reference within the **`array()`** language construct. While not strictly being an assignment by reference.

```
$foo = 'hi';
$bar = array(1, 2);
$array = array(&$foo, &$bar[0]);

```

> 
<p>**Note**, however, that references inside arrays are potentially
dangerous. Doing a normal (not by reference) assignment with a
reference on the right side does not turn the left side into a
reference, but references inside arrays are preserved in these normal
assignments. This also applies to function calls where the array is
passed by value.</p>


Assigning by reference is not only limited to variables and arrays, they are also present for functions and all "pass-by-reference" associations.

```
function incrementArray(&$arr) {
    foreach ($arr as &$val) {
        $val++;
    }
}

function &getArray() {
    static $arr = [1, 2, 3];
    return $arr;
}

incrementArray(getArray());
var_dump(getArray()); // prints an array [2, 3, 4]

```

Assignment is key within the function definition as above. You **can not** pass an expression by reference, only a value/variable. Hence the instantiation of `$a` in `bar()`.



## Return by Reference


Occasionally there comes time for you to implicitly return-by-reference.

> 
<p>Returning by reference is useful when you want to use a function to
find to which variable a reference should be bound. Do not use
return-by-reference to increase performance. The engine will
automatically optimize this on its own. Only return references when
you have a valid technical reason to do so.</p>


<sup>**Taken from the [PHP Documentation for Returning By Reference](http://php.net/manual/en/language.references.return.php).**</sup>

There are many different forms return by reference can take, including the following example:

```
function parent(&$var) {
    echo $var;
    $var = "updated";
}

function &child() {
    static $a = "test";
    return $a;
}

parent(child()); // returns "test"
parent(child()); // returns "updated"

```

Return by reference is not only limited to function references. You also have the ability to implicitly call the function:

```
function &myFunction() {
    static $a = 'foo';
    return $a;
}

$bar = &myFunction();
$bar = "updated"
echo myFunction();

```

You cannot directly **reference** a function call, it has to be assigned to a variable before harnessing it. To see how that works, simply try `echo &myFunction();`.

### Notes

- You are required to specify a reference (`&`) in both places you intend on using it. That means, for your function definition (`function &myFunction() {...`) and in the calling reference (`function callFunction(&$variable) {...` or `&myFunction();`).
- You can only return a variable by reference. Hence the instantiation of `$a` in the example above. This means you can not return an expression, otherwise an **`E_NOTICE`** PHP error will be generated (**`Notice: Only variable references should be returned by reference in ......`**).
- Return by reference does have legitimate use cases, but I should warn that they should be used sparingly, only after exploring all other potential options of achieving the same goal.



## Pass by Reference


This allows you to pass a variable by reference to a function or element that allows you to modify the original variable.

Passing-by-reference is not limited to variables only, the following can also be passed by reference:

- New statements, e.g. `foo(new SomeClass)`
- References returned from functions

### Arrays

A common use of ["passing-by-reference"](http://php.net/manual/en/language.references.pass.php) is to modify initial values within an array without going to the extent of creating new arrays or littering your namespace. Passing-by-reference is as simple as preceding/prefixing the variable with an `&` => `&$myElement`.

Below is an example of harnessing an element from an array and simply adding 1 to its initial value.

```
$arr = array(1, 2, 3, 4, 5);

foreach($arr as &$num) {
    $num++;
}

```

Now when you harness any element within `$arr`, the original element will be updated as the reference was increased. You can verify this by:

```
print_r($arr);

```

> 
**Note**
You should take note when harnessing pass by reference within loops. At the end of the above loop, `$num` still holds a reference to the last element of the array. Assigning it post loop will end up manipulating the last array element! You can ensure this doesn't happen by `unset()`'ing it post-loop:
<pre><code>$myArray = array(1, 2, 3, 4, 5);

foreach($myArray as &$num) {
   $num++;
}
unset($num);
</code></pre>
The above will ensure you don't run into any issues. An example of issues that could relate from this is present in [this question on StackOverflow](http://stackoverflow.com/q/24902742/2518525).


### Functions

Another common usage for passing-by-reference is within functions. Modifying the original variable is as simple as:

```
$var = 5;
// define
function add(&$var) {
    $var++;
}
// call
add($var);

```

Which can be verified by `echo`'ing the original variable.

```
echo $var;

```

There are various restrictions around functions, as noted below from the PHP docs:

> 
<p>**Note:** There is no reference sign on a function call - only on function
definitions. Function definitions alone are enough to correctly pass
the argument by reference. As of PHP 5.3.0, you will get a warning
saying that "call-time pass-by-reference" is deprecated when you use &
in foo(&$a);. And as of PHP 5.4.0, call-time pass-by-reference was
removed, so using it will raise a fatal error.</p>




#### Syntax


- `$foo = 1; $bar = &$foo; // both $foo and $bar point to the same value: 1`
- `$var = 1; function calc(&$var) { $var *= 15; } calc($var); echo $var;`



#### Remarks


While assigning two variables by reference, both variables point to the same value. Take the following example:

```
$foo = 1;
$bar = &$foo;

```

`$foo` ****does not**** point to `$bar`. `$foo` and `$bar` both point to the same value of `$foo`, which is `1`. To illustrate:

```
$baz = &$bar;
unset($bar);
$baz++;

```

If we had a `points to` relationship, this would be broken now after the `unset()`; instead, `$foo` and `$baz` still point to the same value, which is `2`.

