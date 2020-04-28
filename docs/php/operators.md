---
metaTitle: "Operators"
description: "Null Coalescing Operator (??), Spaceship Operator (<=>), Ternary Operator (?:), Incrementing (++) and Decrementing Operators (--), Execution Operator (``), Logical Operators (&&/AND and ||/OR), String Operators (. and .=), Object and Class Operators, Combined Assignment (+= etc), Altering operator precedence (with parentheses), Basic Assignment (=), Association, Comparison Operators, instanceof (type operator), Bitwise Operators"
---

# Operators


An operator is something that takes one or more values (or expressions, in programming jargon) and yields another value (so that the construction itself becomes an expression).

Operators can be grouped according to the number of values they take.



## Null Coalescing Operator (??)


Null coalescing is a new operator introduced in PHP 7. This operator returns its first operand if it is set and not `NULL`. Otherwise it will return its second operand.

The following example:

```php
$name = $_POST['name'] ?? 'nobody';

```

is equivalent to both:

```php
if (isset($_POST['name'])) {
    $name = $_POST['name'];
} else {
    $name = 'nobody';
}

```

and:

```php
$name = isset($_POST['name']) ? $_POST['name'] : 'nobody'; 

```

This operator can also be chained (with right-associative semantics):

```php
$name = $_GET['name'] ?? $_POST['name'] ?? 'nobody';

```

which is an equivalent to:

```php
if (isset($_GET['name'])) {
    $name = $_GET['name'];
} elseif (isset($_POST['name'])) {
    $name = $_POST['name'];
} else {
    $name = 'nobody';
}

```

Note:<br>
When using coalescing operator on string concatenation dont forget to use parentheses `()`
<br>

```php
$firstName = "John";
$lastName = "Doe";
echo $firstName ?? "Unknown" . " " . $lastName ?? "";

```

This will output `John` only, and if its $firstName is null and $lastName is `Doe` it will output `Unknown Doe`. In order to output `John Doe`, we must use parentheses like this.<br>

```php
$firstName = "John";
$lastName = "Doe";
echo ($firstName ?? "Unknown") . " " . ($lastName ?? "");

```

This will output `John Doe` instead of `John` only.



## Spaceship Operator (<=>)


PHP 7 introduces a new kind of operator, which can be used to compare expressions. This operator will return -1, 0 or 1 if the first expression is less than, equal to, or greater than the second expression.

```php
// Integers
print (1 <=> 1); // 0
print (1 <=> 2); // -1
print (2 <=> 1); // 1

// Floats
print (1.5 <=> 1.5); // 0
print (1.5 <=> 2.5); // -1
print (2.5 <=> 1.5); // 1
 
// Strings
print ("a" <=> "a"); // 0
print ("a" <=> "b"); // -1
print ("b" <=> "a"); // 1

```

Objects are not comparable, and so doing so will result in undefined behaviour.

This operator is particularly useful when writing a user-defined comparison function using `usort`, `uasort`, or `uksort`. Given an array of objects to be sorted by their `weight` property, for example, an anonymous function can use `<=>` to return the value expected by the sorting functions.

```php
usort($list, function($a, $b) { return $a->weight <=> $b->weight; });

```

In PHP 5 this would have required a rather more elaborate expression.

```php
usort($list, function($a, $b) {
    return $a->weight < $b->weight ? -1 : ($a->weight == $b->weight ? 0 : 1);
});

```



## Ternary Operator (?:)


The ternary operator can be thought of as an inline `if` statement. It consists of three parts. The `operator`, and two outcomes. The syntax is as follows:

```php
$value = <operator> ? <true value> : <false value>

```

If the `operator` is evaluated as `true`, the value in the first block will be returned (`<true value>`), else the value in the second block will be returned (`<false value>`). Since we are setting `$value` to the result of our ternary operator it will store the returned value.

Example:

```php
$action = empty($_POST['action']) ? 'default' : $_POST['action'];

```

`$action` would contain the string `'default'` if `empty($_POST['action'])` evaluates to true. Otherwise it would contain the value of `$_POST['action']`.

The expression `(expr1) ? (expr2) : (expr3)` evaluates to `expr2` if `expr1`evaluates to `true`, and `expr3` if `expr1` evaluates to `false`.

It is possible to leave out the middle part of the ternary operator. Expression `expr1 ?: expr3` returns `expr1` if `expr1` evaluates to TRUE, and `expr3` otherwise. `?:` is often referred to as **Elvis** operator.

This behaves like the [Null Coalescing operator `??`](http://stackoverflow.com/documentation/php/1687/operators/7164/null-coalescing-operator), except that `??` requires the left operand to be exactly `null` while `?:` tries to resolve the left operand into a boolean and check if it resolves to boolean `false`.

Example:

```php
function setWidth(int $width = 0){
    $_SESSION["width"] = $width ?: getDefaultWidth();
}

```

In this example, `setWidth` accepts a width parameter, or default 0, to change the width session value. If `$width` is 0 (if `$width` is not provided), which will resolve to boolean false, the value of `getDefaultWidth()` is used instead. The `getDefaultWidth()` function will not be called if `$width` did not resolve to boolean false.

Refer to [Types](http://stackoverflow.com/documentation/php/232/php-types) for more information about conversion of variables to boolean.



## Incrementing (++) and Decrementing Operators (--)


Variables can be incremented or decremented by 1 with `++` or `--`, respectively. They can either precede or succeed variables and slightly vary semantically, as shown below.

```php
$i = 1;
echo $i; // Prints 1

// Pre-increment operator increments $i by one, then returns $i
echo ++$i; // Prints 2

// Pre-decrement operator decrements $i by one, then returns $i
echo --$i; // Prints 1

// Post-increment operator returns $i, then increments $i by one
echo $i++; // Prints 1 (but $i value is now 2)

// Post-decrement operator returns $i, then decrements $i by one
echo $i--; // Prints 2 (but $i value is now 1)

```

More information about incrementing and decrementing operators can be found in the [official documentation](http://php.net/manual/en/language.operators.increment.php).



## Execution Operator (``)


The PHP execution operator consists of backticks (``) and is used to run shell commands. The output of the command will be returned, and may, therefore, be stored in a variable.

```php
// List files
$output = `ls`;
echo "<pre>$output</pre>";

```

Note that the execute operator and [`shell_exec()`](http://php.net/manual/en/function.shell-exec.php) will give the same result.



## Logical Operators (&&/AND and ||/OR)


In PHP, there are two versions of logical AND and OR operators.

|Operator|True if
|------
|`$a and $b`|Both `$a` and `$b` are true
|`$a && $b`|Both `$a` and `$b` are true
|`$a or $b`|Either `$a` or `$b` is true
|`$a || $b`|Either `$a` or `$b` is true

Note that the `&&` and `||` opererators have higher [precedence](http://php.net/manual/en/language.operators.precedence.php) than `and` and `or`. See table below:

|Evaluation|Result of `$e`|Evaluated as
|------
|`$e = false || true`|True|`$e = (false || true)`
|`$e = false or true`|False|`($e = false) or true`

Because of this it's safer to use `&&` and `||` instead of `and` and `or`.



## String Operators (. and .=)


There are only two string operators:

<li>
Concatenation of two strings (dot):

```php
$a = "a";
$b = "b";
$c = $a . $b; // $c => "ab"

```


</li>
<li>
Concatenating assignment (dot=):

```php
$a = "a";
$a .= "b"; // $a => "ab"

```


</li>



## Object and Class Operators


Members of objects or classes can be accessed using the object operator (`->`) and the class operator (`::`).

```php
class MyClass {
    public $a = 1;
    public static $b = 2;
    const C = 3;
    public function d() { return 4; }
    public static function e() { return 5; }
}

$object = new MyClass();
var_dump($object->a);   // int(1)
var_dump($object::$b);  // int(2)
var_dump($object::C);   // int(3)
var_dump(MyClass::$b);  // int(2)
var_dump(MyClass::C);   // int(3)
var_dump($object->d()); // int(4)
var_dump($object::d()); // int(4)
var_dump(MyClass::e()); // int(5)
$classname = "MyClass";
var_dump($classname::e()); // also works! int(5)

```

Note that after the object operator, the `$` should not be written (`$object->a` instead of `$object->$a`). For the class operator, this is not the case and the `$` is necessary. For a constant defined in the class, the `$` is never used.

Also note that `var_dump(MyClass::d());` is only allowed if the function `d()` does **not** reference the object:

```php
class MyClass {
    private $a = 1;
    public function d() {
        return $this->a;
    }
}

$object = new MyClass();
var_dump(MyClass::d());   // Error!

```

This causes a 'PHP Fatal error: Uncaught Error: Using $this when not in object context'

These operators have **left** associativity, which can be used for 'chaining':

```php
class MyClass {
    private $a = 1;
    
    public function add(int $a) {
        $this->a += $a;
        return $this;
    }
    
    public function get() {
        return $this->a;
    }
}

$object = new MyClass();
var_dump($object->add(4)->get());  // int(5)

```

These operators have the highest precedence (they are not even mentioned in the manual), even higher that `clone`. Thus:

```php
class MyClass {
    private $a = 0;
    public function add(int $a) {
        $this->a += $a;
        return $this;
    }
    public function get() {
        return $this->a;
    }
}

$o1 = new MyClass();
$o2 = clone $o1->add(2);
var_dump($o1->get()); // int(2)
var_dump($o2->get()); // int(2)

```

The value of `$o1` is added to **before** the object is cloned!

Note that using parentheses to influence precedence did not work in PHP version 5 and older (it does in PHP 7):

```php
// using the class MyClass from the previous code
$o1 = new MyClass();
$o2 = (clone $o1)->add(2);  // Error in PHP 5 and before, fine in PHP 7
var_dump($o1->get()); // int(0) in PHP 7
var_dump($o2->get()); // int(2) in PHP 7

```



## Combined Assignment (+= etc)


The combined assignment operators are a shortcut for an operation on some variable and subsequently assigning this new value to that variable.

Arithmetic:

```php
$a = 1;   // basic assignment
$a += 2; // read as '$a = $a + 2'; $a now is (1 + 2) => 3
$a -= 1; // $a now is (3 - 1) => 2
$a *= 2; // $a now is (2 * 2) => 4
$a /= 2; // $a now is (16 / 2) => 8
$a %= 5; // $a now is (8 % 5) => 3 (modulus or remainder)

// array +
$arrOne = array(1);
$arrTwo = array(2);
$arrOne += $arrTwo;

```

[Processing Multiple Arrays Together](http://stackoverflow.com/documentation/php/6827/processing-multiple-arrays-together)

```php
$a **= 2; // $a now is (4 ** 2) => 16 (4 raised to the power of 2)

```

Combined concatenation and assignment of a string:

```php
$a = "a";
$a .= "b"; // $a => "ab"

```

Combined binary bitwise assignment operators:

```php
$a = 0b00101010;  // $a now is 42
$a &= 0b00001111; // $a now is (00101010 & 00001111) => 00001010 (bitwise and)
$a |= 0b00100010; // $a now is (00001010 | 00100010) => 00101010 (bitwise or)
$a ^= 0b10000010; // $a now is (00101010 ^ 10000010) => 10101000 (bitwise xor)
$a >>= 3;         // $a now is (10101000 >> 3) => 00010101 (shift right by 3)
$a <<= 1;         // $a now is (00010101 << 1) => 00101010 (shift left by 1)

```



## Altering operator precedence (with parentheses)


The order in which operators are evaluated is determined by the **operator precedence** (see also the Remarks section).

In

```php
$a = 2 * 3 + 4;

```

`$a` gets a value of 10 because `2 * 3` is evaluated first (multiplication has a higher precedence than addition) yielding a sub-result of `6 + 4`, which equals to 10.

The precedence can be altered using parentheses: in

```php
$a = 2 * (3 + 4);

```

`$a` gets a value of 14 because `(3 + 4)` is evaluated first.



## Basic Assignment (=)


```php
$a = "some string";

```

results in `$a` having the value `some string`.

The result of an assignment expression is the value being assigned. **Note that a single equal sign `=` is NOT for comparison!**

```php
$a = 3;
$b = ($a = 5);

```

does the following:

1. Line 1 assigns `3` to `$a`.
1. Line 2 assigns `5` to `$a`. This expression yields value `5` as well.
1. Line 2 then assigns the result of the expression in parentheses (`5`) to `$b`.

Thus: both `$a` and `$b` now have value `5`.



## Association


### Left association

If the preceedence of two operators is equal, the associativity determines the grouping (see also the Remarks section):

```php
$a = 5 * 3 % 2; // $a now is (5 * 3) % 2 => (15 % 2) => 1

```

`*` and `%` have equal precedence and **left** associativity. Because the multiplication occurs first (left), it is grouped.

```php
$a = 5 % 3 * 2; // $a now is (5 % 3) * 2 => (2 * 2) => 4

```

Now, the modulus operator occurs first (left) and is thus grouped.

### Right association

```php
$a = 1;
$b = 1;
$a = $b += 1;

```

Both `$a` and `$b` now have value `2` because `$b += 1` is grouped and then the result (`$b` is `2`) is assigned to `$a`.



## Comparison Operators


### Equality

For basic equality testing, the equal operator `==` is used. For more comprehensive checks, use the identical operator `===`.

The identical operator works the same as the equal operator, requiring its operands have the same value, but also requires them to have the same data type.

For example, the sample below will display 'a and b are equal', but not 'a and b are identical'.

```php
$a = 4;
$b = '4';
if ($a == $b) {
    echo 'a and b are equal'; // this will be printed
}
if ($a === $b) {
    echo 'a and b are identical'; // this won't be printed
}

```

When using the equal operator, numeric strings are cast to integers.

### Comparison of objects

`===` compares two objects by checking if they are exactly the **same instance**. This means that `new stdClass() === new stdClass()` resolves to false, even if they are created in the same way (and have the exactly same values).

`==` compares two objects by recursively checking if they are equal (**deep equals**). That means, for `$a == $b`, if `$a` and `$b` are:

1. of the same class
1. have the same properties set, including dynamic properties
1. for each property `$property` set, `$a->property == $b->property` is true (hence recursively checked).

### Other commonly used operators

They include:

1. Greater Than (`>`)
1. Lesser Than (`<`)
1. Greater Than Or Equal To (`>=`)
1. Lesser Than Or Equal To (`<=`)
1. Not Equal To (`!=`)
1. Not Identically Equal To (`!==`)

1. **Greater Than**: `$a > $b`, returns `true` if `$a`'s value is greater than of `$b`, otherwise returns false.

**Example**:

```php
var_dump(5 > 2); // prints bool(true)
var_dump(2 > 7); // prints bool(false)

```


1. **Lesser Than**: `$a < $b`, returns `true` if `$a`'s value is smaller that of `$b`, otherwise returns false.

**Example**:

```php
var_dump(5 < 2); // prints bool(false)
var_dump(1 < 10); // prints bool(true)

```


1. **Greater Than Or Equal To**: `$a >= $b`, returns `true` if `$a`'s value is either greater than of `$b` or equal to `$b`, otherwise returns `false`.

**Example**:

```php
var_dump(2 >= 2); // prints bool(true)
var_dump(6 >= 1); // prints bool(true)
var_dump(1 >= 7); // prints bool(false)

```


1. **Smaller Than Or Equal To**: `$a <= $b`, returns `true` if `$a`'s value is either smaller than of `$b` or equal to `$b`, otherwise returns `false`.

**Example**:

```php
var_dump(5 <= 5); // prints bool(true)
var_dump(5 <= 8); // prints bool(true)
var_dump(9 <= 1); // prints bool(false)

```

5/6. **Not Equal/Identical To:** To rehash the earlier example on equality, the sample below will display 'a and b are not identical', but not 'a and b are not equal'.

```php
$a = 4;
$b = '4';
if ($a != $b) {
    echo 'a and b are not equal'; // this won't be printed
}
if ($a !== $b) {
    echo 'a and b are not identical'; // this will be printed
}

```



## instanceof (type operator)


For checking whether some object is of a certain class, the (binary) `instanceof` operator can be used since PHP version 5.

The first (left) parameter is the object to test. If this variable is not an object, `instanceof` always returns `false`. If a constant expression is used, an error is thrown.

The second (right) parameter is the class to compare with. The class can be provided as the class name itself, a string variable containing the class name (not a string constant!) or an object of that class.

```php
class MyClass {
}

$o1 = new MyClass();
$o2 = new MyClass();
$name = 'MyClass';

// in the cases below, $a gets boolean value true
$a = $o1 instanceof MyClass;
$a = $o1 instanceof $name;
$a = $o1 instanceof $o2;

// counter examples:
$b = 'b';
$a = $o1 instanceof 'MyClass'; // parse error: constant not allowed
$a = false instanceof MyClass; // fatal error: constant not allowed
$a = $b instanceof MyClass;    // false ($b is not an object)

```

`instanceof` can also be used to check whether an object is of some class which extends another class or implements some interface:

```php
interface MyInterface {
}

class MySuperClass implements MyInterface {
}

class MySubClass extends MySuperClass {
}

$o = new MySubClass();

// in the cases below, $a gets boolean value true    
$a = $o instanceof MySubClass;
$a = $o instanceof MySuperClass;
$a = $o instanceof MyInterface;

```

To check whether an object is **not** of some class, the not operator (`!`) can be used:

```php
class MyClass {
}

class OtherClass {
}

$o = new MyClass();
$a = !$o instanceof OtherClass; // true

```

Note that parentheses around `$o instanceof MyClass` are not needed because `instanceof` has higher precedence than `!`, although it may make the code better readable **with** parentheses.

### Caveats

If a class does not exist, the registered autoload functions are called to try to define the class (this is a topic outside the scope of this part of the Documentation!). In PHP versions before 5.1.0, the `instanceof` operator would also trigger these calls, thus actually defining the class (and if the class could not be defined, a fatal error would occur). To avoid this, use a string:

```php
// only PHP versions before 5.1.0!
class MyClass {
}

$o = new MyClass();
$a = $o instanceof OtherClass; // OtherClass is not defined!
// if OtherClass can be defined in a registered autoloader, it is actually
// loaded and $a gets boolean value false ($o is not a OtherClass)
// if OtherClass can not be defined in a registered autoloader, a fatal
// error occurs.

$name = 'YetAnotherClass';
$a = $o instanceof $name; // YetAnotherClass is not defined!
// $a simply gets boolean value false, YetAnotherClass remains undefined.

```

As of PHP version 5.1.0, the registered autoloaders are not called anymore in these situations.

### Older versions of PHP (before 5.0)

In older versions of PHP (before 5.0), the `is_a` function can be used to determine wether an object is of some class. This function was deprecated in PHP version 5 and undeprecated in PHP version 5.3.0.



## Bitwise Operators


### Prefix bitwise operators

Bitwise operators are like logical operators but executed per bit rather than per boolean value.

```php
// bitwise NOT ~: sets all unset bits and unsets all set bits
printf("%'06b", ~0b110110); // 001001

```

### Bitmask-bitmask operators

Bitwise AND `&`: a bit is set only if it is set in both operands

```php
printf("%'06b", 0b110101 & 0b011001); // 010001

```

Bitwise OR `|`:  a bit is set if it is set in either or both operands

```php
printf("%'06b", 0b110101 | 0b011001); // 111101

```

Bitwise XOR `^`: a bit is set if it is set in one operand and not set in another operand, i.e. only if that bit is in different state in the two operands

```php
printf("%'06b", 0b110101 ^ 0b011001); // 101100

```

### Example uses of bitmasks

These operators can be used to manipulate bitmasks. For example:

```php
file_put_contents("file.log", LOCK_EX | FILE_APPEND);

```

Here, the `|` operator is used to combine the two bitmasks. Although `+` has the same effect, `|` emphasizes that you are combining bitmasks, not adding two normal scalar integers.

```php
class Foo{
    const OPTION_A = 1;
    const OPTION_B = 2;
    const OPTION_C = 4;
    const OPTION_A = 8;

    private $options = self::OPTION_A | self::OPTION_C;

    public function toggleOption(int $option){
        $this->options ^= $option;
    }

    public function enable(int $option){
        $this->options |= $option; // enable $option regardless of its original state
    }

    public function disable(int $option){
        $this->options &= ~$option; // disable $option regardless of its original state,
                                    // without affecting other bits
    }

    /** returns whether at least one of the options is enabled */
    public function isOneEnabled(int $options) : bool{
        return $this->options & $option !== 0;
        // Use !== rather than >, because 
        // if $options is about a high bit, we may be handling a negative integer
    }

    /** returns whether all of the options are enabled */
    public function areAllEnabled(int $options) : bool{
        return ($this->options & $options) === $options;
        // note the parentheses; beware the operator precedence
    }
}

```

This example (assuming `$option` always only contain one bit) uses:

- the `^` operator to conveniently toggle bitmasks.
- the `|` operator to set a bit neglecting its original state or other bits
- the `~` operator to convert an integer with only one bit set into an integer with only one bit not set
<li>the `&` operator to unset a bit, using these properties of `&`:
<ul>
- Since `&=` with a set bit will not do anything (`(1 & 1) === 1`, `(0 & 1) === 0`), doing `&=` with an integer with only one bit not set will only unset that bit, not affecting other bits.
- `&=` with an unset bit will unset that bit (`(1 & 0) === 0`, `(0 & 0) === 0`)

- If the output has any bits set, it means that any one of the options are enabled.
- If the output has all bits of the bitmask set, it means that all of the options in the bitmask are enabled.

Bear in mind that these comparison operators: (`<` `>` `<=` `>=` `==` `===` `!=` `!==` `<>` `<=>`) have higher precedence than these bitmask-bitmask operators: (`|` `^` `&`). As bitwise results are often compared using these comparison operators, this is a common pitfall to be aware of.

### Bit-shifting operators

Bitwise left shift `<<`: shift all bits to the left (more significant) by the given number of steps and discard the bits exceeding the int size

`<< $x` is equivalent to unsetting the highest `$x` bits and multiplying by the `$x`th power of 2

```php
printf("%'08b", 0b00001011<< 2); // 00101100

assert(PHP_INT_SIZE === 4); // a 32-bit system
printf("%x, %x", 0x5FFFFFFF << 2, 0x1FFFFFFF << 4); // 7FFFFFFC, FFFFFFFF

```

Bitwise right shift `>>`: discard the lowest shift and shift the remaining bits to the right (less significant)

`>> $x` is equivalent to dividing by the `$x`th power of 2 and discard the non-integer part

```php
printf("%x", 0xFFFFFFFF >> 3); // 1FFFFFFF

```

### Example uses of bit shifting:

Fast division by 16 (better performance than `/= 16`)

```php
$x >>= 4;

```

On 32-bit systems, this discards all bits in the integer, setting the value to 0.
On 64-bit systems, this unsets the most significant 32 bits and keep the least

```php
$x = $x << 32 >> 32;

```

significant 32 bits, equivalent to `$x & 0xFFFFFFFF`

Note: In this example, `printf("%'06b")` is used. It outputs the value in 6 binary digits.



#### Remarks


Operators 'operate' or act on one (unary operators such as `!$a` and `++$a`), two (binary operators such as `$a + $b` or `$a >> $b`) or three (the only ternary operator is `$a ? $b : $c`) expressions.

Operator precedence influences how operators are grouped (as if there were parentheses). The following is a list of operators in order of there precendence (operators in the second column). If multiple operators are in one row, the grouping is determined by the code order, where the first column indicates the associativity (see examples).

|Association|Operator
|------
|left|`->` `::`
|none|`clone` `new`
|left|`[`
|right|`**`
|right|`++` `--` `~` `(int)` `(float)` `(string)` `(array)` `(object)` `(bool)` `@`
|none|`instanceof`
|right|`!`
|left|`*` `/` `%`
|left|`+` `-` `.`
|left|`<<` `>>`
|none|`<` `<=` `>` `>=`
|none|`==` `!=` `===` `!==` `<>` `<=>`
|left|`&`
|left|`^`
|left|`|`
|left|`&&`
|left|`||`
|right|`??`
|left|`? :`
|right|`=` `+=` `-=` `*=` `**=` `/=` `.=` `%=` `&=` `
|left|`and`
|left|`xor`
|left|`or`

Full information is at [Stack Overflow](http://stackoverflow.com/questions/3737139/reference-what-do-various-symbols-mean-in-php).

Note that functions and language constructs (e.g. `print`) are always evaluated first, but any return value will be used according to the above precedence/associativity rules. Special care is needed if the parentheses after a language construct are omitted. E.g. `echo 2 . print 3 + 4;` echo's `721`: the `print` part evaluates `3 + 4`, prints the outcome `7` and returns `1`. After that, `2` is echoed, concatenated with the return value of `print` (`1`).

