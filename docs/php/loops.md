---
metaTitle: "Loops"
description: "continue, break, foreach, do...while, for, while"
---

# Loops


Loops are a fundamental aspect of programming. They allow programmers to create code that repeats for some given number of repetitions, or **iterations**. The number of iterations can be explicit (6 iterations, for example), or continue until some condition is met ('until Hell freezes over').

This topic covers the different types of loops, their associated control statements, and their potential applications in PHP.



## continue


> 
The `continue` keyword halts the current iteration of a loop but does not terminate the loop.


Just like the `break` statement the `continue` statement is situated inside the loop body. When executed, the `continue` statement causes execution to immediately jump to the loop conditional.

In the following example loop prints out a message based on the values in an array, but skips a specified value.

```php
$list = ['apple', 'banana', 'cherry'];

foreach ($list as $value) {
    if ($value == 'banana') {
        continue;
    }
    echo "I love to eat {$value} pie.".PHP_EOL;
}

```

The expected output is:

```php
I love to eat apple pie.
I love to eat cherry pie.

```

The `continue` statement may also be used to immediately continue execution to an outer level of a loop by specifying the number of loop levels to jump. For example, consider data such as

|Fruit|Color|Cost
|------
|Apple|Red|1
|Banana|Yellow|7
|Cherry|Red|2
|Grape|Green|4

In order to only make pies from fruit which cost less than 5

```php
$data = [
    [ "Fruit" => "Apple",  "Color" => "Red",    "Cost" => 1 ],
    [ "Fruit" => "Banana", "Color" => "Yellow", "Cost" => 7 ],
    [ "Fruit" => "Cherry", "Color" => "Red",    "Cost" => 2 ],
    [ "Fruit" => "Grape",  "Color" => "Green",  "Cost" => 4 ]
];

foreach($data as $fruit) {
    foreach($fruit as $key => $value) {
        if ($key == "Cost" && $value >= 5) {
            continue 2;
        }
        /* make a pie */
    }
}

```

When the `continue 2` statement is executed, execution immediately jumps back to `$data as $fruit` continuing the outer loop and skipping all other code (including the conditional in the inner loop.



## break


> 
The `break` keyword immediately terminates the current loop.


Similar to the `continue` statement, a `break` halts execution of a loop. Unlike a `continue` statement, however, `break` causes the immediate termination of the loop and does **not** execute the conditional statement again.

```php
$i = 5;
while(true) {
    echo 120/$i.PHP_EOL;
    $i -= 1;
    if ($i == 0) {
        break;
    }
}

```

This code will produce

```php
24
30
40
60
120

```

but will not execute the case where `$i` is 0, which would result in a fatal error due to division by 0.

The break statement may also be used to break out of several levels of loops. Such behavior is very useful when executing nested loops. For example, to copy an array of strings into an output string, removing any `#` symbols, until the output string is exactly 160 characters

```php
$output = "";
$inputs = array(
    "#soblessed #throwbackthursday",
    "happy tuesday",
    "#nofilter",
    /* more inputs */
);
foreach($inputs as $input) {
    for($i = 0; $i < strlen($input); $i += 1) {
        if ($input[$i] == '#') continue;
        $output .= $input[$i];
        if (strlen($output) == 160) break 2; 
    }
    $output .= ' ';
}

```

The `break 2` command immediately terminates execution of both the inner and outer loops.



## foreach


> 
The `foreach` statement is used to loop through arrays.


For each iteration the value of the current array element is assigned to `$value` variable and the array pointer is moved by one and in the next iteration next element will be processed.

The following example displays the items in the array assigned.

```php
$list = ['apple', 'banana', 'cherry'];

foreach ($list as $value) {
    echo "I love to eat {$value}. ";
}

```

The expected output is:

```php
I love to eat apple. I love to eat banana. I love to eat cherry. 

```

You can also access the key / index of a value using foreach:

```php
foreach ($list as $key => $value) {
    echo $key . ":" . $value . " ";
}

//Outputs - 0:apple 1:banana 2:cherry

```

By default `$value` is a copy of the value in `$list`, so changes made inside the loop will not be reflected in `$list` afterwards.

```php
foreach ($list as $value) {
    $value = $value . " pie";
}
echo $list[0]; // Outputs "apple"

```

To modify the array within the `foreach` loop, use the `&` operator to assign `$value` by reference. It's important to `unset` the variable afterwards so that reusing `$value` elsewhere doesn't overwrite the array.

```php
foreach ($list as &$value) { // Or foreach ($list as $key => &$value) {
    $value = $value . " pie";
}
unset($value);
echo $list[0]; // Outputs "apple pie"

```

You can also modify the array items within the `foreach` loop by referencing the array key of the current item.

```php
foreach ($list as $key => $value) {
    $list[$key] = $value . " pie";
}
echo $list[0]; // Outputs "apple pie"

```



## do...while


> 
The `do...while` statement will execute a block of code at least once - it then will repeat the loop as long as a condition is true.


The following example will increment the value of `$i` at least once, and it will continue incrementing the variable `$i` as long as it has a value of less than 25;

```php
$i = 0;
do {
    $i++;
} while($i < 25);

echo 'The final value of i is: ', $i;

```

The expected output is:

```php
The final value of i is: 25

```



## for


> 
The `for` statement is used when you know how many times you want to execute a statement or a block of statements.


The initializer is used to set the start value for the counter of the number of loop iterations. A variable may be declared here for this purpose and it is traditional to name it `$i`.

The following example iterates 10 times and displays numbers from 0 to 9.

```php
for ($i = 0; $i <= 9; $i++) {
    echo $i, ',';
}

# Example 2
for ($i = 0; ; $i++) {
  if ($i > 9) {
      break;
  }
  echo $i, ',';
}

# Example 3
$i = 0;
for (; ; ) {
    if ($i > 9) {
        break;
    }
    echo $i, ',';
    $i++;
}

# Example 4
for ($i = 0, $j = 0; $i <= 9; $j += $i, print $i. ',', $i++);

```

The expected output is:

```php
0,1,2,3,4,5,6,7,8,9,

```



## while


> 
The `while` statement will execute a block of code if and as long as a test expression is true.


If the test expression is true then the code block will be executed. After the code has executed the test expression will again be evaluated and the loop will continue until the test expression is found to be false.

The following example iterates till the sum reaches 100 before terminating.

```php
$i = true;
$sum = 0;

while ($i) {
    if ($sum === 100) {
        $i = false;
    } else {
        $sum += 10;
    }
}
echo 'The sum is: ', $sum;

```

The expected output is:

```php
The sum is: 100

```



#### Syntax


- for (init counter; test counter; increment counter) { /* code */ }
- foreach (array as value) { /* code */ }
- foreach (array as key => value) { /* code */ }
- while (condition) { /* code */ }
- do { /* code */ } while (condition);
- **anyloop** { continue; }
- **anyloop** { [ **anyloop** ...] { continue int; } }
- **anyloop** { break; }
- **anyloop** { [ **anyloop** ...] { break int; } }



#### Remarks


It is often useful to execute the same or similar block of code several times. Instead of copy-pasting almost equal statements loops provide a mechanism for executing code a specific number of times and walking over data structures. PHP supports the following four types of loops:

- **`for`**
- **`while`**
- **`do..while`**
- **`foreach`**

To control these loops, **`continue`** and **`break`** statements are available.

