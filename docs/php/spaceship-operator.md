---
metaTitle: "Spaceship operator"
description: "Sorting a list of numbers, Simple Example, Generic numerical example"
---

# Spaceship operator


The spaceship operator is used for comparing two expressions. For example, $a <=> $b returns -1, 0 or 1 when $a is respectively less than, equal to, or greater than $b. Comparisons are performed according to PHP's usual type comparison rules.



## Sorting a list of numbers


```
$array = [1, 0, 5, 9, 3, 7, 6, 8, 4, 2];

usort($array, function (int $a, int $b): int {
    return $a <=> $b;
});

print_r($array);

```

```
Array
(
    [0] => 0
    [1] => 1
    [2] => 2
    [3] => 3
    [4] => 4
    [5] => 5
    [6] => 6
    [7] => 7
    [8] => 8
    [9] => 9
)

```



## Simple Example


```
$a = 5;
$b = 10;

$a <=> $a; // 0, because $a == $a
$a <=> $b; // -1, because $a < $b
$b <=> $a; // 1, because $b > $a 

```



## Generic numerical example


Generic example in a form of `$a <=> $b` matrix.

```
0 <=> 1; // -1 (left operand less than right, right is greater)
0 <=> 0; // 0 (operands are equal)
1 <=> 0; // 1 (left operand greater than right, left is greater)
1 <=> 1; // 0 (operands are equal)

```

```
╔═══════╦════╦════╗
║ $a/$b ║  0 ║  1 ║
╠═══════╬════╬════╣
║   0   ║  0 ║ -1 ║
║   1   ║  1 ║  0 ║
╚═══════╩════╩════╝

```

> 
**$a** - leftmost column, **$b** - topmost row


