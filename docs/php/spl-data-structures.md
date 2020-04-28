---
metaTitle: "SPL data structures"
description: "SplFixedArray"
---

# SPL data structures



## SplFixedArray


### Difference from PHP Array

PHP's default Array type is actually implemented as ordered hash maps, which allow us to create arrays that consist of key/value pairs where values can be of any type and keys can be either numbers or strings. This is not traditionally how arrays are created, however.

[<img src="https://i.stack.imgur.com/vWcnk.png" alt="Traditional PHP Array Figure 1" />](https://i.stack.imgur.com/vWcnk.png)

So as you can see from this illustration a normal PHP array can be viewed more like an an ordered set of key/value pairs, where each key can map to any value. Notice in this array we have keys that are both numbers and strings, as well as values of different types and the key has no bearing on the order of the elements.

```
$arr = [
    9     => "foo",
    1     => 4.2,
    "bar" => null,
];

foreach($arr as $key => $value) {
    echo "$key => $value\n";
}

```

So the above code would give us exactly what we'd expect.

```
$arr = new SplFixedArray(4);

$arr[0] = "foo";
$arr[1] = "bar";
$arr[2] = "baz";

foreach($arr as $key => $value) {
    echo "$key => $value\n";
}

```

This gives you what you would expect.

```
var_dump(count($arr));

```

Gives us...

```
$arr->setSize(3);

var_dump(count($arr));

foreach($arr as $key => $value) {
    echo "$key => $value\n";
}

```

Now we get...

```
$array      = [1,2,3,4,5];
$fixedArray = SplFixedArray::fromArray($array);

foreach($fixedArray as $value) {
    echo $value, "\n";
}

```

```
$fixedArray = new SplFixedArray(5);

$fixedArray[0] = 1;
$fixedArray[1] = 2;
$fixedArray[2] = 3;
$fixedArray[3] = 4;
$fixedArray[4] = 5;

$array = $fixedArray->toArray();

foreach($array as $value) {
    echo $value, "\n";
}

```

