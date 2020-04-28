---
metaTitle: "Generators"
description: "The Yield Keyword, Reading a large file with a generator, Why use a generator?, Using the send()-function to pass values to a generator, Re-writing randomNumbers() using a generator"
---

# Generators



## The Yield Keyword


A `yield` statement is similar to a return statement, except that instead of stopping execution of the function and returning, yield instead returns a [Generator](http://php.net/manual/en/class.generator.php) object and pauses execution of the generator function.

Here is an example of the range function, written as a generator:

```php
function gen_one_to_three() {
    for ($i = 1; $i <= 3; $i++) {
        // Note that $i is preserved between yields.
        yield $i;
    }
}

```

You can see that this function returns a [Generator](http://php.net/manual/en/class.generator.php) object by inspecting the output of `var_dump`:

```php
var_dump(gen_one_to_three())

# Outputs:
class Generator (0) {
}

```

### Yielding Values

The [Generator](http://php.net/manual/en/class.generator.php) object can then be iterated over like an array.

```php
foreach (gen_one_to_three() as $value) {
    echo "$value\n";
}

```

The above example will output:

```php
1
2
3

```

### Yielding Values with Keys

In addition to yielding values, you can also yield key/value pairs.

```php
function gen_one_to_three() {
    $keys = ["first", "second", "third"];

    for ($i = 1; $i <= 3; $i++) {
        // Note that $i is preserved between yields.
        yield $keys[$i - 1] => $i;
    }
}

foreach (gen_one_to_three() as $key => $value) {
    echo "$key: $value\n";
}

```

The above example will output:

```php
first: 1
second: 2
third: 3

```



## Reading a large file with a generator


One common use case for generators is reading a file from disk and iterating over its contents. Below is a class that allows you to iterate over a CSV file. The memory usage for this script is very predictable, and will not fluctuate depending on the size of the CSV file.

```php
<?php

class CsvReader
{
    protected $file;
 
    public function __construct($filePath) {
        $this->file = fopen($filePath, 'r');
    }
 
    public function rows()
    {
        while (!feof($this->file)) {
            $row = fgetcsv($this->file, 4096);
            
            yield $row;
        }
        
        return;
    }
}
 
$csv = new CsvReader('/path/to/huge/csv/file.csv');

foreach ($csv->rows() as $row) {
    // Do something with the CSV row.
}

```



## Why use a generator?


Generators are useful when you need to generate a large collection to later iterate over. They're a simpler alternative to creating a class that implements an [Iterator](http://php.net/manual/en/class.iterator.php), which is often overkill.

For example, consider the below function.

```php
function randomNumbers(int $length)
{
    $array = [];
    
    for ($i = 0; $i < $length; $i++) {
        $array[] = mt_rand(1, 10);
    }
    
    return $array;
}

```

All this function does is generates an array that's filled with random numbers. To use it, we might do `randomNumbers(10)`, which will give us an array of 10 random numbers. What if we want to generate one million random numbers? `randomNumbers(1000000)` will do that for us, but at a cost of memory. One million integers stored in an array uses approximately **33 megabytes** of memory.

```php
$startMemory = memory_get_usage();

$randomNumbers = randomNumbers(1000000);

echo memory_get_usage() - $startMemory, ' bytes';

```

This is due to the entire one million random numbers being generated and returned at once, rather than one at a time. Generators are an easy way to solve this issue.



## Using the send()-function to pass values to a generator


Generators are fast coded and in many cases a slim alternative to heavy iterator-implementations.
With the fast implementation comes a little lack of control when a generator should stop generating or if it should generate something else. However this can be achieved with the usage of the `send()` function, enabling the requesting function to send parameters to the generator after every loop.

```php
//Imagining accessing a large amount of data from a server, here is the generator for this:
function generateDataFromServerDemo()
{
    $indexCurrentRun = 0; //In this example in place of data from the server, I just send feedback everytime a loop ran through.

    $timeout = false;
    while (!$timeout)
    {
        $timeout = yield $indexCurrentRun; // Values are passed to caller. The next time the generator is called, it will start at this statement. If send() is used, $timeout will take this value.
        $indexCurrentRun++;
    }

    yield 'X of bytes are missing. </br>';
}

// Start using the generator
$generatorDataFromServer = generateDataFromServerDemo ();
foreach($generatorDataFromServer as $numberOfRuns)
{
    if ($numberOfRuns < 10)
    {
        echo $numberOfRuns . "</br>";
    }
    else
    {
        $generatorDataFromServer->send(true); //sending data to the generator
        echo $generatorDataFromServer->current(); //accessing the latest element (hinting how many bytes are still missing.
    }
}

```

Resulting in this Output:

[<img src="https://i.stack.imgur.com/ipsO9.png" alt="enter image description here" />](https://i.stack.imgur.com/ipsO9.png)



## Re-writing randomNumbers() using a generator


Our `randomNumbers()` function can be re-written to use a generator.

```php
<?php

function randomNumbers(int $length)
{
    for ($i = 0; $i < $length; $i++) {
        // yield tells the PHP interpreter that this value
        // should be the one used in the current iteration.
        yield mt_rand(1, 10);
    }
}

foreach (randomNumbers(10) as $number) {
    echo "$number\n";
}

```

Using a generator, we don't have to build an entire list of random numbers to return from the function, leading to much less memory being used.

