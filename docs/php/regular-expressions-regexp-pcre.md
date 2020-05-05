---
metaTitle: "Regular Expressions (regexp/PCRE)"
description: "Global RegExp match, String matching with regular expressions, Split string into array by a regular expression, String replacing with regular expression, String replace with callback"
---

# Regular Expressions (regexp/PCRE)



## Global RegExp match


A **global** RegExp match can be performed using `preg_match_all`. `preg_match_all` returns all matching results in the subject string (in contrast to `preg_match`, which only returns the first one).

The `preg_match_all` function returns the number of matches. Third parameter `$matches` will contain matches in format controlled by flags that can be given in fourth parameter.

If given an array, `$matches` will contain array in similar format you’d get with `preg_match`, except that `preg_match` stops at first match, where `preg_match_all` iterates over the string until the string is wholly consumed and returns result of each iteration in a multidimensional array, which format can be controlled by the flag in fourth argument.

The fourth argument, `$flags`, controls structure of `$matches` array. Default mode is `PREG_PATTERN_ORDER` and possible flags are `PREG_SET_ORDER` and `PREG_PATTERN_ORDER`.

Following code demonstrates usage of `preg_match_all`:

```php
$subject = "a1b c2d3e f4g";
$pattern = '/[a-z]([0-9])[a-z]/';

var_dump(preg_match_all($pattern, $subject, $matches, PREG_SET_ORDER)); // int(3)
var_dump($matches);
preg_match_all($pattern, $subject, $matches); // the flag is PREG_PATTERN_ORDER by default
var_dump($matches);
// And for reference, same regexp run through preg_match()
preg_match($pattern, $subject, $matches);
var_dump($matches);

```

The first var_dump from `PREG_SET_ORDER` gives this output:

```php
array(3) {
  [0]=>
  array(2) {
    [0]=>
    string(3) "a1b"
    [1]=>
    string(1) "1"
  }
  [1]=>
  array(2) {
    [0]=>
    string(3) "c2d"
    [1]=>
    string(1) "2"
  }
  [2]=>
  array(2) {
    [0]=>
    string(3) "f4g"
    [1]=>
    string(1) "4"
  }
}

```

`$matches` has three nested arrays. Each array represents one match, which has the same format as the return result of `preg_match`.

The second var_dump (`PREG_PATTERN_ORDER`) gives this output:

```php
array(2) {
  [0]=>
  array(3) {
    [0]=>
    string(3) "a1b"
    [1]=>
    string(3) "c2d"
    [2]=>
    string(3) "f4g"
  }
  [1]=>
  array(3) {
    [0]=>
    string(1) "1"
    [1]=>
    string(1) "2"
    [2]=>
    string(1) "4"
  }
}

```

When the same regexp is run through `preg_match`, following array is returned:

```php
array(2) {
  [0] =>
  string(3) "a1b"
  [1] =>
  string(1) "1"
}

```



## String matching with regular expressions


`preg_match` checks whether a string matches the regular expression.

```php
$string = 'This is a string which contains numbers: 12345';

$isMatched = preg_match('%^[a-zA-Z]+: [0-9]+$%', $string);
var_dump($isMatched); // bool(true)

```

If you pass in a third parameter, it will be populated with the matching data of the regular expression:

```php
preg_match('%^([a-zA-Z]+): ([0-9]+)$%', 'This is a string which contains numbers: 12345', $matches);
// $matches now contains results of the regular expression matches in an array.
echo json_encode($matches); // ["numbers: 12345", "numbers", "12345"]

```

`$matches` contains an array of the whole match then substrings in the regular expression bounded by parentheses, in the order of open parenthesis's offset. That means, if you have `/z(a(b))/` as the regular expression, index 0 contains the whole substring `zab`, index 1 contains the substring bounded by the outer parentheses `ab` and index 2 contains the inner parentheses `b`.



## Split string into array by a regular expression


```php
$string = "0| PHP 1| CSS 2| HTML 3| AJAX 4| JSON";

//[0-9]: Any single character in the range 0 to 9
// +   : One or more of 0 to 9
$array = preg_split("/[0-9]+\|/", $string, -1, PREG_SPLIT_NO_EMPTY);
//Or
// []  : Character class
// \d  : Any digit
//  +  : One or more of Any digit
$array = preg_split("/[\d]+\|/", $string, -1, PREG_SPLIT_NO_EMPTY);

```

Output:

```php
Array
(
    [0] =>  PHP
    [1] =>  CSS 
    [2] =>  HTML 
    [3] =>  AJAX 
    [4] =>  JSON
)

```

To split a string into a array simply pass the string and a regexp for `preg_split();` to match and search, adding a third parameter (`limit`) allows you to set the number of "matches" to perform, the remaining string will be added to the end of the array.

The fourth parameter is (`flags`) here we use the `PREG_SPLIT_NO_EMPTY` which prevents our array from containing any empty keys / values.



## String replacing with regular expression


```php
$string = "a;b;c\nd;e;f";
// $1, $2 and $3 represent the first, second and third capturing groups
echo preg_replace("(^([^;]+);([^;]+);([^;]+)$)m", "$3;$2;$1", $string);

```

Outputs

```php
c;b;a
f;e;d

```

Searches for everything between semicolons and reverses the order.



## String replace with callback


`preg_replace_callback` works by sending every matched capturing group to the defined callback and replaces it with the return value of the callback.
This allows us to replace strings based on any kind of logic.

```php
$subject = "He said 123abc, I said 456efg, then she said 789hij";
$regex = "/\b(\d+)\w+/";

// This function replaces the matched entries conditionally 
// depending upon the first character of the capturing group
function regex_replace($matches){
    switch($matches[1][0]){
        case '7':
            $replacement = "<b>{$matches[0]}</b>";
            break;
        default:
            $replacement = "<i>{$matches[0]}</i>";
    }
    return $replacement;
}

$replaced_str = preg_replace_callback($regex, "regex_replace", $subject);

print_r($replaced_str);
# He said <i>123abc</i>, I said <i>456efg</i>, then she said <b>789hij</b> 

```



#### Syntax


- `preg_replace($pattern, $replacement, $subject, $limit = -1, $count = 0);`
- `preg_replace_callback($pattern, $callback, $subject, $limit = -1, $count = 0);`
- `preg_match($pattern, $subject, &$matches, $flags = 0, $offset = 0);`
- `preg_match_all($pattern, $subject, &$matches, $flags = PREG_PATTERN_ORDER, $offset = 0);`
- `preg_split($pattern, $subject, $limit = -1, $flags = 0)`



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`$pattern`|a string with a regular expression (PCRE pattern)



#### Remarks


PHP regular expressions follow PCRE pattern standards, which are derived from Perl regular expressions.

All PCRE strings in PHP must be enclosed with delimiters. A delimiter can be any non-alphanumeric, non-backslash, non-whitespace character. Popular delimiters are `~`, `/`, `%` for instance.

PCRE patterns can contain groups, character classes, character groups, look-ahead/look-behind assertions and escaped characters.

It is possible to use PCRE modifiers in the `$pattern` string. Some common ones are `i` (case insensitive), `m` (multiline) and `s` (the dot metacharacter includes newlines). The `g` (global) modifier is not allowed, you will use the `preg_match_all` function instead.

Matches to PCRE strings are done with `$` prefixed numbered strings:

```php
<?php

$replaced = preg_replace('%hello ([a-z]+) world%', 'goodbye $1 world', 'hello awesome world');

echo $replaced; // 'goodbye awesome world'

```

