---
metaTitle: "PHP - String Parsing"
description: "Splitting a string by separators, Searching a substring with strpos, Substring, Parsing string using regular expressions"
---

# String Parsing



## Splitting a string by separators


[`explode`](http://php.net/explode) and [`strstr`](http://php.net/strstr) are simpler methods to get substrings by separators.

A string containing several parts of text that are separated by a common character can be split into parts with the [`explode`](http://php.net/explode) function.

```php
$fruits = "apple,pear,grapefruit,cherry";
print_r(explode(",",$fruits)); // ['apple', 'pear', 'grapefruit', 'cherry']

```

The method also supports a limit parameter that can be used as follow:

```php
$fruits= 'apple,pear,grapefruit,cherry';

```

If the limit parameter is zero, then this is treated as 1.

```php
print_r(explode(',',$fruits,0)); // ['apple,pear,grapefruit,cherry']

```

If limit is set and positive, the returned array will contain a maximum of limit elements with the last element containing the rest of string.

```php
print_r(explode(',',$fruits,2)); // ['apple', 'pear,grapefruit,cherry']

```

If the limit parameter is negative, all components except the last -limit are returned.

```php
print_r(explode(',',$fruits,-1)); // ['apple', 'pear', 'grapefruit']

```

`explode` can be combined with [`list`](http://php.net/list) to parse a string into variables in one line:

```php
$email = "user@example.com";
list($name, $domain) = explode("@", $email);

```

However, make sure that the result of `explode` contains enough elements, or an undefined index warning would be triggered.

`strstr` strips away or only returns the substring before the first occurrence of the given needle.

```php
$string = "1:23:456";
echo json_encode(explode(":", $string)); // ["1","23","456"]
var_dump(strstr($string, ":")); // string(7) ":23:456"

var_dump(strstr($string, ":", true)); // string(1) "1"

```



## Searching a substring with strpos


`strpos` can be understood as the number of bytes in the haystack before the first occurrence of the needle.

```php
var_dump(strpos("haystack", "hay")); // int(0)
var_dump(strpos("haystack", "stack")); // int(3)
var_dump(strpos("haystack", "stackoverflow"); // bool(false)

```

### Checking if a substring exists

Be careful with checking against TRUE or FALSE because if a index of 0 is returned an if statement will see this as FALSE.

```php
$pos = strpos("abcd", "a"); // $pos = 0;
$pos2 = strpos("abcd", "e"); // $pos2 = FALSE;

// Bad example of checking if a needle is found.
if($pos) { // 0 does not match with TRUE.
    echo "1. I found your string\n";
}
else {
    echo "1. I did not found your string\n";
}

// Working example of checking if needle is found.
if($pos !== FALSE) {
    echo "2. I found your string\n";
}
else {
    echo "2. I did not found your string\n";
}

// Checking if a needle is not found
if($pos2 === FALSE) {
    echo "3. I did not found your string\n";
}
else {
    echo "3. I found your string\n";
}

```

Output of the whole example:

```php
1. I did not found your string 
2. I found your string 
3. I did not found your string 

```

### Search starting from an offset

```php
// With offset we can search ignoring anything before the offset
$needle = "Hello";
$haystack = "Hello world! Hello World";

$pos = strpos($haystack, $needle, 1); // $pos = 13, not 0

```

### Get all occurrences of a substring

```php
$haystack = "a baby, a cat, a donkey, a fish";
$needle = "a ";
$offsets = [];
// start searching from the beginning of the string
for($offset = 0;
        // If our offset is beyond the range of the
        // string, don't search anymore.
        // If this condition is not set, a warning will
        // be triggered if $haystack ends with $needle
        // and $needle is only one byte long.
        $offset < strlen($haystack); ){
    $pos = strpos($haystack, $needle, $offset);
    // we don't have anymore substrings
    if($pos === false) break;
    $offsets[] = $pos;
    // You may want to add strlen($needle) instead,
    // depending on whether you want to count "aaa"
    // as 1 or 2 "aa"s.
    $offset = $pos + 1;
}
echo json_encode($offsets); // [0,8,15,25]

```



## Substring


Substring returns the portion of string specified by the start and length parameters.

```php
var_dump(substr("Boo", 1)); // string(2) "oo"

```

If there is a possibility of meeting multi-byte character strings, then it would be safer to use mb_substr.

```php
$cake = "cakeæøå";
var_dump(substr($cake, 0, 5)); // string(5) "cake�"
var_dump(mb_substr($cake, 0, 5, 'UTF-8')); // string(6) "cakeæ"

```

Another variant is the substr_replace function, which replaces text within a portion of a string.

```php
var_dump(substr_replace("Boo", "0", 1, 1)); // string(3) "B0o"
var_dump(substr_Replace("Boo", "ts", strlen("Boo"))); // string(5) "Boots"

```

Let's say you want to find a specific word in a string - and don't want to use Regex.

```php
$hi = "Hello World!";
$bye = "Goodbye cruel World!";

var_dump(strpos($hi, " ")); // int(5)
var_dump(strpos($bye, " ")); // int(7)

var_dump(substr($hi, 0, strpos($hi, " "))); // string(5) "Hello"
var_dump(substr($bye, -1 * (strlen($bye) - strpos($bye, " ")))); // string(13) " cruel World!"

// If the casing in the text is not important, then using strtolower helps to compare strings
var_dump(substr($hi, 0, strpos($hi, " ")) == 'hello'); // bool(false)
var_dump(strtolower(substr($hi, 0, strpos($hi, " "))) == 'hello'); // bool(true)

```

Another option is a very basic parsing of an email.

```php
$email = "test@example.com";
$wrong = "foobar.co.uk";
$notld = "foo@bar";

$at = strpos($email, "@"); // int(4)
$wat = strpos($wrong, "@"); // bool(false)
$nat = strpos($notld , "@"); // int(3)

$domain = substr($email, $at + 1); // string(11) "example.com"
$womain = substr($wrong, $wat + 1); // string(11) "oobar.co.uk"
$nomain = substr($notld, $nat + 1); // string(3) "bar"

$dot = strpos($domain, "."); // int(7)
$wot = strpos($womain, "."); // int(5)
$not = strpos($nomain, "."); // bool(false)

$tld = substr($domain, $dot + 1); // string(3) "com"
$wld = substr($womain, $wot + 1); // string(5) "co.uk"
$nld = substr($nomain , $not + 1); // string(2) "ar"

// string(25) "test@example.com is valid"
if ($at && $dot) var_dump("$email is valid");
else var_dump("$email is invalid");

// string(21) "foobar.com is invalid"
if ($wat && $wot) var_dump("$wrong is valid");
else var_dump("$wrong is invalid");

// string(18) "foo@bar is invalid"
if ($nat && $not) var_dump("$notld is valid");
else var_dump("$notld is invalid");

// string(27) "foobar.co.uk is an UK email"
if ($tld == "co.uk") var_dump("$email is a UK address");
if ($wld == "co.uk") var_dump("$wrong is a UK address");
if ($nld == "co.uk") var_dump("$notld is a UK address");

```

Or even putting the "Continue reading" or "..." at the end of a blurb

```php
$blurb = "Lorem ipsum dolor sit amet";
$limit = 20;

var_dump(substr($blurb, 0, $limit - 3) . '...'); // string(20) "Lorem ipsum dolor..."

```



## Parsing string using regular expressions


preg_match can be used to parse string using regular expression. The parts of expression enclosed in parenthesis are called subpatterns and with them you can pick individual parts of the string.

```php
$str = "<a href=\"http://example.org\">My Link</a>";
$pattern = "/<a href=\"(.*)\">(.*)<\/a>/";
$result = preg_match($pattern, $str, $matches);
if($result === 1) {
    // The string matches the expression
    print_r($matches);
} else if($result === 0) {
    // No match
} else {
    // Error occured
}

```

Output

```php
Array
(
    [0] => <a href="http://example.org">My Link</a>
    [1] => http://example.org
    [2] => My Link
)

```



#### Remarks


Regex should be used for other uses besides getting strings out of strings or otherwise cutting strings into pieces.

