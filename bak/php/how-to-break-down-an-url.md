---
metaTitle: "PHP - How to break down an URL"
description: "Using parse_url(), Using explode(), Using basename()"
---

# How to break down an URL


As you code PHP you will most likely get your self in a position where you need to break down an URL into several pieces. There's obviously more than one way of doing it depending on your needs. This article will explain those ways for you so you can find what works best for you.



## Using parse_url()


> 
parse_url(): This function parses a URL and returns an associative array containing any of the various components of the URL that are present.


```php
$url = parse_url('http://example.com/project/controller/action/param1/param2');

Array
(
    [scheme] => http
    [host] => example.com
    [path] => /project/controller/action/param1/param2
)

```

If you need the path separated you can use explode

```php
$url = parse_url('http://example.com/project/controller/action/param1/param2');
$url['sections'] = explode('/', $url['path']);

Array
(
    [scheme] => http
    [host] => example.com
    [path] => /project/controller/action/param1/param2
    [sections] => Array
        (
            [0] => 
            [1] => project
            [2] => controller
            [3] => action
            [4] => param1
            [5] => param2
        )

)

```

If you need the last part of the section you can use end() like this:

```php
$last = end($url['sections']);

```

If the URL contains GET vars you can retrieve those as well

```php
$url = parse_url('http://example.com?var1=value1&var2=value2');

Array
(
    [scheme] => http
    [host] => example.com
    [query] => var1=value1&var2=value2
)

```

If you wish to break down the query vars you can use parse_str() like this:

```php
$url = parse_url('http://example.com?var1=value1&var2=value2');
parse_str($url['query'], $parts);

Array
(
    [var1] => value1
    [var2] => value2
)

```



## Using explode()


> 
<p>explode(): Returns an array of strings, each of which is a substring of
string formed by splitting it on boundaries formed by the string
delimiter.</p>


This function is pretty much straight forward.

```php
$url = "http://example.com/project/controller/action/param1/param2";
$parts = explode('/', $url);

Array
(
    [0] => http:
    [1] => 
    [2] => example.com
    [3] => project
    [4] => controller
    [5] => action
    [6] => param1
    [7] => param2
)

```

You can retrieve the last part of the URL by doing this:

```php
$last = end($parts);
// Output: param2

```

You can also navigate inside the array by using sizeof() in combination with a math operator like this:

```php
echo $parts[sizeof($parts)-2];
// Output: param1

```



## Using basename()


> 
<p>basename(): Given a string containing the path to a file or directory,
this function will return the trailing name component.</p>


This function will return only the last part of an URL

```php
$url = "http://example.com/project/controller/action/param1/param2";
$parts = basename($url);
// Output: param2

```

If your URL has more stuff to it and what you need is the dir name containing the file you can use it with dirname() like this:

```php
$url = "http://example.com/project/controller/action/param1/param2/index.php";
$parts = basename(dirname($url));
// Output: param2

```

