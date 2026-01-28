---
metaTitle: "PHP - URLs"
description: "Parsing a URL, Build an URL-encoded query string from an array, Redirecting to another URL"
---

# URLs



## Parsing a URL


To separate a URL into its individual components, use [`parse_url()`](http://php.net/parse_url):

```php
$url = 'http://www.example.com/page?foo=1&bar=baz#anchor';
$parts = parse_url($url);

```

After executing the above, the contents of `$parts` would be:

```php
Array
(
    [scheme] => http
    [host] => www.example.com
    [path] => /page
    [query] => foo=1&bar=baz
    [fragment] => anchor
)

```

You can also selectively return just one component of the url. To return just the querystring:

```php
$url = 'http://www.example.com/page?foo=1&bar=baz#anchor';
$queryString = parse_url($url, PHP_URL_QUERY);

```

Any of the following constants are accepted: `PHP_URL_SCHEME`, `PHP_URL_HOST`, `PHP_URL_PORT`, `PHP_URL_USER`, `PHP_URL_PASS`, `PHP_URL_PATH`, `PHP_URL_QUERY` and `PHP_URL_FRAGMENT`.

To further parse a query string into key value pairs use [`parse_str()`](http://php.net/parse_str):

```php
$params = [];
parse_str($queryString, $params);

```

After execution of the above, the `$params` array would be populated with the following:

```php
Array
(
    [foo] => 1
    [bar] => baz
)

```



## Build an URL-encoded query string from an array


The [`http_build_query()`](http://php.net/manual/function.http-build-query.php) will create a query string from an array or object. These strings can be appended to a URL to create a GET request, or used in a POST request with, for example, cURL.

```php
$parameters = array(
    'parameter1' => 'foo',
    'parameter2' => 'bar',
);
$queryString = http_build_query($parameters);

```

`$queryString` will have the following value:

```php
parameter1=foo&parameter2=bar

```

`http_build_query()` will also work with multi-dimensional arrays:

```php
$parameters = array(
    "parameter3" => array(
        "sub1" => "foo",
        "sub2" => "bar",
    ),
    "parameter4" => "baz",
);
$queryString = http_build_query($parameters);

```

`$queryString` will have this value:

```php
parameter3%5Bsub1%5D=foo&parameter3%5Bsub2%5D=bar&parameter4=baz

```

which is the URL-encoded version of

```php
parameter3[sub1]=foo&parameter3[sub2]=bar&parameter4=baz

```



## Redirecting to another URL


You can use the `header()` function to instruct the browser to redirect to a different URL:

```php
$url = 'https://example.org/foo/bar';
if (!headers_sent()) { // check headers - you can not send headers if they already sent
  header('Location: ' . $url);
  exit; // protects from code being executed after redirect request
} else {
  throw new Exception('Cannot redirect, headers already sent');
}

```

You can also redirect to a relative URL (this is not part of the official HTTP specification, but it does work in all browsers):

```php
$url = 'foo/bar';
if (!headers_sent()) {
  header('Location: ' . $url);
  exit;
} else {
  throw new Exception('Cannot redirect, headers already sent');
}

```

If headers have been sent, you can alternatively send a `meta refresh` HTML tag.

**WARNING:** The meta refresh tag relies on HTML being properly processed by the client, and some will not do this. In general, it only works in web browsers. Also, consider that if headers have been sent, you may have a bug and this should trigger an exception.

You may also print a link for users to click, for clients that ignore the meta refresh tag:

```php
$url = 'https://example.org/foo/bar';
if (!headers_sent()) {
  header('Location: ' . $url);
} else {
  $saveUrl = htmlspecialchars($url); // protects from browser seeing url as HTML
  // tells browser to redirect page to $saveUrl after 0 seconds
  print '<meta http-equiv="refresh" content="0; url=' . $saveUrl . '">';
  // shows link for user
  print '<p>Please continue to <a href="' . $saveUrl . '">' . $saveUrl . '</a></p>';
}
exit;

```

