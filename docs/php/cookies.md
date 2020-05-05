---
metaTitle: "PHP - Cookies"
description: "Modifying a Cookie, Setting a Cookie, Checking if a Cookie is Set, Removing a Cookie, Retrieving a Cookie"
---

# Cookies


An HTTP cookie is a small piece of data sent from a website and stored on the user's computer by the user's web browser while the user is browsing.



## Modifying a Cookie


The value of a cookie can be modified by resetting the cookie

```php
setcookie("user", "John", time() + 86400, "/"); // assuming there is a "user" cookie already

```

> 
Cookies are part of the HTTP header, so `setcookie()` must be called before any output is sent to the browser.


> 
When modifying a cookie make sure the `path` and `domain` parameters of `setcookie()` matches the existing cookie or a new cookie will be created instead.


> 
The value portion of the cookie will automatically be urlencoded when you send the cookie, and when it is received, it is automatically decoded and assigned to a variable by the same name as the cookie name




## Setting a Cookie


A cookie is set using the `setcookie()` function. Since cookies are part of the HTTP header, you must set any cookies before sending any output to the browser.

Example:

```php
setcookie("user", "Tom", time() + 86400, "/"); // check syntax for function params

```

Description:

- Creates a cookie with name `user`
- (Optional) Value of the cookie is `Tom`
- (Optional) Cookie will expire in 1 day (86400 seconds)
- (Optional) Cookie is available throughout the whole website `/`
- (Optional) Cookie is only sent over HTTPS
- (Optional) Cookie is not accessible to scripting languages such as JavaScript

> 
A created or modified cookie can  only be accessed on subsequent requests (where `path` and `domain` matches) as the superglobal `$_COOKIE`is not populated with the new data immediately.




## Checking if a Cookie is Set


Use the `isset()` function upon the superglobal `$_COOKIE` variable to check if a cookie is set.

Example:

```php
// PHP <7.0
if (isset($_COOKIE['user'])) {
    // true, cookie is set
    echo 'User is ' . $_COOKIE['user'];
else {
    // false, cookie is not set
    echo 'User is not logged in';
}

// PHP 7.0+
echo 'User is ' . $_COOKIE['user'] ?? 'User is not logged in'; 

```



## Removing a Cookie


To remove a cookie, set the expiry timestamp to a time in the past. This triggers the browser's removal mechanism:

```php
setcookie('user', '', time() - 3600, '/');

```

> 
When deleting a cookie make sure the `path` and `domain` parameters of `setcookie()` matches the cookie you're trying to delete or a new cookie, which expires immediately, will be created.


It is also a good idea to unset the `$_COOKIE` value in case the current page uses it:

```php
unset($_COOKIE['user']);

```



## Retrieving a Cookie


****Retrieve and Output a Cookie Named `user`****

The value of a cookie can be retrieved using the global variable `$_COOKIE`. example if we have a cookie named `user` we can retrieve it like this

```php
echo $_COOKIE['user'];

```



#### Syntax


- `bool setcookie( string $name [, string $value = "" [, int $expire = 0 [, string $path = "" [, string $domain = "" [, bool $secure = false [, bool $httponly = false ]]]]]] )`



#### Parameters


|parameter|detail
|---|---|---|---|---|---|---|---|---|---
|name|The name of the cookie. This is also the key you can use to retrieve the value from the `$_COOKIE` super global. **This is the only required parameter**
|value|The value to store in the cookie. This data is accessible to the browser so don't store anything sensitive here.
|expire|A Unix timestamp representing when the cookie should expire. If set to zero the cookie will expire at the end of the session. If set to a number less than the current Unix timestamp the cookie will expire immediately.
|path|The scope of the cookie. If set to `/` the cookie will be available within the entire domain. If set to `/some-path/` then the cookie will only be available in that path and descendants of that path. Defaults to the current path of the file that the cookie is being set in.
|domain|The domain or subdomain the cookie is available on. If set to the bare domain `stackoverflow.com` then the cookie will be available to that domain and all subdomains. If set to a subdomain `meta.stackoverflow.com` then the cookie will be available only on that subdomain, and all sub-subdomains.
|secure|When set to `TRUE` the cookie will only be set if a secure HTTPS connection exists between the client and the server.
|httponly|Specifies that the cookie should only be made available through the HTTP/S protocol and should not be available to client side scripting languages like JavaScript. Only available in PHP 5.2 or later.



#### Remarks


It is worth noting that mere invoking `setcookie` function doesn't just put given data into `$_COOKIE` superglobal array.

For example there is no point in doing:

```php
setcookie("user", "Tom", time() + 86400, "/");
var_dump(isset($_COOKIE['user'])); // yields false or the previously set value

```

The value is not there yet, not until next page load. The function `setcookie` just says "**with next http connection tell the client (browser) to set this cookie**". Then when the headers are sent to the browser, they contain this cookie header. The browser then checks if the cookie hasn't expired yet, and if not, then in http request it sends the cookie to the server and that's when PHP receives it and puts the contents into `$_COOKIE` array.

