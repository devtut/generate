---
metaTitle: "Cookies"
description: "Adding and Setting Cookies, Reading cookies, Removing cookies, Test if cookies are enabled"
---

# Cookies



## Adding and Setting Cookies


The following variables set up the below example:

```js
var COOKIE_NAME = "Example Cookie";    /* The cookie's name. */
var COOKIE_VALUE = "Hello, world!";    /* The cookie's value. */
var COOKIE_PATH = "/foo/bar";          /* The cookie's path. */
var COOKIE_EXPIRES;                    /* The cookie's expiration date (config'd below). */

/* Set the cookie expiration to 1 minute in future (60000ms = 1 minute). */
COOKIE_EXPIRES = (new Date(Date.now() + 60000)).toUTCString();

```

```js
document.cookie += 
  COOKIE_NAME + "=" + COOKIE_VALUE
  + "; expires=" + COOKIE_EXPIRES
  + "; path=" + COOKIE_PATH;

```



## Reading cookies


```js
var name = name + "=",
    cookie_array = document.cookie.split(';'),
    cookie_value;
for(var i=0;i<cookie_array.length;i++) {
    var cookie=cookie_array[i];
    while(cookie.charAt(0)==' ')
        cookie = cookie.substring(1,cookie.length);
    if(cookie.indexOf(name)==0)
        cookie_value = cookie.substring(name.length,cookie.length);
    }

```

This will set `cookie_value` to the value of the cookie, if it exists. If the cookie is not set, it will set `cookie_value` to `null`



## Removing cookies


```js
var expiry = new Date();
expiry.setTime(expiry.getTime() - 3600);
document.cookie = name + "=; expires=" + expiry.toGMTString() + "; path=/"

```

This will remove the cookie with a given `name`.



## Test if cookies are enabled


If you want to make sure cookies are enabled before using them, you can use `navigator.cookieEnabled`:

```js
if (navigator.cookieEnabled === false)
{
    alert("Error: cookies not enabled!");
}

```

Note that on older browsers `navigator.cookieEnabled` may not exist and be undefined. In those cases you won't detect that cookies are not enabled.

