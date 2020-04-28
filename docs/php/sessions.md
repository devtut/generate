---
metaTitle: "Sessions"
description: "session_start() Options, Session Locking, Manipulating session data, Destroy an entire session, Session name, Safe Session Start With no Errors"
---

# Sessions




## session_start() Options


Starting with PHP Sessions we can pass an array with [session-based `php.ini` options](http://php.net/manual/en/ini.list.php) to the `session_start` function.

**Example**

```php
<?php
   if (version_compare(PHP_VERSION, '7.0.0') >= 0) {
       // php >= 7 version
       session_start([
          'cache_limiter' => 'private',
          'read_and_close' => true,
       ]);
   } else {
       // php < 7 version
       session_start();
   }
?>

```

This feature also introduces a new `php.ini` setting named `session.lazy_write`, which defaults to `true` and means that session data is only rewritten, if it changes.

Referencing: [https://wiki.php.net/rfc/session-lock-ini](https://wiki.php.net/rfc/session-lock-ini)



## Session Locking


As we all are aware that PHP writes session data into a file at server side. When a request is made to php script which starts the session via `session_start()`, PHP locks this session file resulting to block/wait other incoming requests for same `session_id` to complete, because of which the other requests will get stuck on `session_start()` until or unless the **session file locked** is not released

The session file remains locked until the script is completed or session is manually closed. To avoid this situation **i.e. to prevent multiple requests getting blocked**, we can start the session and close the session which will release the lock from session file and allow to continue the remaining requests.

```php
// php < 7.0 
// start session 
session_start();

// write data to session
$_SESSION['id'] = 123; // session file is locked, so other requests are blocked

// close the session, release lock
session_write_close();

```

Now one will think if session is closed how we will read the session values, beautify even after session is closed, session is still available. So, we can still read the session data.

```php
echo $_SESSION['id'];    // will output  123

```

In **php >= 7.0**, we can have **read_only** session, **read_write** session and **lazy_write** session, so it may not required to use `session_write_close()`



## Manipulating session data


The `$_SESSION` variable is an array, and you can retrieve or manipulate it like a normal array.

```php
<?php
// Starting the session
session_start();

// Storing the value in session
$_SESSION['id'] = 342;

// conditional usage of session values that may have been set in a previous session
if(!isset($_SESSION["login"])) {
    echo "Please login first";
    exit;
}
// now you can use the login safely
$user = $_SESSION["login"];

// Getting a value from the session data, or with default value, 
//     using the Null Coalescing operator in PHP 7
$name = $_SESSION['name'] ?? 'Anonymous';

```

Also see [Manipulating an Array](http://stackoverflow.com/documentation/php/6825/manipulating-an-array) for more reference how to work on an array.

Note that if you store an object in a session, it can be retrieved gracefully only if you have an class autoloader or you have loaded the class already. Otherwise, the object will come out as the type `__PHP_Incomplete_Class`, which may later lead to [crashes](http://stackoverflow.com/q/1055728/3990767). See [Namespacing and Autoloading](http://stackoverflow.com/documentation/php/504/classes-and-objects/6315/namespacing-and-autoloading#t=201611011543500298544) about autoloading.

### Warning:

Session data can be hijacked.  This is outlined in: **[Pro PHP Security: From Application Security Principles to the Implementation of XSS Defense - Chapter 7: Preventing Session Hijacking](https://books.google.com.au/books?id=EUc6NlZRDqcC&pg=PA97#v=onepage&q&f=false)** So it can be strongly recommended to never store any personal information in `$_SESSION`. This would most critically include **credit card numbers**, **government issued ids**, and **passwords**; but would also extend into less assuming data like **names**, **emails**, **phone numbers**, etc which would allow a hacker to impersonate/compromise a legitimate user.  As a general rule, use worthless/non-personal values, such as numerical identifiers, in session data.



## Destroy an entire session


If you've got a session which you wish to destroy, you can do this with [`session_destroy()`](http://php.net/session_destroy)

```php
/*
    Let us assume that our session looks like this:
    Array([firstname] => Jon, [id] => 123)

    We first need to start our session:
*/
session_start();

/*
    We can now remove all the values from the `SESSION` superglobal:
    If you omitted this step all of the global variables stored in the 
    superglobal would still exist even though the session had been destroyed.
*/
$_SESSION = array();

// If it's desired to kill the session, also delete the session cookie.
// Note: This will destroy the session, and not just the session data!
if (ini_get("session.use_cookies")) {
    $params = session_get_cookie_params();
    setcookie(session_name(), '', time() - 42000,
        $params["path"], $params["domain"],
        $params["secure"], $params["httponly"]
    );
}

//Finally we can destroy the session:
session_destroy();

```

Using `session_destroy()` is different to using something like `$_SESSION = array();` which will remove all of the values stored in the `SESSION` superglobal but it will not destroy the actual stored version of the session.

**Note**: We use `$_SESSION = array();` instead of `session_unset()` because [the manual](http://php.net/session_destroy) stipulates:

> 
Only use session_unset() for older deprecated code that does not use $_SESSION.




## Session name


### Checking if session cookies have been created

Session name is the name of the cookie used to store sessions. You can use this to detect if cookies for a session have been created for the user:

```php
if(isset($_COOKIE[session_name()])) {
    session_start();
}

```

Note that this method is generally not useful unless you really don't want to create cookies unnecessarily.

### Changing session name

You can update the session name by calling `session_name()`.

```php
//Set the session name
session_name('newname');
//Start the session
session_start();

```

If no argument is provided into `session_name()` then the current session name is returned.

> 
<p>It should contain only alphanumeric characters; it should be short and descriptive (i.e. for users with enabled cookie warnings).
The session name can't consist of digits only, at least one letter must be present. Otherwise a new session id is generated every time.</p>




## Safe Session Start With no Errors


Many developers have this problem when they work on huge projects, especially if they work on some modular CMS on plugins, addons, components etc. Here is solution for safe session start where if first checked PHP version to cover all versions and on next is checked if session is started. If session not exists then I start session safe. If session exists nothing happen.

```php
if (version_compare(PHP_VERSION, '7.0.0') >= 0) {
    if(session_status() == PHP_SESSION_NONE) {
        session_start(array(
          'cache_limiter' => 'private',
          'read_and_close' => true,
       ));
    }
}
else if (version_compare(PHP_VERSION, '5.4.0') >= 0)
{
    if (session_status() == PHP_SESSION_NONE) {
        session_start();
    }
}
else
{
    if(session_id() == '') {
        session_start();
    }
}

```

This can help you a lot to avoid `session_start` error.



#### Syntax


- void session_abort ( void )
- int session_cache_expire ([ string $new_cache_expire ] )
- void session_commit ( void )
- string session_create_id ([ string $prefix ] )
- bool session_decode ( string $data )
- bool session_destroy ( void )
- string session_encode ( void )
- int session_gc ( void )
- array session_get_cookie_params ( void )
- string session_id ([ string $id ] )
- bool session_is_registered ( string $name )
- string session_module_name ([ string $module ] )
- string session_name ([ string $name ] )
- bool session_regenerate_id ([ bool $delete_old_session = false ] )
- void session_register_shutdown ( void )
- bool session_register ( mixed $name [, mixed $... ] )
- void session_reset ( void )
- string session_save_path ([ string $path ] )
- void session_set_cookie_params ( int $lifetime [, string $path [, string $domain [, bool $secure = false [, bool $httponly = false ]]]] )
- bool session_set_save_handler ( callable $open , callable $close , callable $read , callable $write , callable $destroy , callable $gc [, callable $create_sid [, callable $validate_sid [, callable $update_timestamp ]]] )
- bool session_start ([ array $options = [] ] )
- int session_status ( void )
- bool session_unregister ( string $name )
- void session_unset ( void )
- void session_write_close ( void )



#### Remarks


Note that calling `session_start()` even if the session has already started will result in a PHP warning.

