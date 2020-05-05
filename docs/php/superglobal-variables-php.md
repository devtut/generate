---
metaTitle: "PHP - Superglobal Variables PHP"
description: "Suberglobals explained, PHP5 SuperGlobals"
---

# Superglobal Variables PHP


Superglobals are built-in variables that are always available in all scopes.

Several predefined variables in PHP are "superglobals", which means they are available in all scopes throughout a script. There is no need to do `global $variable;` to access them within functions or methods.



## Suberglobals explained


### Introduction

Put simply, these are variables that are available in **all** scope in your scripts.

This means that there is no need to pass them as parameters in your functions, or store them outside a block of code to have them available in different scopes.

### What's a superglobal??

If you're thinking that these are like superheroes - they're not.

As of PHP version 7.1.3 there are 9 superglobal variables. They are as follows:

- `$GLOBALS` - References all variables available in global scope
- `$_SERVER` - Server and execution environment information
- `$_GET` - HTTP GET variables
- `$_POST` - HTTP POST variables
- `$_FILES` - HTTP File Upload variables
- `$_COOKIE` - HTTP Cookies
- `$_SESSION` - Session variables
- `$_REQUEST` - HTTP Request variables
- `$_ENV` - Environment variables

See the [documentation](http://php.net/manual/en/language.variables.superglobals.php).

### Tell me more, tell me more

<sub>I'm sorry for the Grease reference! [Link](https://www.youtube.com/watch?v=ZW0DfsCzfq4)</sub>

Time for some explanation on these super<strike>heroes</strike>globals.

### `$GLOBALS`

> 
An associative array containing references to all variables which are currently defined in the global scope of the script. The variable names are the keys of the array.


Code

```php
$myGlobal = "global"; // declare variable outside of scope

function test()
{
    $myLocal = "local"; // declare variable inside of scope
    // both variables are printed
    var_dump($myLocal);
    var_dump($GLOBALS["myGlobal"]);
}

test(); // run function
// only $myGlobal is printed since $myLocal is not globally scoped
var_dump($myLocal);
var_dump($myGlobal);

```

Output

```php
string 'local' (length=5)
string 'global' (length=6)
null
string 'global' (length=6)

```

In the above example `$myLocal` is not displayed the second time because it is declared inside the `test()` function and then destroyed after the function is closed.

### Becoming global

To remedy this there are two options.

Option one: **`global` keyword**

```php
function test()
{
    global $myLocal;
    $myLocal = "local";
    var_dump($myLocal);
    var_dump($GLOBALS["myGlobal"]);
}

```

The `global` keyword is a prefix on a variable that forces it to be part of the global scope.

Note that you cannot assign a value to a variable in the same statement as the global keyword. Hence, why I had to assign a value underneath. (It is possible if you remove new lines and spaces but I don't think it is neat. `global $myLocal; $myLocal = "local"`).

Option two: **`$GLOBALS` array**

```php
function test()
{
    $GLOBALS["myLocal"] = "local";
    $myLocal = $GLOBALS["myLocal"];
    var_dump($myLocal);
    var_dump($GLOBALS["myGlobal"]);
}

```

In this example I reassigned `$myLocal` the value of `$GLOBAL["myLocal"]` since I find it easier writing a variable name rather than the associative array.

### `$_SERVER`

> 
$_SERVER is an array containing information such as headers, paths, and script locations. The entries in this array are created by the web server. There is no guarantee that every web server will provide any of these; servers may omit some, or provide others not listed here. That said, a large number of these variables are accounted for in the [CGI/1.1 specification](http://www.faqs.org/rfcs/rfc3875), so you should be able to expect those.


An example output of this might be as follows (run on my Windows PC using WAMP)

```php
C:\wamp64\www\test.php:2:
array (size=36)
    'HTTP_HOST' => string 'localhost' (length=9)
    'HTTP_CONNECTION' => string 'keep-alive' (length=10)
    'HTTP_CACHE_CONTROL' => string 'max-age=0' (length=9)
    'HTTP_UPGRADE_INSECURE_REQUESTS' => string '1' (length=1)
    'HTTP_USER_AGENT' => string 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36' (length=110)
    'HTTP_ACCEPT' => string 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' (length=74)
    'HTTP_ACCEPT_ENCODING' => string 'gzip, deflate, sdch, br' (length=23)
    'HTTP_ACCEPT_LANGUAGE' => string 'en-US,en;q=0.8,en-GB;q=0.6' (length=26)
    'HTTP_COOKIE' => string 'PHPSESSID=0gslnvgsci371ete9hg7k9ivc6' (length=36)
    'PATH' => string 'C:\Program Files (x86)\NVIDIA Corporation\PhysX\Common;C:\Program Files (x86)\Intel\iCLS Client\;C:\Program Files\Intel\iCLS Client\;C:\ProgramData\Oracle\Java\javapath;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;E:\Program Files\ATI Technologies\ATI.ACE\Core-Static;E:\Program Files\AMD\ATI.ACE\Core-Static;C:\Program Files (x86)\AMD\ATI.ACE\Core-Static;C:\Program Files (x86)\ATI Technologies\ATI.ACE\Core-Static;C:\Program Files\Intel\Intel(R) Managemen'... (length=1169)
    'SystemRoot' => string 'C:\WINDOWS' (length=10)
    'COMSPEC' => string 'C:\WINDOWS\system32\cmd.exe' (length=27)
    'PATHEXT' => string '.COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC;.PY' (length=57)
    'WINDIR' => string 'C:\WINDOWS' (length=10)
    'SERVER_SIGNATURE' => string '<address>Apache/2.4.23 (Win64) PHP/7.0.10 Server at localhost Port 80</address>' (length=80)
    'SERVER_SOFTWARE' => string 'Apache/2.4.23 (Win64) PHP/7.0.10' (length=32)
    'SERVER_NAME' => string 'localhost' (length=9)
    'SERVER_ADDR' => string '::1' (length=3)
    'SERVER_PORT' => string '80' (length=2)
    'REMOTE_ADDR' => string '::1' (length=3)
    'DOCUMENT_ROOT' => string 'C:/wamp64/www' (length=13)
    'REQUEST_SCHEME' => string 'http' (length=4)
    'CONTEXT_PREFIX' => string '' (length=0)
    'CONTEXT_DOCUMENT_ROOT' => string 'C:/wamp64/www' (length=13)
    'SERVER_ADMIN' => string 'wampserver@wampserver.invalid' (length=29)
    'SCRIPT_FILENAME' => string 'C:/wamp64/www/test.php' (length=26)
    'REMOTE_PORT' => string '5359' (length=4)
    'GATEWAY_INTERFACE' => string 'CGI/1.1' (length=7)
    'SERVER_PROTOCOL' => string 'HTTP/1.1' (length=8)
    'REQUEST_METHOD' => string 'GET' (length=3)
    'QUERY_STRING' => string '' (length=0)
    'REQUEST_URI' => string '/test.php' (length=13)
    'SCRIPT_NAME' => string '/test.php' (length=13)
    'PHP_SELF' => string '/test.php' (length=13)
    'REQUEST_TIME_FLOAT' => float 1491068771.413
    'REQUEST_TIME' => int 1491068771

```

There is a lot to take in there so I will pick out some important ones below. If you wish to read about them all then consult the [indices section](http://php.net/manual/en/reserved.variables.server.php#refsect1-reserved.variables.server-indices) of the documentation.

<sub>I might add them all below one day. Or someone can edit and add a ****good**** explanation of them below? **Hint, hint**;)</sub>

For all explanations below, assume the URL is [http://www.example.com/index.php](http://www.example.com/index.php)

<li>`HTTP_HOST` - The host address.<br />
This would return `www.example.com`</li>
- `HTTP_USER_AGENT` - Contents of the user agent. This is a string which contains all the information about the client's browser, including operating system.
- `HTTP_COOKIE` - All cookies in a concatenated string, with a semi-colon delimiter.
<li>`SERVER_ADDR` - The IP address of the server, of which the current script is running.<br />
This would return `93.184.216.34`</li>
<li>`PHP_SELF` - The file name of the currently executed script, relative to document root.<br />
This would return `/index.php`</li>
- `REQUEST_TIME_FLOAT` - The timestamp of the start of the request, with microsecond precision. Available since PHP 5.4.0.
- `REQUEST_TIME`  - The timestamp of the start of the request. Available since PHP 5.1.0.

### `$_GET`

> 
An associative array of variables passed to the current script via the URL parameters.


`$_GET` is an array that contains all the URL parameters; these are the whatever is after the ? in the URL.

Using [http://www.example.com/index.php?myVar=myVal](http://www.example.com/index.php?myVar=myVal) as an example. This information from this URL can be obtained by accessing in this format `$_GET["myVar"]` and the result of this will be `myVal`.

Using some code for those that don't like reading.

```php
// URL = http://www.example.com/index.php?myVar=myVal
echo $_GET["myVar"] == "myVal" ? "true" : "false"; // returns "true"

```

The above example makes use of the [ternary operator](http://stackoverflow.com/documentation/php/1687/operators/7608/ternary-operator#t=201704011912247393013).

This shows how you can access the value from the URL using the `$_GET` superglobal.

Now another example! **gasp**

```php
// URL = http://www.example.com/index.php?myVar=myVal&myVar2=myVal2
echo $_GET["myVar"]; // returns "myVal"
echo $_GET["myVar2"]; // returns "myVal2"

```

It is possible to send multiple variables through the URL by separating them with an ampersand (`&`) character.

**Security risk**<br />
It is very important not to send any sensitive information via the URL as it will stay in history of the computer and will be visible to anyone that can access that browser.

### `$_POST`

> 
An associative array of variables passed to the current script via the HTTP POST method when using application/x-www-form-urlencoded or multipart/form-data as the HTTP Content-Type in the request.


Very similar to `$_GET` in that data is sent from one place to another.

I'll start by going straight into an example. (I have omitted the action attribute as this will send the information to the page that the form is in).

```php
<form method="POST">
    <input type="text" name="myVar" value="myVal" />
    <input type="submit" name="submit" value="Submit" />
</form>

```

Above is a basic form for which data can be sent. In an real environment the `value` attribute would not be set meaning the form would be blank. This would then send whatever information is entered by the user.

```php
echo $_POST["myVar"]); // returns "myVal"

```

**Security risk**<br />
Sending data via POST is also not secure. Using HTTPS will ensure that data is kept more secure.

### `$_FILES`

> 
An associative array of items uploaded to the current script via the HTTP POST method. The structure of this array is outlined in the [POST method uploads](http://php.net/manual/en/features.file-upload.post-method.php) section.


Let's start with a basic form.

```php
<form method="POST" enctype="multipart/form-data">
    <input type="file" name="myVar" />
    <input type="submit" name="Submit" />
</form>

```

Note that I omitted the `action` attribute (again!). Also, I added `enctype="multipart/form-data"`, this is important to any form that will be dealing with file uploads.

```php
// ensure there isn't an error
if ($_FILES["myVar"]["error"] == UPLOAD_ERR_OK)
{
    $folderLocation = "myFiles"; // a relative path. (could be "path/to/file" for example)
    
    // if the folder doesn't exist then make it
    if (!file_exists($folderLocation)) mkdir($folderLocation);

    // move the file into the folder
    move_uploaded_file($_FILES["myVar"]["tmp_name"], "$folderLocation/" . basename($_FILES["myVar"]["name"]));
}

```

This is used to upload one file. Sometimes you may wish to upload more than one file. An attribute exists for that, it's called `multiple`.<br />
There's an attribute for just about **anything**. <sub>[I'm sorry](https://www.youtube.com/watch?v=szrsfeyLzyg)</sub>

Below is an example of a form submitting multiple files.

```php
<form method="POST" enctype="multipart/form-data">
    <input type="file" name="myVar[]" multiple="multiple" />
    <input type="submit" name="Submit" />
</form>

```

Note the changes made here; there are only a few.

- The `input` name has square brackets. This is because it is now an array of files and so we are telling the form to make an array of the files selected. Omitting the square brackets will result in the latter most file being set to `$_FILES["myVar"]`.
- The `multiple="multiple"` attribute. This just tells the browser that users can select more than one file.

```php
$total = isset($_FILES["myVar"]) ? count($_FILES["myVar"]["name"]) : 0; // count how many files were sent
// iterate over each of the files
for ($i = 0; $i < $total; $i++)
{
    // there isn't an error
    if ($_FILES["myVar"]["error"][$i] == UPLOAD_ERR_OK)
    {
        $folderLocation = "myFiles"; // a relative path. (could be "path/to/file" for example)
        
        // if the folder doesn't exist then make it
        if (!file_exists($folderLocation)) mkdir($folderLocation);

        // move the file into the folder
        move_uploaded_file($_FILES["myVar"]["tmp_name"][$i], "$folderLocation/" . basename($_FILES["myVar"]["name"][$i]));
    }
    // else report the error
    else switch ($_FILES["myVar"]["error"][$i])
    {
        case UPLOAD_ERR_INI_SIZE:
            echo "Value: 1; The uploaded file exceeds the upload_max_filesize directive in php.ini.";
            break;
        case UPLOAD_ERR_FORM_SIZE:
            echo "Value: 2; The uploaded file exceeds the MAX_FILE_SIZE directive that was specified in the HTML form.";
            break;
        case UPLOAD_ERR_PARTIAL:
            echo "Value: 3; The uploaded file was only partially uploaded.";
            break;
        case UPLOAD_ERR_NO_FILE:
            echo "Value: 4; No file was uploaded.";
            break;
        case UPLOAD_ERR_NO_TMP_DIR:
            echo "Value: 6; Missing a temporary folder. Introduced in PHP 5.0.3.";
            break;
        case UPLOAD_ERR_CANT_WRITE:
            echo "Value: 7; Failed to write file to disk. Introduced in PHP 5.1.0.";
            break;
        case UPLOAD_ERR_EXTENSION:
            echo "Value: 8; A PHP extension stopped the file upload. PHP does not provide a way to ascertain which extension caused the file upload to stop; examining the list of loaded extensions with phpinfo() may help. Introduced in PHP 5.2.0.";
            break;
        
        default:
            echo "An unknown error has occured.";
            break;
    }
}

```

This is a very simple example and doesn't handle problems such as file extensions that aren't allowed or files named with PHP code (like a PHP equivalent of an SQL injection).  See the [documentation](http://stackoverflow.com/documentation/php/2781/security/29134/uploading-files).

The first process is checking if there are any files, and if so, set the total number of them to `$total`.

Using the for loop allows an iteration of the `$_FILES` array and accessing each item one at a time. If that file doesn't encounter a problem then the if statement is true and the code from the single file upload is run.<br />
If an problem is encountered the switch block is executed and an error is presented in accordance with the error for that particular upload.

### `$_COOKIE`

> 
An associative array of variables passed to the current script via HTTP Cookies.


Cookies are variables that contain data and are stored on the client's computer.

Unlike the aforementioned superglobals, cookies must be created with a function (and not be assigning a value).  The convention is below.

```php
setcookie("myVar", "myVal", time() + 3600);

```

In this example a name is specified for the cookie (in this example it is "myVar"), a value is given (in this example it is "myVal", but a variable can be passed to assign its value to the cookie), and then an expiration time is given (in this example it is one hour since 3600 seconds is a minute).

Despite the convention for creating a cookie being different, it is accessed in the same way as the others.

```php
echo $_COOKIE["myVar"]; // returns "myVal"

```

To destroy a cookie, `setcookie` must be called again, but the expiration time is set to **any** time in the past. See below.

```php
setcookie("myVar", "", time() - 1);
var_dump($_COOKIE["myVar"]); // returns null 

```

This will unset the cookies and remove it from the clients computer.

### `$_SESSION`

> 
An associative array containing session variables available to the current script. See the [Session functions](http://php.net/manual/en/ref.session.php) documentation for more information on how this is used.


Sessions are much like cookies except they are server side.

To use sessions you must include `session_start()` at the top of your scripts to allow sessions to be utilised.

Setting a session variable is the same as setting any other variable. See example below.

```php
$_SESSION["myVar"] = "myVal";

```

When starting a session a random ID is set as a cookie and called "PHPSESSID" and will contain the session ID for that current session. This can be accessed by calling the `session_id()` function.

It is possible to destroy session variables using the `unset` function (such that `unset($_SESSION["myVar"])` would destroy that variable).<br />
The alternative is to call `session_destory()`. This will destroy the entire session meaning that **all** session variables will no longer exist.

### `$_REQUEST`

> 
An associative array that by default contains the contents of [`$_GET`](http://php.net/manual/en/reserved.variables.get.php), [`$_POST`](http://php.net/manual/en/reserved.variables.post.php) and [`$_COOKIE`](http://php.net/manual/en/reserved.variables.cookies.php).


As the PHP documentation states, this is just a collation of `$_GET`, `$_POST`, and `$_COOKIE` all in one variable.

Since it is possible for all three of those arrays to have an index with the same name, there is a setting in the `php.ini` file called `request_order` which can specify which of the three has precedence.<br />
For instance, if it was set to `"GPC"`, then the value of `$_COOKIE` will be used, as it is read from left to right meaning the `$_REQUEST` will set its value to `$_GET`, then `$_POST`, and then `$_COOKIE` and since `$_COOKIE` is last that is the value that is in `$_REQUEST`.<br />
See [this question](http://stackoverflow.com/questions/43157933/what-is-the-request-precedence).

### `$_ENV`

> 
An associative array of variables passed to the current script via the environment method.
These variables are imported into PHP's global namespace from the environment under which the PHP parser is running. Many are provided by the shell under which PHP is running and different systems are likely running different kinds of shells, a definitive list is impossible. Please see your shell's documentation for a list of defined environment variables.
Other environment variables include the CGI variables, placed there regardless of whether PHP is running as a server module or CGI processor.


Anything stored within `$_ENV` is from the environment from which PHP is running in.

`$_ENV` is only populated if `php.ini` allows it.<br />
See [this answer](http://stackoverflow.com/questions/3780866/why-is-my-env-empty/27077452#27077452) for more information on why `$_ENV` is not populated.



## PHP5 SuperGlobals


Below are the PHP5 SuperGlobals

- $GLOBALS
- $_REQUEST
- $_GET
- $_POST
- $_FILES
- $_SERVER
- $_ENV
- $_COOKIE
- $_SESSION

**$GLOBALS**: This SuperGlobal Variable is used for accessing globals variables.

```php
<?php    
$a = 10;    
function foo(){
    echo $GLOBALS['a'];
}    
//Which will print 10 Global Variable a
?>

```

**$_REQUEST**: This SuperGlobal Variable is used to collect data submitted by a HTML Form.

```php
<?php
if(isset($_REQUEST['user'])){
    echo $_REQUEST['user'];
}
//This will print value of HTML Field with name=user submitted using POST and/or GET MEthod
?>

```

**$_GET**: This SuperGlobal Variable is used to collect data submitted by HTML Form with `get` method.

```php
<?php
if(isset($_GET['username'])){
    echo $_GET['username'];
}
//This will print value of HTML field with name username submitted using GET Method
?>

```

**$_POST**: This SuperGlobal Variable is used to collect data submitted by HTML Form with `post` method.

```php
<?php
if(isset($_POST['username'])){
    echo $_POST['username'];
}
//This will print value of HTML field with name username submitted using POST Method
?>

```

**$_FILES**: This SuperGlobal Variable holds the information of uploaded files via HTTP Post method.

```php
<?php
if($_FILES['picture']){
    echo "<pre>";
    print_r($_FILES['picture']);
    echo "</pre>";
}
/**
This will print details of the File with name picture uploaded via a form with method='post and with enctype='multipart/form-data'
Details includes Name of file, Type of File, temporary file location, error code(if any error occured while uploading the file) and size of file in Bytes.
Eg.

Array
(
    [picture] => Array
        (
            [0] => Array
                (
                    [name] => 400.png
                    [type] => image/png
                    [tmp_name] => /tmp/php5Wx0aJ
                    [error] => 0
                    [size] => 15726
                )
        )
)

*/
?>

```

**$_SERVER**: This SuperGlobal Variable holds information about Scripts, HTTP Headers and Server Paths.

```php
<?php
    echo "<pre>";
    print_r($_SERVER);
    echo "</pre>";
    /**
    Will print the following details
    on my local XAMPP 
    Array
(
    [MIBDIRS] => C:/xampp/php/extras/mibs
    [MYSQL_HOME] => \xampp\mysql\bin
    [OPENSSL_CONF] => C:/xampp/apache/bin/openssl.cnf
    [PHP_PEAR_SYSCONF_DIR] => \xampp\php
    [PHPRC] => \xampp\php
    [TMP] => \xampp\tmp
    [HTTP_HOST] => localhost
    [HTTP_CONNECTION] => keep-alive
    [HTTP_CACHE_CONTROL] => max-age=0
    [HTTP_UPGRADE_INSECURE_REQUESTS] => 1
    [HTTP_USER_AGENT] => Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.82 Safari/537.36
    [HTTP_ACCEPT] => text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*;q=0.8
    [HTTP_ACCEPT_ENCODING] => gzip, deflate, sdch
    [HTTP_ACCEPT_LANGUAGE] => en-US,en;q=0.8
    [PATH] => C:\xampp\php;C:\ProgramData\ComposerSetup\bin;
    [SystemRoot] => C:\Windows
    [COMSPEC] => C:\Windows\system32\cmd.exe
    [PATHEXT] => .COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC
    [WINDIR] => C:\Windows
    [SERVER_SIGNATURE] => Apache/2.4.16 (Win32) OpenSSL/1.0.1p PHP/5.6.12 Server at localhost Port 80
    [SERVER_SOFTWARE] => Apache/2.4.16 (Win32) OpenSSL/1.0.1p PHP/5.6.12
    [SERVER_NAME] => localhost
    [SERVER_ADDR] => ::1
    [SERVER_PORT] => 80
    [REMOTE_ADDR] => ::1
    [DOCUMENT_ROOT] => C:/xampp/htdocs
    [REQUEST_SCHEME] => http
    [CONTEXT_PREFIX] => 
    [CONTEXT_DOCUMENT_ROOT] => C:/xampp/htdocs
    [SERVER_ADMIN] => postmaster@localhost
    [SCRIPT_FILENAME] => C:/xampp/htdocs/abcd.php
    [REMOTE_PORT] => 63822
    [GATEWAY_INTERFACE] => CGI/1.1
    [SERVER_PROTOCOL] => HTTP/1.1
    [REQUEST_METHOD] => GET
    [QUERY_STRING] => 
    [REQUEST_URI] => /abcd.php
    [SCRIPT_NAME] => /abcd.php
    [PHP_SELF] => /abcd.php
    [REQUEST_TIME_FLOAT] => 1469374173.88
    [REQUEST_TIME] => 1469374173
)
*/
?>

```

**$_ENV**: This SuperGlobal Variable Shell Environment Variable details under which the PHP is running.

**$_COOKIE**: This SuperGlobal Variable is used to retrieve Cookie value with given Key.

```php
<?php
$cookie_name = "data";
$cookie_value = "Foo Bar";
setcookie($cookie_name, $cookie_value, time() + (86400 * 30), "/"); // 86400 = 1 day
if(!isset($_COOKIE[$cookie_name])) {
    echo "Cookie named '" . $cookie_name . "' is not set!";
}
else {
    echo "Cookie '" . $cookie_name . "' is set!<br>";
    echo "Value is: " . $_COOKIE[$cookie_name];
}

/**
    Output
    Cookie 'data' is set!
    Value is: Foo Bar
*/
?>

```

**$_SESSION**: This SuperGlobal Variable is used to Set and Retrieve Session Value which is stored on Server.

```php
<?php
//Start the session
session_start();
/**
    Setting the Session Variables
    that can be accessed on different
    pages on save server.
*/
$_SESSION["username"] = "John Doe";
$_SESSION["user_token"] = "d5f1df5b4dfb8b8d5f";
echo "Session is saved successfully";

/**
    Output
    Session is saved successfully
*/
?>

```

