---
metaTitle: "PHP - Reading Request Data"
description: "Reading raw POST data, Reading POST data, Reading GET data, Handling file upload errors, Uploading files with HTTP PUT, Passing arrays by POST"
---

# Reading Request Data



## Reading raw POST data


Usually data sent in a POST request is structured key/value pairs with a MIME type of `application/x-www-form-urlencoded`. However many applications such as web services require raw data, often in XML or JSON format, to be sent instead. This data can be read using one of two methods.

**`php://input`** is a stream that provides access to the raw request body.

```php
$rawdata = file_get_contents("php://input");
// Let's say we got JSON
$decoded = json_decode($rawdata);

```

**`$HTTP_RAW_POST_DATA`** is a global variable that contains the raw POST data. It is only available if the `always_populate_raw_post_data` directive in `php.ini` is enabled.

```php
$rawdata = $HTTP_RAW_POST_DATA;
// Or maybe we get XML
$decoded = simplexml_load_string($rawdata);

```

This variable has been deprecated since PHP version 5.6, and was removed in PHP 7.0.

Note that neither of these methods are available when the content type is set to `multipart/form-data`, which is used for file uploads.



## Reading POST data


Data from a POST request is stored in the [superglobal](http://php.net/manual/en/language.variables.superglobals.php) `$_POST` in the form of an associative array.

Note that accessing a non-existent array item generates a notice, so existence should always be checked with the `isset()` or `empty()` functions, or the null coalesce operator.

Example:

```php
$from = isset($_POST["name"]) ? $_POST["name"] : "NO NAME";
$message = isset($_POST["message"]) ? $_POST["message"] : "NO MESSAGE";

echo "Message from $from: $message";

```

```php
$from = $_POST["name"] ?? "NO NAME";
$message = $_POST["message"] ?? "NO MESSAGE";

echo "Message from $from: $message";

```



## Reading GET data


Data from a GET request is stored in the [superglobal](http://php.net/manual/en/language.variables.superglobals.php) `$_GET` in the form of an associative array.

Note that accessing a non-existent array item generates a notice, so existence should always be checked with the `isset()` or `empty()` functions, or the null coalesce operator.

Example: (for URL `/topics.php?author=alice&topic=php`)

```php
$author = isset($_GET["author"]) ? $_GET["author"] : "NO AUTHOR";
$topic = isset($_GET["topic"]) ? $_GET["topic"] : "NO TOPIC";

echo "Showing posts from $author about $topic";

```

```php
$author = $_GET["author"] ?? "NO AUTHOR";
$topic = $_GET["topic"] ?? "NO TOPIC";

echo "Showing posts from $author about $topic";

```



## Handling file upload errors


The `$_FILES["FILE_NAME"]['error']` (where `"FILE_NAME"` is the value of the name attribute of the file input, present in your form) might contain one of the following values:

1. `UPLOAD_ERR_OK` - There is no error, the file uploaded with success.
1. `UPLOAD_ERR_INI_SIZE` - The uploaded file exceeds the upload_max_filesize directive in `php.ini`.
1. `UPLOAD_ERR_PARTIAL` - The uploaded file exceeds the MAX_FILE_SIZE directive that was specified in the HTML form.
1. `UPLOAD_ERR_NO_FILE` - No file was uploaded.
1. `UPLOAD_ERR_NO_TMP_DIR` - Missing a temporary folder. (From PHP 5.0.3).
1. `UPLOAD_ERR_CANT_WRITE` - Failed to write file to disk. (From PHP 5.1.0).
1. `UPLOAD_ERR_EXTENSION` - A PHP extension stopped the file upload. (From PHP 5.2.0).

An basic way to check for the errors, is as follows:

```php
<?php
$fileError = $_FILES["FILE_NAME"]["error"]; // where FILE_NAME is the name attribute of the file input in your form
switch($fileError) {
    case UPLOAD_ERR_INI_SIZE:
        // Exceeds max size in php.ini
        break;
    case UPLOAD_ERR_PARTIAL:
        // Exceeds max size in html form
        break;
    case UPLOAD_ERR_NO_FILE:
        // No file was uploaded
        break;
    case UPLOAD_ERR_NO_TMP_DIR:
        // No /tmp dir to write to
        break;
    case UPLOAD_ERR_CANT_WRITE:
        // Error writing to disk
        break;
    default:
        // No error was faced! Phew!
        break;
}

```



## Uploading files with HTTP PUT


[PHP provides support](http://php.net/manual/en/features.file-upload.put-method.php) for the HTTP PUT method used by some clients to store files on a server. PUT requests are much simpler than a file upload using POST requests and they look something like this:

```php
PUT /path/filename.html HTTP/1.1

```

Into your PHP code you would then do something like this:

```php
<?php
/* PUT data comes in on the stdin stream */
$putdata = fopen("php://input", "r");

/* Open a file for writing */
$fp = fopen("putfile.ext", "w");

/* Read the data 1 KB at a time
   and write to the file */
while ($data = fread($putdata, 1024))
  fwrite($fp, $data);

/* Close the streams */
fclose($fp);
fclose($putdata);
?>

```

Also [here](http://stackoverflow.com/questions/12005790/how-to-receive-a-file-via-http-put-with-php) you can read interesting SO question/answers about receiving file via HTTP PUT.



## Passing arrays by POST


Usually, an HTML form element submitted to PHP results in a single value. For example:

```php
<pre>
<?php print_r($_POST);?>
</pre>
<form method="post">
    <input type="hidden" name="foo" value="bar"/>
    <button type="submit">Submit</button>
</form>

```

This results in the following output:

```php
Array
(
    [foo] => bar
)

```

However, there may be cases where you want to pass an array of values. This can be done by adding a PHP-like suffix to the name of the HTML elements:

```php
<pre>
<?php print_r($_POST);?>
</pre>
<form method="post">
    <input type="hidden" name="foo[]" value="bar"/>
    <input type="hidden" name="foo[]" value="baz"/>
    <button type="submit">Submit</button>
</form>

```

This results in the following output:

```php
Array
(
    [foo] => Array
        (
            [0] => bar
            [1] => baz
        )

)

```

You can also specify the array indices, as either numbers or strings:

```php
<pre>
<?php print_r($_POST);?>
</pre>
<form method="post">
    <input type="hidden" name="foo[42]" value="bar"/>
    <input type="hidden" name="foo[foo]" value="baz"/>
    <button type="submit">Submit</button>
</form>

```

Which returns this output:

```php
Array
(
    [foo] => Array
        (
            [42] => bar
            [foo] => baz
        )

)

```

This technique can be used to avoid post-processing loops over the `$_POST` array, making your code leaner and more concise.



#### Remarks


### Choosing between GET and POST

**GET** requests, are best for providing data that's needed to render the page and may be used multiple times (search queries, data filters...). They are a part of the URL, meaning that they can be bookmarked and are often reused.

**POST** requests on the other hand, are meant for submitting data to the server just once (contact forms, login forms...). Unlike GET, which only accepts ASCII, POST requests also allow binary data, including [file uploads](http://stackoverflow.com/documentation/php/2295/file-uploads).

You can find a more detailed explanation of their differences [here](http://www.w3schools.com/tags/ref_httpmethods.asp).

### Request Data Vulnerabilities

Also look at: [what are the vulnerabilities in direct use of GET and POST?](http://stackoverflow.com/questions/1301863/what-are-the-vulnerabilities-in-direct-use-of-get-and-post)

Retrieving data from the $_GET and $_POST superglobals without any validation is considered bad practice, and opens up methods for users to potentially access or compromise data through [code](https://www.owasp.org/index.php/Code_Injection) and or [SQL injections](http://stackoverflow.com/documentation/php/2781/security/9372/sql-injection-prevention#t=201607231513063494449). Invalid data should be checked for and rejected as to prevent such attacks.

Request data should be escaped depending on how it is being used in code, as noted [here](http://stackoverflow.com/a/130323/2104168) and [here](http://stackoverflow.com/a/4224002/2104168). A few different escape functions for common data use cases can be found in [this answer](http://stackoverflow.com/a/1206461/2104168).

