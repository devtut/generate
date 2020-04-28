---
metaTitle: "Using cURL in PHP"
description: "POST Requests, Basic Usage (GET Requests), Using Cookies, Using multi_curl to make multiple POST requests, Creating and sending a request with a custom method, Sending multi-dimensional data and multiple files with CurlFile in one request, Get and Set custom http headers in php"
---

# Using cURL in PHP




## POST Requests


If you want to mimic HTML form POST action, you can use cURL.

```
// POST data in array
$post = [
    'a' => 'apple',
    'b' => 'banana'
];

// Create a new cURL resource with URL to POST
$ch = curl_init('http://www.example.com');

// We set parameter CURLOPT_RETURNTRANSFER to read output
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

// Let's pass POST data
curl_setopt($ch, CURLOPT_POSTFIELDS, $post);

// We execute our request, and get output in a $response variable
$response = curl_exec($ch);

// Close the connection
curl_close($ch);

```



## Basic Usage (GET Requests)


cURL is a tool for transferring data with URL syntax. It support HTTP, FTP, SCP and many others(curl >= 7.19.4). **Remember, you need to [install and enable the cURL extension](http://php.net/manual/en/curl.installation.php) to use it.**

```
// a little script check is the cURL extension loaded or not
if(!extension_loaded("curl")) {
    die("cURL extension not loaded! Quit Now.");
}

// Actual script start

// create a new cURL resource
// $curl is the handle of the resource
$curl = curl_init();

// set the URL and other options
curl_setopt($curl, CURLOPT_URL, "http://www.example.com");

// execute and pass the result to browser
curl_exec($curl);

// close the cURL resource
curl_close($curl);

```



## Using Cookies


cURL can keep cookies received in responses for use with subsequent requests. For simple session cookie handling in memory, this is achieved with a single line of code:

```
curl_setopt($ch, CURLOPT_COOKIEFILE, "");

```

In cases where you are required to keep cookies after the cURL handle is destroyed, you can specify the file to store them in:

```
curl_setopt($ch, CURLOPT_COOKIEJAR, "/tmp/cookies.txt");

```

Then, when you want to use them again, pass them as the cookie file:

```
curl_setopt($ch, CURLOPT_COOKIEFILE, "/tmp/cookies.txt");

```

Remember, though, that these two steps are not necessary unless you need to carry cookies between different cURL handles. For most use cases, setting `CURLOPT_COOKIEFILE` to the empty string is all you need.

Cookie handling can be used, for example, to retrieve resources from a web site that requires a login. This is typically a two-step procedure. First, POST to the login page.

```
<?php

# create a cURL handle
$ch  = curl_init();

# set the URL (this could also be passed to curl_init() if desired)
curl_setopt($ch, CURLOPT_URL, "https://www.example.com/login.php");

# set the HTTP method to POST
curl_setopt($ch, CURLOPT_POST, true);

# setting this option to an empty string enables cookie handling
# but does not load cookies from a file
curl_setopt($ch, CURLOPT_COOKIEFILE, "");

# set the values to be sent
curl_setopt($ch, CURLOPT_POSTFIELDS, array(
    "username"=>"joe_bloggs",
    "password"=>"$up3r_$3cr3t",
));

# return the response body
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

# send the request
$result = curl_exec($ch);

```

The second step (after standard error checking is done) is usually a simple GET request. The important thing is to **reuse the existing cURL handle** for the second request. This ensures the cookies from the first response will be automatically included in the second request.

```
# we are not calling curl_init()

# simply change the URL
curl_setopt($ch, CURLOPT_URL, "https://www.example.com/show_me_the_foo.php");

# change the method back to GET
curl_setopt($ch, CURLOPT_HTTPGET, true);

# send the request
$result = curl_exec($ch);

# finished with cURL
curl_close($ch);

# do stuff with $result...

```

This is only intended as an example of cookie handling. In real life, things are usually more complicated. Often you must perform an initial GET of the login page to pull a login token that needs to be included in your POST. Other sites might block the cURL client based on its User-Agent string, requiring you to change it.



## Using multi_curl to make multiple POST requests


Sometimes we need to make a lot of POST requests to one or many different endpoints. To deal with this scenario, we can use `multi_curl`.

First of all, we create how many requests as needed exactly in the same way of the simple example and put them in an array.

We use the curl_multi_init and add each handle to it.

In this example, we are using 2 different endpoints:

```
//array of data to POST
$request_contents = array();
//array of URLs
$urls = array();
//array of cURL handles
$chs = array();

//first POST content
$request_contents[] = [
    'a' => 'apple',
    'b' => 'banana'
];
//second POST content
$request_contents[] = [
    'a' => 'fish',
    'b' => 'shrimp'
];
//set the urls
$urls[] = 'http://www.example.com';
$urls[] = 'http://www.example2.com';

//create the array of cURL handles and add to a multi_curl
$mh = curl_multi_init();
foreach ($urls as $key => $url) {
    $chs[$key] = curl_init($url);
    curl_setopt($chs[$key], CURLOPT_RETURNTRANSFER, true);
    curl_setopt($chs[$key], CURLOPT_POST, true);
    curl_setopt($chs[$key], CURLOPT_POSTFIELDS, $request_contents[$key]);

    curl_multi_add_handle($mh, $chs[$key]);
}

```

Then, we use curl_multi_exec to send the requests

```
//running the requests
$running = null;
do {
  curl_multi_exec($mh, $running);
} while ($running);

//getting the responses
foreach(array_keys($chs) as $key){
    $error = curl_error($chs[$key]);
    $last_effective_URL = curl_getinfo($chs[$key], CURLINFO_EFFECTIVE_URL);
    $time = curl_getinfo($chs[$key], CURLINFO_TOTAL_TIME);
    $response = curl_multi_getcontent($chs[$key]);  // get results
    if (!empty($error)) {
      echo "The request $key return a error: $error" . "\n";
    }
    else {
      echo "The request to '$last_effective_URL' returned '$response' in $time seconds." . "\n";
    }

    curl_multi_remove_handle($mh, $chs[$key]);
}

// close current handler
curl_multi_close($mh);

```

A possible return for this example could be:

> 
The request to '[http://www.example.com](http://www.example.com)' returned 'fruits' in 2 seconds.
The request to '[http://www.example2.com](http://www.example2.com)' returned 'seafood' in 5 seconds.




## Creating and sending a request with a custom method


By default, PHP Curl supports `GET` and `POST` requests. It is possible to also send custom requests, such as `DELETE`, `PUT` or `PATCH` (or even non-standard methods) using the `CURLOPT_CUSTOMREQUEST` parameter.

```
$method = 'DELETE'; // Create a DELETE request

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $method);
$content = curl_exec($ch);
curl_close($ch);

```



## Sending multi-dimensional data and multiple files with CurlFile in one request


Let's say we have a form like the one below. We want to send the data to our webserver via AJAX and from there to a script running on an external server.

[<img src="https://i.stack.imgur.com/55fQ6.png" alt="This is the form we want to send" />](https://i.stack.imgur.com/55fQ6.png)

So we have normal inputs, a multi-select field and a file dropzone where we can upload multiple files.

Assuming the AJAX POST request was successful we get the following data on PHP site:

```
// print_r($_POST)

Array
(
    [first_name] => John
    [last_name] => Doe
    [activities] => Array
        (
            [0] => soccer
            [1] => hiking
        )
)

```

and the files should look like this

```
// print_r($_FILES)

Array
(
    [upload] => Array
        (
            [name] => Array
                (
                    [0] => my_photo.jpg
                    [1] => my_life.pdf
                )

            [type] => Array
                (
                    [0] => image/jpg
                    [1] => application/pdf
                )

            [tmp_name] => Array
                (
                    [0] => /tmp/phpW5spji
                    [1] => /tmp/phpWgnUeY
                )

            [error] => Array
                (
                    [0] => 0
                    [1] => 0
                )

            [size] => Array
                (
                    [0] => 647548
                    [1] => 643223
                )

        )

)

```

So far, so good. Now we want to send this data and files to the external server using cURL with the CurlFile Class

Since cURL only accepts a simple but not a multi-dimensional array, we have to flatten the $_POST array first.

To do this, you could use [this function for example](http://codereview.stackexchange.com/a/14685) which gives you the following:

```
// print_r($new_post_array)

Array
(
    [first_name] => John
    [last_name] => Doe
    [activities[0]] => soccer
    [activities[1]] => hiking
)

```

The next step is to create CurlFile Objects for the uploaded files. This is done by the following loop:

```
$files = array();

foreach ($_FILES["upload"]["error"] as $key => $error) {
    if ($error == UPLOAD_ERR_OK) {

        $files["upload[$key]"] = curl_file_create(
            $_FILES['upload']['tmp_name'][$key],
            $_FILES['upload']['type'][$key],
            $_FILES['upload']['name'][$key]
        );
    }
}

```

curl_file_create is a helper function of the CurlFile Class and creates the CurlFile objects. We save each object in the $files array with keys named "upload[0]" and "upload[1]" for our two files.

We now have to combine the flattened post array and the files array and save it as $data like this:

```
$data = $new_post_array + $files;

```

The last step is to send the cURL request:

```
$ch = curl_init();

curl_setopt_array($ch, array(
    CURLOPT_POST => 1,
    CURLOPT_URL => "https://api.externalserver.com/upload.php",
    CURLOPT_RETURNTRANSFER => 1,
    CURLINFO_HEADER_OUT => 1,
    CURLOPT_POSTFIELDS => $data
));

$result = curl_exec($ch);

curl_close ($ch);

```

Since $data is now a simple (flat) array, cURL automatically sends this POST request with Content Type: multipart/form-data

In upload.php on the external server you can now get the post data and files with $_POST and $_FILES as you would normally do.



## Get and Set custom http headers in php


**Sending The Request Header**

```
$uri = 'http://localhost/http.php';
$ch = curl_init($uri);
curl_setopt_array($ch, array(
    CURLOPT_HTTPHEADER  => array('X-User: admin', 'X-Authorization: 123456'),
    CURLOPT_RETURNTRANSFER  =>true,
    CURLOPT_VERBOSE     => 1
));
$out = curl_exec($ch);
curl_close($ch);
// echo response output
echo $out;

```

**Reading the custom header**

```
print_r(apache_request_headers());

```

**OutPut :-**

```
Array
(
    [Host] => localhost
    [Accept] => */*
    [X-User] => admin
    [X-Authorization] => 123456
    [Content-Length] => 9
    [Content-Type] => application/x-www-form-urlencoded
)

```

We can also send the header using below syntax :-

```
curl --header "X-MyHeader: 123" www.google.com

```



#### Syntax


- resource curl_init ([ string $url = NULL ] )
- bool curl_setopt ( resource $ch , int $option , mixed $value )
- bool curl_setopt_array ( resource $ch, array $options )
- mixed curl_exec ( resource $ch )
- void curl_close ( resource $ch )



#### Parameters


|Parameter|Details
|------
|**curl_init**|--  Initialize a cURL session
|url|The url to be used in the cURL request
|**curl_setopt**|-- Set an option for a cURL transfer
|ch|The cURL handle (return value from **curl_init()**)
|option|CURLOPT_XXX to be set - see [PHP documentation](http://php.net/manual/en/function.curl-setopt.php) for the list of options and acceptable values
|value|The value to be set on the cURL handle for the given option
|**curl_exec**|--  Perform a cURL session
|ch|The cURL handle (return value from **curl_init()**)
|**curl_close**|--  Close a cURL session
|ch|The cURL handle (return value from **curl_init()**)

