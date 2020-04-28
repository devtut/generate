---
metaTitle: "HTTP Authentication"
description: "Simple authenticate"
---

# HTTP Authentication




## Simple authenticate


**PLEASE NOTE: ONLY PUT THIS CODE IN THE HEADER OF THE PAGE, OTHERWISE IT WILL NOT WORK!**

```
<?php
if (!isset($_SERVER['PHP_AUTH_USER'])) {
    header('WWW-Authenticate: Basic realm="My Realm"');
    header('HTTP/1.0 401 Unauthorized');
    echo 'Text to send if user hits Cancel button';
    exit;
}
echo "<p>Hello {$_SERVER['PHP_AUTH_USER']}.</p>";
$user = $_SERVER['PHP_AUTH_USER']; //Lets save the information
echo "<p>You entered {$_SERVER['PHP_AUTH_PW']} as your password.</p>";
$pass = $_SERVER['PHP_AUTH_PW']; //Save the password(optionally add encryption)!
?>
//You html page

```

