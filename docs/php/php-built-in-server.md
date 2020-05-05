---
metaTitle: "PHP - PHP Built in server"
description: "Running the built in server, built in server with specific directory and router script"
---

# PHP Built in server


Learn how to use the built in server to develop and test your application without the need of other tools like xamp, wamp, etc.



## Running the built in server


`php -S localhost:80`

> 
<p>PHP 7.1.7 Development Server started at Fri Jul 14 15:11:05 2017<br />
Listening on [http://localhost:80](http://localhost:80)<br />
Document root is C:\projetos\repgeral<br />
Press Ctrl-C to quit.</p>


This is the simplest way to start a PHP server that responds to request made to localhost at the port 80.

The -S tells that we are starting a webserver.

The **localhost:80** indicates the host that we are answering and the port. You can use other combinations like:

- mymachine:80 - will listen on the address mymachine and port 80;
- 127.0.0.1:8080 - will listen on the address 127.0.0.1 and port 8080;



## built in server with specific directory and router script


`php -S localhost:80 -t project/public router.php`

> 
<p>PHP 7.1.7 Development Server started at Fri Jul 14 15:22:25 2017<br />
Listening on [http://localhost:80](http://localhost:80)<br />
Document root is /home/project/public<br />
Press Ctrl-C to quit.</p>




#### Parameters


|Column|Column
|---|---|---|---|---|---|---|---|---|---
|-S|Tell the php that we want a webserver
|<hostname>:<port>|The host name and the por to be used
|-t|Public directory
|<filename>|The routing script



#### Remarks


An example of router script is:

```php
<?php
// router.php
if (preg_match('/\.(?:png|jpg|jpeg|gif)$/', $_SERVER["REQUEST_URI"])) {
    return false;    // serve the requested resource as-is.
}  //the rest of you code goes here.

```

