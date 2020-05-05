---
metaTitle: "PHP - Output Buffering"
description: "Basic usage getting content between buffers and clearing, Nested output buffers, Running output buffer before any content, Processing the buffer via a callback, Using Output buffer to store contents in a file, useful for reports, invoices etc, Stream output to client, Capturing the output buffer to re-use later, Typical usage and reasons for using ob_start"
---

# Output Buffering




## Basic usage getting content between buffers and clearing


Output buffering allows you to store any textual content (Text, `HTML`) in a variable and send to the browser as one piece at the end of your script. By default, `php` sends your content as it interprets it.

```php
<?php

// Turn on output buffering
ob_start();

// Print some output to the buffer (via php)
print 'Hello ';

// You can also `step out` of PHP
?>
<em>World</em>
<?php
// Return the buffer AND clear it
$content = ob_get_clean();

// Return our buffer and then clear it
# $content = ob_get_contents();
# $did_clear_buffer = ob_end_clean();

print($content);

#> "Hello <em>World</em>"

```

Any content outputted between `ob_start()` and `ob_get_clean()` will be captured and placed into the variable `$content`.

Calling `ob_get_clean()` triggers both `ob_get_contents()` and `ob_end_clean()`.



## Nested output buffers


You can nest output buffers and fetch the level for them to provide different content using the `ob_get_level()` function.

```php
<?php

$i = 1;
$output = null;

while( $i <= 5 ) {
    // Each loop, creates a new output buffering `level`
    ob_start();
    print "Current nest level: ". ob_get_level() . "\n";
    $i++;
}

// We're at level 5 now
print 'Ended up at level: ' . ob_get_level() . PHP_EOL;

// Get clean will `pop` the contents of the top most level (5)
$output .= ob_get_clean();
print $output;

print 'Popped level 5, so we now start from 4' . PHP_EOL;

// We're now at level 4 (we pop'ed off 5 above)

// For each level we went up, come back down and get the buffer
while( $i > 2 ) {
    print "Current nest level: " . ob_get_level() . "\n";
    echo ob_get_clean();
    $i--;
}

```

**Outputs:**

```php
Current nest level: 1
Current nest level: 2
Current nest level: 3
Current nest level: 4
Current nest level: 5
Ended up at level: 5
Popped level 5, so we now start from 4
Current nest level: 4
Current nest level: 3
Current nest level: 2
Current nest level: 1

```



## Running output buffer before any content


```php
ob_start();

$user_count = 0;
foreach( $users as $user ) {
    if( $user['access'] != 7 ) { continue; }
    ?>
    <li class="users user-<?php echo $user['id']; ?>">
        <a href="<?php echo $user['link']; ?>">
            <?php echo $user['name'] ?>
        </a>
    </li>
<?php
    $user_count++;
}
$users_html = ob_get_clean();

if( !$user_count ) {
    header('Location: /404.php');
    exit();
}
?>
<html>
<head>
    <title>Level 7 user results (<?php echo $user_count; ?>)</title>
</head>

<body>
<h2>We have a total of <?php echo $user_count; ?> users with access level 7</h2>
<ul class="user-list">
    <?php echo $users_html; ?>
</ul>
</body>
</html>

```

In this example we assume `$users` to be a multidimensional array, and we loop through it to find all users with an access level of 7.

If there are no results, we redirect to an error page.

We are using the output buffer here because we are triggering a `header()` redirect based on the result of the loop



## Processing the buffer via a callback


You can apply any kind of additional processing to the output by passing a callable to `ob_start()`.

```php
<?php
function clearAllWhiteSpace($buffer) {
    return str_replace(array("\n", "\t", ' '), '', $buffer);
}

ob_start('clearAllWhiteSpace');
?>
<h1>Lorem Ipsum</h1>

<p><strong>Pellentesque habitant morbi tristique</strong> senectus et netus et malesuada fames ac turpis egestas. <a href="#">Donec non enim</a> in turpis pulvinar facilisis.</p>

<h2>Header Level 2</h2>

<ol>
   <li>Lorem ipsum dolor sit amet, consectetuer adipiscing elit.</li>
   <li>Aliquam tincidunt mauris eu risus.</li>
</ol>

<?php
/* Output will be flushed and processed when script ends or call
     ob_end_flush();
*/

```

Output:

```php
<h1>LoremIpsum</h1><p><strong>Pellentesquehabitantmorbitristique</strong>senectusetnetusetmalesuadafamesacturpisegestas.<ahref="#">Donecnonenim</a>inturpispulvinarfacilisis.</p><h2>HeaderLevel2</h2><ol><li>Loremipsumdolorsitamet,consectetueradipiscingelit.</li><li>Aliquamtinciduntmauriseurisus.</li></ol>

```



## Using Output buffer to store contents in a file, useful for reports, invoices etc


```php
<?php
ob_start();
?>
    <html>
    <head>
        <title>Example invoice</title>
    </head>
    <body>
    <h1>Invoice #0000</h1>
    <h2>Cost: &pound;15,000</h2>
    ...
    </body>
    </html>
<?php
$html = ob_get_clean();

$handle = fopen('invoices/example-invoice.html', 'w');
fwrite($handle, $html);
fclose($handle);

```

This example takes the complete document, and writes it to file, it does not output the document into the browser, but do by using `echo $html;`



## Stream output to client


```php
/**
 * Enables output buffer streaming. Calling this function
 * immediately flushes the buffer to the client, and any
 * subsequent output will be sent directly to the client.
 */
function _stream() {
    ob_implicit_flush(true);
    ob_end_flush();
}

```



## Capturing the output buffer to re-use later


In this example, we have an array containing some data.

We capture the output buffer in `$items_li_html` and use it twice in the page.

```php
<?php

// Start capturing the output
ob_start();

$items = ['Home', 'Blog', 'FAQ', 'Contact'];

foreach($items as $item):

// Note we're about to step "out of PHP land"
?>
  <li><?php echo $item ?></li>
<?php
// Back in PHP land
endforeach;

// $items_lists contains all the HTML captured by the output buffer
$items_li_html = ob_get_clean();
?>

<!-- Menu 1: We can now re-use that (multiple times if required) in our HTML. -->
<ul class="header-nav">
    <?php echo $items_li_html ?>
</ul>

<!-- Menu 2 -->
<ul class="footer-nav">
    <?php echo $items_li_html ?>
</ul>

```

Save the above code in a file `output_buffer.php` and run it via `php output_buffer.php`.

You should see the 2 list items we created above with the same list items we generated in PHP using the output buffer:

```php
<!-- Menu 1: We can now re-use that (multiple times if required) in our HTML. -->
<ul class="header-nav">
  <li>Home</li>
  <li>Blog</li>
  <li>FAQ</li>
  <li>Contact</li>
</ul>

<!-- Menu 2 -->
<ul class="footer-nav">
  <li>Home</li>
  <li>Blog</li>
  <li>FAQ</li>
  <li>Contact</li>
</ul>

```



## Typical usage and reasons for using ob_start


`ob_start` is especially handy when you have redirections on your page. For example, the following code won't work:

```php
Hello!
<?php
  header("Location: somepage.php");
?>

```

The error that will be given is something like: `headers already sent by <xxx> on line <xxx>`.

In order to fix this problem, you would write something like this at the start of your page:

```php
<?php
  ob_start();
?>

```

And something like this at the end of your page:

```php
<?php
  ob_end_flush();
?>

```

This stores all generated content into an output buffer, and displays it in one go. Hence, if you have any redirection calls on your page, those will trigger before any data is sent, removing the possibility of a `headers already sent` error occurring.



#### Parameters


|Function|Details
|---|---|---|---|---|---|---|---|---|---
|ob_start()|Starts the output buffer, any output placed after this will be captured and not displayed
|ob_get_contents()|Returns all content captured by `ob_start()`
|ob_end_clean()|Empties the output buffer and turns it off for the current nesting level
|ob_get_clean()|Triggers both `ob_get_contents()` and `ob_end_clean()`
|ob_get_level()|Returns the current nesting level of the output buffer
|ob_flush()|Flush the content buffer and send it to the browser without ending the buffer
|ob_implicit_flush()|Enables implicit flushing after every output call.
|ob_end_flush()|Flush the content buffer and send it to the browser also ending the buffer

