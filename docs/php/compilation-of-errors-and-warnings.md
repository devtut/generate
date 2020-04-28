---
metaTitle: "Compilation of Errors and Warnings"
description: "Notice: Undefined index, Warning: Cannot modify header information - headers already sent, Parse error: syntax error, unexpected T_PAAMAYIM_NEKUDOTAYIM"
---

# Compilation of Errors and Warnings



## Notice: Undefined index


**Appearance :**

Trying to access an array by a key that does not exist in the array

**Possible Solution :**

Check the availability before accessing it. Use:

1. [`isset()`](http://php.net/manual/en/function.isset.php)
1. [`array_key_exists()`](http://php.net/manual/en/function.array-key-exists.php)



## Warning: Cannot modify header information - headers already sent


**Appearance :**

Happens when your script tries to send a HTTP header to the client but there already was output before, which resulted in headers to be already sent to the client.

**Possible Causes :**

<li>
**Print, echo:** Output from print and echo statements will terminate the opportunity to send HTTP headers. The application flow must be restructured to avoid that.
</li>
<li>
**Raw HTML areas:** Unparsed HTML sections in a .php file are direct output as well. Script conditions that will trigger a `header()` call must be noted before any raw  blocks.
<pre><code><!DOCTYPE html>
<?php
     // Too late for headers already.
</code></pre>
</li>
<li>
**Whitespace before `<?php` for "script.php line 1" warnings:** If the warning refers to output in line 1, then it's mostly leading whitespace, text or HTML before the opening `<?php` token.
<pre><code><?php
# There's a SINGLE space/newline before <? - Which already seals it.
</code></pre>
</li>

Reference from SO [answer](http://stackoverflow.com/a/8028987/5447994) by [Mario](http://stackoverflow.com/users/345031/mario)



## Parse error: syntax error, unexpected T_PAAMAYIM_NEKUDOTAYIM


**Appearance:**

"Paamayim Nekudotayim" means "double colon" in Hebrew; thus this error refers to the inappropriate use of the double colon operator (`::`). The error is typically caused by an attempt to call a static method that is, in fact, not static.

**Possible Solution:**

```
$classname::doMethod();

```

If the above code causes this error, you most likely need to simply change the way you call the method:

```
$classname->doMethod();

```

The latter example assumes that `$classname` is an instance of a class, and the `doMethod()` is not a static method of that class.

