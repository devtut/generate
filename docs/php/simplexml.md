---
metaTitle: "SimpleXML"
description: "Loading XML data into simplexml"
---

# SimpleXML



## Loading XML data into simplexml


### Loading from string

Use `simplexml_load_string` to create a `SimpleXMLElement` from a string:

```
$xmlString = "<?xml version='1.0' encoding='UTF-8'?>";
$xml = simplexml_load_string($xmlString) or die("Error: Cannot create object");

```

Note that `or` not `||` must be used here because the precedence of `or` is higher than `=`. The code after `or` will only be executed if `$xml` finally resolves to false.

### Loading from file

Use `simplexml_load_file` to load XML data from a file or a URL:

```
$xml = simplexml_load_string("filePath.xml");

$xml = simplexml_load_string("https://example.com/doc.xml");

```

The URL can be of any [schemes that PHP supports](http://php.net/wrappers), or custom stream wrappers.

