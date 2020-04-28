---
metaTitle: "XML"
description: "Create a XML using DomDocument, Read a XML document with DOMDocument, Create an XML file using XMLWriter, Read a XML document with SimpleXML, Leveraging XML with PHP's SimpleXML Library"
---

# XML



## Create a XML using DomDocument


To create a XML using DOMDocument,basically, we need to create all the tags and attributes using the `createElement()` and `createAttribute()` methods and them create the XML structure with the `appendChild()`.

The example below includes tags, attributes, a CDATA section and a different namespace for the second tag:

```
$dom = new DOMDocument('1.0', 'utf-8');
$dom->preserveWhiteSpace = false;
$dom->formatOutput = true;

//create the main tags, without values
$books = $dom->createElement('books');
$book_1 = $dom->createElement('book');

// create some tags with values
$name_1 = $dom->createElement('name', 'PHP - An Introduction');
$price_1 = $dom->createElement('price', '$5.95');
$id_1 = $dom->createElement('id', '1');

//create  and append an attribute
$attr_1 = $dom->createAttribute('version');
$attr_1->value = '1.0';
//append the attribute
$id_1->appendChild($attr_1);

//create the second tag book with different namespace
$namespace = 'www.example.com/libraryns/1.0';

//include the namespace prefix in the books tag
$books->setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:ns', $namespace);
$book_2 = $dom->createElementNS($namespace,'ns:book');
$name_2 = $dom->createElementNS($namespace, 'ns:name');

//create a CDATA section (that is another DOMNode instance) and put it inside the name tag
$name_cdata = $dom->createCDATASection('PHP - Advanced');
$name_2->appendChild($name_cdata);
$price_2 = $dom->createElementNS($namespace, 'ns:price', '$25.00');
$id_2 = $dom->createElementNS($namespace, 'ns:id', '2');

//create the XML structure
$books->appendChild($book_1);
$book_1->appendChild($name_1);
$book_1->appendChild($price_1);
$book_1->appendChild($id_1);
$books->appendChild($book_2);
$book_2->appendChild($name_2);
$book_2->appendChild($price_2);
$book_2->appendChild($id_2);

$dom->appendChild($books);

//saveXML() method returns the XML in a String
print_r ($dom->saveXML());

```

This will output the following XML:

```
<?xml version="1.0" encoding="utf-8"?>
<books xmlns:ns="www.example.com/libraryns/1.0">
  <book>
    <name>PHP - An Introduction</name>
    <price>$5.95</price>
    <id version="1.0">1</id>
  </book>
  <ns:book>
    <ns:name><![CDATA[PHP - Advanced]]></ns:name>
    <ns:price>$25.00</ns:price>
    <ns:id>2</ns:id>
  </ns:book>
</books>

```



## Read a XML document with DOMDocument


Similarly to the SimpleXML, you can use DOMDocument to parse XML from a string or from a XML file

**1. From a string**

```
$doc = new DOMDocument();
$doc->loadXML($string);

```

**2. From a file**

```
$doc = new DOMDocument();
$doc->load('books.xml');// use the actual file path. Absolute or relative

```

**Example of parsing**

Considering the following XML:

```
<?xml version="1.0" encoding="UTF-8"?>
<books>
   <book>
      <name>PHP - An Introduction</name>
      <price>$5.95</price>
      <id>1</id>
   </book>
   <book>
      <name>PHP - Advanced</name>
      <price>$25.00</price>
      <id>2</id>
   </book>
</books>

```

This is a example code to parse it

```
$books = $doc->getElementsByTagName('book');
foreach ($books as $book) {
    $title = $book->getElementsByTagName('name')->item(0)->nodeValue;
    $price = $book->getElementsByTagName('price')->item(0)->nodeValue;
    $id = $book->getElementsByTagName('id')->item(0)->nodeValue;
    print_r ("The title of the book $id is $title and it costs $price." . "\n");
}

```

This will output:

> 
The title of the book 1 is PHP - An Introduction and it costs $5.95.
The title of the book 2 is PHP - Advanced and it costs $25.00.




## Create an XML file using XMLWriter


Instantiate a XMLWriter object:

```
$xml = new XMLWriter();

```

Next open the file to which you want to write. For example, to write to `/var/www/example.com/xml/output.xml`, use:

```
$xml->openUri('file:///var/www/example.com/xml/output.xml');

```

To start the document (create the XML open tag):

```
$xml->startDocument('1.0', 'utf-8');

```

This will output:

```
<?xml version="1.0" encoding="UTF-8"?>

```

Now you can start writing elements:

```
$xml->writeElement('foo', 'bar');

```

This will generate the XML:

```
<foo>bar</foo>

```

If you need something a little more complex than simply nodes with plain values, you can also "start" an element and add attributes to it before closing it:

```
$xml->startElement('foo');
$xml->writeAttribute('bar', 'baz');
$xml->writeCdata('Lorem ipsum');
$xml->endElement();

```

This will output:

```
<foo bar="baz"><![CDATA[Lorem ipsum]]></foo>

```



## Read a XML document with SimpleXML


You can parse XML from a string or from a XML file

**1. From a string**

```
$xml_obj = simplexml_load_string($string);

```

**2. From a file**

```
$xml_obj = simplexml_load_file('books.xml');

```

**Example of parsing**

Considering the following XML:

```
<?xml version="1.0" encoding="UTF-8"?>
<books>
   <book>
      <name>PHP - An Introduction</name>
      <price>$5.95</price>
      <id>1</id>
   </book>
   <book>
      <name>PHP - Advanced</name>
      <price>$25.00</price>
      <id>2</id>
   </book>
</books>

```

This is a example code to parse it

```
$xml = simplexml_load_string($xml_string);
$books = $xml->book;
foreach ($books as $book) {
    $id = $book->id;
    $title = $book->name;
    $price = $book->price;
    print_r ("The title of the book $id is $title and it costs $price." . "\n");
}

```

This will output:

> 
<p>The title of the book 1 is PHP - An Introduction and it costs $5.95.<br/>
The title of the book 2 is PHP - Advanced and it costs $25.00.</p>




## Leveraging XML with PHP's SimpleXML Library


SimpleXML is a powerful library which converts XML strings to an easy to use PHP object.

The following assumes an XML structure as below.

```
<?xml version="1.0" encoding="UTF-8"?>
<document>
    <book>
        <bookName>StackOverflow SimpleXML Example</bookName>
        <bookAuthor>PHP Programmer</bookAuthor>
    </book>
    <book>
        <bookName>Another SimpleXML Example</bookName>
        <bookAuthor>Stack Overflow Community</bookAuthor>
        <bookAuthor>PHP Programmer</bookAuthor>
        <bookAuthor>FooBar</bookAuthor>
    </book>
</document>

```

**Read our data in to SimpleXML**

To get started, we need to read our data into SimpleXML. We can do this in 3 different ways. Firstly, we can [load our data from a DOM node.](http://php.net/manual/en/function.simplexml-import-dom.php)

```
$xmlElement = simplexml_import_dom($domNode);

```

Our next option is to [load our data from an XML file.](http://php.net/manual/en/function.simplexml-load-file.php)

```
$xmlElement = simplexml_load_file($filename);

```

Lastly, we can [load our data from a variable.](http://php.net/manual/en/function.simplexml-load-string.php)

```
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
<document>
    <book>
        <bookName>StackOverflow SimpleXML Example</bookName>
        <bookAuthor>PHP Programmer</bookAuthor>
    </book>
    <book>
        <bookName>Another SimpleXML Example</bookName>
        <bookAuthor>Stack Overflow Community</bookAuthor>
        <bookAuthor>PHP Programmer</bookAuthor>
        <bookAuthor>FooBar</bookAuthor>
    </book>
</document>';
$xmlElement = simplexml_load_string($xmlString);

```

Whether you've picked to load from [a DOM Element](http://php.net/manual/en/function.simplexml-import-dom.php), from [a file](http://php.net/manual/en/function.simplexml-load-file.php) or from [a string](http://php.net/manual/en/function.simplexml-load-string.php), you are now left with a SimpleXMLElement variable called `$xmlElement`. Now, we can start to make use of our XML in PHP.

**Accessing our SimpleXML Data**

The simplest way to access data in our SimpleXMLElement object is to [call the properties directly](http://php.net/manual/en/simplexml.examples-basic.php#example-6325). If we want to access our first bookName, `StackOverflow SimpleXML Example`, then we can access it as per below.

```
echo $xmlElement->book->bookName;

```

At this point, SimpleXML will assume that because we have not told it explicitly which book we want, that we want the first one. However, if we decide that we do not want the first one, rather that we want `Another SimpleXML Example`, then we can access it as per below.

```
echo $xmlElement->book[1]->bookName;

```

It is worth noting that using `[0]` works the same as not using it, so

```
$xmlElement->book

```

works the same as

```
$xmlElement->book[0]

```

**Looping through our XML**

There are many reasons you may wish to [loop through XML](http://php.net/manual/en/simplexml.examples-basic.php#example-6327), such as that you have a number of items, books in our case, that we would like to display on a webpage. For this, we can use a [foreach loop](http://php.net/manual/en/control-structures.foreach.php) or a standard [for loop](http://php.net/manual/en/control-structures.for.php), taking advantage of [SimpleXMLElement's count function.](http://php.net/manual/en/simplexmlelement.count.php).

```
foreach ( $xmlElement->book as $thisBook ) {
    echo $thisBook->bookName
}

```

or

```
$count = $xmlElement->count();
for ( $i=0; $i<$count; $i++ ) {
    echo $xmlElement->book[$i]->bookName;
}

```

**Handling Errors**

Now we have come so far, it is important to realise that we are only humans, and will likely encounter an error eventually - especially if we are playing with different XML files all the time. And so, we will want to handle those errors.

Consider we created an XML file. You will notice that while this XML is much alike what we had earlier, the problem with this XML file is that the final closing tag is /doc instead of /document.

```
<?xml version="1.0" encoding="UTF-8"?>
<document>
    <book>
        <bookName>StackOverflow SimpleXML Example</bookName>
        <bookAuthor>PHP Programmer</bookAuthor>
    </book>
    <book>
        <bookName>Another SimpleXML Example</bookName>
        <bookAuthor>Stack Overflow Community</bookAuthor>
        <bookAuthor>PHP Programmer</bookAuthor>
        <bookAuthor>FooBar</bookAuthor>
    </book>
</doc>

```

Now, say, we load this into our PHP as $file.

```
libxml_use_internal_errors(true);
$xmlElement = simplexml_load_file($file);
if ( $xmlElement === false ) {
    $errors = libxml_get_errors();
    foreach ( $errors as $thisError ) {
        switch ( $thisError->level ) {
            case LIBXML_ERR_FATAL:
                echo "FATAL ERROR: ";
                break;
            case LIBXML_ERR_ERROR:
                echo "Non Fatal Error: ";
                break;
            case LIBXML_ERR_WARNING:
                echo "Warning: ";
                break;
        }
        echo $thisError->code . PHP_EOL .
            'Message: ' . $thisError->message . PHP_EOL .
            'Line: ' . $thisError->line . PHP_EOL .
            'Column: ' . $thisError->column . PHP_EOL .
            'File: ' . $thisError->file;
    }
    libxml_clear_errors();
} else {
    echo 'Happy Days';
}

```

We will be greeted with the following

```
FATAL ERROR: 76
Message: Opening and ending tag mismatch: document line 2 and doc

Line: 13
Column: 10
File: filepath/filename.xml

```

However as soon as we fix this problem, we are presented with "Happy Days".

