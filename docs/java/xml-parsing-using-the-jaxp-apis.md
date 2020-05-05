---
metaTitle: "Java - XML Parsing using the JAXP APIs"
description: "Parsing a document using the StAX API, Parsing and navigating a document using the DOM API"
---

# XML Parsing using the JAXP APIs



## Parsing a document using the StAX API


Considering the following document :

```java
<?xml version='1.0' encoding='UTF-8' ?>
<library>
   <book id='1'>Effective Java</book>
   <book id='2'>Java Concurrency In Practice</book>
   <notABook id='3'>This is not a book element</notABook>
</library>

```

One can use the following code to parse it and build a map of book titles by book id.

```java
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;

public class StaxDemo {

public static void main(String[] args) throws Exception {
    String xmlDocument = "<?xml version='1.0' encoding='UTF-8' ?>"
            + "<library>"
                + "<book id='1'>Effective Java</book>"
                + "<book id='2'>Java Concurrency In Practice</book>"
                + "<notABook id='3'>This is not a book element </notABook>"
            + "</library>";

    XMLInputFactory xmlInputFactory = XMLInputFactory.newFactory();
    // Various flavors are possible, e.g. from an InputStream, a Source, ...
    XMLStreamReader xmlStreamReader = xmlInputFactory.createXMLStreamReader(new StringReader(xmlDocument));

    Map<Integer, String> bookTitlesById = new HashMap<>();

    // We go through each event using a loop
    while (xmlStreamReader.hasNext()) {
        switch (xmlStreamReader.getEventType()) {
            case XMLStreamConstants.START_ELEMENT:
                System.out.println("Found start of element: " + xmlStreamReader.getLocalName());
                // Check if we are at the start of a <book> element
                if ("book".equals(xmlStreamReader.getLocalName())) {
                    int bookId = Integer.parseInt(xmlStreamReader.getAttributeValue("", "id"));
                    String bookTitle = xmlStreamReader.getElementText();
                    bookTitlesById.put(bookId, bookTitle);
                }
                break;
            // A bunch of other things are possible : comments, processing instructions, Whitespace...
            default:
                break;
        }
        xmlStreamReader.next();
    }

    System.out.println(bookTitlesById);
}

```

This outputs :

```java
Found start of element: library
Found start of element: book
Found start of element: book
Found start of element: notABook
{1=Effective Java, 2=Java Concurrency In Practice}

```

In this sample, one must be carreful of a few things :

<li>
THe use of `xmlStreamReader.getAttributeValue` works because we have checked first that the parser is in the `START_ELEMENT` state. In evey other states (except `ATTRIBUTES`), the parser is mandated to throw `IllegalStateException`, because attributes can only appear at the beginning of elements.
</li>
<li>
same goes for `xmlStreamReader.getTextContent()`, it works because we are at a `START_ELEMENT` and we know in this document that the `<book>` element has no non-text child nodes.
</li>

For more complex documents parsing (deeper, nested elements, ...), it is a good practice to "delegate" the parser to sub-methods or other objets, e.g. have a `BookParser` class or method, and have it deal with every element from the START_ELEMENT to the END_ELEMENT of the book XML tag.

One can also use a `Stack` object to keep around important datas up and down the tree.



## Parsing and navigating a document using the DOM API


Considering the following document :

```java
<?xml version='1.0' encoding='UTF-8' ?>
<library>
   <book id='1'>Effective Java</book>
   <book id='2'>Java Concurrency In Practice</book>
</library>

```

One can use the following code to build a DOM tree out of a `String` :

```java
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.StringReader;

public class DOMDemo {

public static void main(String[] args) throws Exception {
    String xmlDocument = "<?xml version='1.0' encoding='UTF-8' ?>"
            + "<library>"
            + "<book id='1'>Effective Java</book>"
            + "<book id='2'>Java Concurrency In Practice</book>"
            + "</library>";

    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
    // This is useless here, because the XML does not have namespaces, but this option is usefull to know in cas
    documentBuilderFactory.setNamespaceAware(true);
    DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
    // There are various options here, to read from an InputStream, from a file, ...
    Document document = documentBuilder.parse(new InputSource(new StringReader(xmlDocument)));

    // Root of the document
    System.out.println("Root of the XML Document: " + document.getDocumentElement().getLocalName());

    // Iterate the contents
    NodeList firstLevelChildren = document.getDocumentElement().getChildNodes();
    for (int i = 0; i < firstLevelChildren.getLength(); i++) {
        Node item = firstLevelChildren.item(i);
        System.out.println("First level child found, XML tag name is: " + item.getLocalName());
        System.out.println("\tid attribute of this tag is : " + item.getAttributes().getNamedItem("id").getTextContent());
    }

    // Another way would have been
    NodeList allBooks = document.getDocumentElement().getElementsByTagName("book");
}
}

```

The code yields the following :

```java
Root of the XML Document: library
First level child found, XML tag name is: book
id attribute of this tag is : 1
First level child found, XML tag name is: book
id attribute of this tag is : 2

```



#### Remarks


XML Parsing is the interpretation of XML documents in order to manipulate their content using sensible constructs, be they "nodes", "attributes", "documents", "namespaces", or events related to these constructs.

Java has a native API for XML document handling, called [JAXP, or Java API for XML Processing](https://jaxp.java.net/). JAXP and a reference implementation has been bundled with every Java release since Java 1.4 (JAXP v1.1) and has evolved since. Java 8 shipped with JAXP version 1.6.

The API provides different ways of interacting with XML documents, which are :

- The DOM interface (Document Object Model)
- The SAX interface (Simple API for XML)
- The StAX interface (Streaming API for XML)

### Principles of the DOM interface

The DOM interface aims to provide a [W3C DOM](https://www.w3.org/DOM/) compliant way of interpreting XML. Various versions of JAXP have supported various DOM Levels of specification (up to level 3).

Under the Document Object Model interface, an XML document is represented as a tree, starting with the "Document Element". The base type of the API is the [`Node`](https://docs.oracle.com/javase/8/docs/api/org/w3c/dom/Node.html) type, it allows to navigate from a `Node` to its parent, its children, or its siblings (although, not all `Node`s can have children, for example, `Text` nodes are final in the tree, and never have childre). XML tags are represented as `Element`s, which notably extend the `Node` with attribute-related methods.

The DOM interface is very usefull since it allows a "one line" parsing of XML documents as trees, and allows easy modification of the constructed tree (node addition, suppression, copying, ...), and finally its serialization (back to disk) post modifications. This comes at a price, though : the tree resides in memory, therefore, DOM trees are not always practical for huge XML documents. Furthermore, the construction of the tree is not always the fastest way of dealing with XML content, especially if one is not interested in all parts of the XML document.

### Principles of the SAX interface

The SAX API is an event-oriented API to deal with XML documents. Under this model, the components of an XML documents are interpreted as events (e.g. "a tag has been opened", "a tag has been closed", "a text node has been encountered", "a comment has been encountered")...

The SAX API uses a "push parsing" approach, where a SAX [`Parser`](https://docs.oracle.com/javase/7/docs/api/javax/xml/parsers/SAXParser.html) is responsible for interpreting the XML document, and invokes methods on a delegate (a [`ContentHandler`](https://docs.oracle.com/javase/7/docs/api/org/xml/sax/ContentHandler.html)) to deal with whatever event is found in the XML document. Usually, one never writes a parser, but one provides a handler to gather all needed informations from the XML document.

The SAX interface overcomes the DOM interface's limitations by keeping only the minimum necessary data at the parser level (e.g. namespaces contexts, validation state), therefore, only informations that are kept by the `ContentHandler` - for which you, the developer, is responsible - are held into memory. The tradeoff is that there is no way of "going back in time/the XML document" with such an approach : while DOM allows a `Node` to go back to its parent, there is no such possibility in SAX.

### Principles of the StAX interface

The StAX API takes a similar approach to processing XML as the SAX API (that is, event driven), the only very significative difference being that StAX is a pull parser (where SAX was a push parser). In SAX, the `Parser` is in control, and uses callbacks on the `ContentHandler`. In Stax, you call the parser, and control when/if you want to obtain the next XML "event".

The API starts with [XMLStreamReader](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLStreamReader.html) (or [XMLEventReader](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLStreamReader.html)), which are the gateways through which the developer can ask `nextEvent()`, in an iterator-style way.

