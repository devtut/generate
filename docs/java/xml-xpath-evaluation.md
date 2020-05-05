---
metaTitle: "Java - XML XPath Evaluation"
description: "Evaluating a NodeList in an XML document, Parsing multiple XPath Expressions in a single XML, Parsing single XPath Expression multiple times in an XML"
---

# XML XPath Evaluation



## Evaluating a NodeList in an XML document


Given the following XML document:

```java
<documentation>
    <tags>
        <tag name="Java">
            <topic name="Regular expressions">
                <example>Matching groups</example>
                <example>Escaping metacharacters</example>
            </topic>
            <topic name="Arrays">
                <example>Looping over arrays</example>
                <example>Converting an array to a list</example>
            </topic>
        </tag>
        <tag name="Android">
            <topic name="Building Android projects">
                <example>Building an Android application using Gradle</example>
                <example>Building an Android application using Maven</example>
            </topic>
            <topic name="Layout resources">
                <example>Including layout resources</example>
                <example>Supporting multiple device screens</example>
            </topic>
        </tag>
    </tags>
</documentation>

```

The following retrieves all `example` nodes for the Java tag (Use this method if only evaluating XPath in the XML once. See other example for when multiple XPath calls are evaluated in the same XML file.):

```java
XPathFactory xPathFactory = XPathFactory.newInstance();
XPath xPath = xPathFactory.newXPath(); //Make new XPath
InputSource inputSource = new InputSource("path/to/xml.xml"); //Specify XML file path

NodeList javaExampleNodes = (NodeList) xPath.evaluate("/documentation/tags/tag[@name='Java']//example", inputSource, XPathConstants.NODESET); //Evaluate the XPath
...

```



## Parsing multiple XPath Expressions in a single XML


Using the same example as **Evaluating a NodeList in an XML document**, here is how you would make multiple XPath calls efficiently:

Given the following XML document:

```java
<documentation>
    <tags>
        <tag name="Java">
            <topic name="Regular expressions">
                <example>Matching groups</example>
                <example>Escaping metacharacters</example>
            </topic>
            <topic name="Arrays">
                <example>Looping over arrays</example>
                <example>Converting an array to a list</example>
            </topic>
        </tag>
        <tag name="Android">
            <topic name="Building Android projects">
                <example>Building an Android application using Gradle</example>
                <example>Building an Android application using Maven</example>
            </topic>
            <topic name="Layout resources">
                <example>Including layout resources</example>
                <example>Supporting multiple device screens</example>
            </topic>
        </tag>
    </tags>
</documentation>

```

This is how you would use XPath to evaluate multiple expressions in one document:

```java
XPath xPath = XPathFactory.newInstance().newXPath(); //Make new XPath
DocumentBuilder builder = DocumentBuilderFactory.newInstance();
Document doc = builder.parse(new File("path/to/xml.xml")); //Specify XML file path

NodeList javaExampleNodes = (NodeList) xPath.evaluate("/documentation/tags/tag[@name='Java']//example", doc, XPathConstants.NODESET); //Evaluate the XPath
xPath.reset(); //Resets the xPath so it can be used again
NodeList androidExampleNodes = (NodeList) xPath.evaluate("/documentation/tags/tag[@name='Android']//example", doc, XPathConstants.NODESET); //Evaluate the XPath

...

```



## Parsing single XPath Expression multiple times in an XML


In this case, you want to have the expression compiled before the evaluations, so that each call to `evaluate` does not `compile` the same expression. The simple syntax would be:

```java
XPath xPath = XPathFactory.newInstance().newXPath(); //Make new XPath
XPathExpression exp = xPath.compile("/documentation/tags/tag[@name='Java']//example");
DocumentBuilder builder = DocumentBuilderFactory.newInstance();
Document doc = builder.parse(new File("path/to/xml.xml")); //Specify XML file path

NodeList javaExampleNodes = (NodeList) exp.evaluate(doc, XPathConstants.NODESET); //Evaluate the XPath from the already-compiled expression

NodeList javaExampleNodes2 = (NodeList) exp.evaluate(doc, XPathConstants.NODESET); //Do it again

```

Overall, two calls to `XPathExpression.evaluate()` will be much more efficient than two calls to `XPath.evaluate()`.



#### Remarks


XPath expressions are used to navigate and select one or more nodes within an XML tree document, such as selecting a certain element or attribute node.

See [this W3C recommendation](https://www.w3.org/TR/xpath/) for a reference on this language.

