---
metaTitle: "Java - JAXB"
description: "Reading an XML file (unmarshalling), Writing an XML file (marshalling an object), Manual field/property XML mapping configuration, Binding an XML namespace to a serializable Java class., Using XmlAdapter to generate desired xml format, Automatic field/property XML mapping configuration (@XmlAccessorType), Specifying a XmlAdapter instance to (re)use existing data, Using XmlAdapter to trim string."
---

# JAXB


JAXB or [Java Architecture for XML Binding](https://en.wikipedia.org/wiki/Java_Architecture_for_XML_Binding) (JAXB) is a software framework that allows Java developers to map Java classes to XML representations. This Page will introduce readers to JAXB using detailed examples about its functions provided mainly for marshaling and un-marshaling Java Objects into xml format and vice-versa.



## Reading an XML file (unmarshalling)


To read an XML file named `UserDetails.xml` with the below content

```java
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<user>
    <name>Jon Skeet</name>
    <userID>8884321</userID>
</user>

```

We need a POJO class named `User.java` as below

```java
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class User {

    private long userID;
    private String name;

    // getters and setters
}

```

Here we have created the variables and class name according to the XML nodes. To map them, we use the annotation `XmlRootElement` on the class.

```java
public class XMLReader {
    public static void main(String[] args) {
        try {
            User user = JAXB.unmarshal(new File("UserDetails.xml"), User.class);
            System.out.println(user.getName());   // prints Jon Skeet
            System.out.println(user.getUserID()); // prints 8884321
        } catch (Exception e) {
            System.err.println("Exception occurred while reading the XML!");
        }
    }
}

```

Here `unmarshal()` method is used to parse the XML file. It takes the XML file name and the class type as two arguments. Then we can use the getter methods of the object to print the data.



## Writing an XML file (marshalling an object)


```java
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class User {

    private long userID;
    private String name;
    
    // getters and setters
}

```

By using the annotation `XMLRootElement`, we can mark a class as a root element of an XML file.

```java
import java.io.File;
import javax.xml.bind.JAXB;

public class XMLCreator {
    public static void main(String[] args) {
        User user = new User();
        user.setName("Jon Skeet");
        user.setUserID(8884321);

        try {
            JAXB.marshal(user, new File("UserDetails.xml"));
        } catch (Exception e) {
            System.err.println("Exception occurred while writing in XML!");
        } finally {
            System.out.println("XML created");
        }
    }
}

```

`marshal()` is used to write the object's content into an XML file. Here `user`object and a new `File` object are passed as arguments to the `marshal()`.

On successful execution, this creates an XML file named `UserDetails.xml` in the class-path with the below content.

```java
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<user>
    <name>Jon Skeet</name>
    <userID>8884321</userID>
</user>

```



## Manual field/property XML mapping configuration


Annotations [`@XmlElement`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/XmlElement.html), [`@XmlAttribute`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/XmlAttribute.html) or [`@XmlTransient`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/XmlTransient.html)  and other in package [`javax.xml.bind.annotation`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/package-summary.html) allow the programmer to specify which and how marked fields or properties should be serialized.

```java
@XmlAccessorType(XmlAccessType.NONE) // we want no automatic field/property marshalling
public class ManualXmlElementsExample {
    
    @XmlElement
    private String field="field value";

    @XmlAttribute
    private String attribute="attr value";
    
    @XmlAttribute(name="differentAttribute")
    private String oneAttribute="other attr value";
    
    @XmlElement(name="different name")
    private String oneName="different name value";
    
    @XmlTransient
    private String transientField = "will not get serialized ever";
    
    
    @XmlElement
    public String getModifiedTransientValue() {
        return transientField.replace(" ever", ", unless in a getter");
    }
    
    public void setModifiedTransientValue(String val) {} // empty on purpose


    public static void main(String[] args) {
        try {
            JAXB.marshal(new ManualXmlElementsExample(), System.out);
        } catch (Exception e) {
            System.err.println("Exception occurred while writing in XML!");
        }
    }
}

```



## Binding an XML namespace to a serializable Java class.


This is an example of a `package-info.java` file that binds an XML namespace to a serializable Java class. This should be placed in the same package as the Java classes that should be serialized using the namespace.

```java
/**
 * A package containing serializable classes.
 */
@XmlSchema
(
    xmlns =
    {
        @XmlNs(prefix = MySerializableClass.NAMESPACE_PREFIX, namespaceURI = MySerializableClass.NAMESPACE)
    },
    namespace = MySerializableClass.NAMESPACE,
    elementFormDefault = XmlNsForm.QUALIFIED
)
package com.test.jaxb;

import javax.xml.bind.annotation.XmlNs;
import javax.xml.bind.annotation.XmlNsForm;
import javax.xml.bind.annotation.XmlSchema;

```



## Using XmlAdapter to generate desired xml format


When desired XML format differs from Java object model, an XmlAdapter implementation can be used to transform model object into xml-format object and vice versa. This example demonstrates how to put a field's value into an attribute of an element with field's name.

```java
public class XmlAdapterExample {

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class NodeValueElement {
          
        @XmlAttribute(name="attrValue")
        String value;
    
        public NodeValueElement() {
        }
    
        public NodeValueElement(String value) {
            super();
            this.value = value;
        }
    
        public String getValue() {
            return value;
        }
    
        public void setValue(String value) {
            this.value = value;
        }
    }

    public static class ValueAsAttrXmlAdapter extends XmlAdapter<NodeValueElement, String> {        
        @Override
        public NodeValueElement marshal(String v) throws Exception {
            return new NodeValueElement(v);
        }
        
        @Override
        public String unmarshal(NodeValueElement v) throws Exception {
            if (v==null) return "";
            return v.getValue();
        }
    }

    @XmlRootElement(name="DataObject")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DataObject {
        
        String elementWithValue;

        @XmlJavaTypeAdapter(value=ValueAsAttrXmlAdapter.class)
        String elementWithAttribute;
    }

    public static void main(String[] args) {
        DataObject data = new DataObject();
        data.elementWithValue="value1";
        data.elementWithAttribute ="value2";
        
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        JAXB.marshal(data, baos);

        String xmlString = new String(baos.toByteArray(), StandardCharsets.UTF_8);
        
        System.out.println(xmlString);
    }
}

```



## Automatic field/property XML mapping configuration (@XmlAccessorType)


Annotation [`@XmlAccessorType`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/XmlAccessorType.html) determines whether fields/properties will be automatically serialized to XML. Note, that field and method annotations [`@XmlElement`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/XmlElement.html), [`@XmlAttribute`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/XmlAttribute.html) or [`@XmlTransient`](https://docs.oracle.com/javase/8/docs/api/javax/xml/bind/annotation/XmlTransient.html) take precedence over the default settings.

```java
public class XmlAccessTypeExample {

@XmlAccessorType(XmlAccessType.FIELD)
static class AccessorExampleField {
    public String field="value1";
    
    public String getGetter() {
        return "getter";
    }
    
    public void setGetter(String value) {}
}

@XmlAccessorType(XmlAccessType.NONE)
static class AccessorExampleNone {
    public String field="value1";
    
    public String getGetter() {
        return "getter";
    }
    
    public void setGetter(String value) {}
}

@XmlAccessorType(XmlAccessType.PROPERTY)
static class AccessorExampleProperty {
    public String field="value1";
    
    public String getGetter() {
        return "getter";
    }
    
    public void setGetter(String value) {}
}

@XmlAccessorType(XmlAccessType.PUBLIC_MEMBER)
static class AccessorExamplePublic {
    public String field="value1";
    
    public String getGetter() {
        return "getter";
    }
    
    public void setGetter(String value) {}
}

public static void main(String[] args) {
    try {
        System.out.println("\nField:");
        JAXB.marshal(new AccessorExampleField(), System.out);
        System.out.println("\nNone:");
        JAXB.marshal(new AccessorExampleNone(), System.out);
        System.out.println("\nProperty:");
        JAXB.marshal(new AccessorExampleProperty(), System.out);
        System.out.println("\nPublic:");
        JAXB.marshal(new AccessorExamplePublic(), System.out);
    } catch (Exception e) {
        System.err.println("Exception occurred while writing in XML!");
    }
}

} // outer class end

```

Output

```java
Field:
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<accessorExampleField>
    <field>value1</field>
</accessorExampleField>

None:
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<accessorExampleNone/>

Property:
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<accessorExampleProperty>
    <getter>getter</getter>
</accessorExampleProperty>

Public:
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<accessorExamplePublic>
    <field>value1</field>
    <getter>getter</getter>
</accessorExamplePublic>

```



## Specifying a XmlAdapter instance to (re)use existing data


Sometimes specific instances of data should be used. Recreation is not desired and referencing `static` data would have a code smell.

It is possible to specify a `XmlAdapter` instance the `Unmarshaller` should use, which allows the user to use `XmlAdapter`s with no zero-arg constructor and/or pass data to the adapter.

### Example

### User class

The following class contains a name and a user's image.

```java
import java.awt.image.BufferedImage;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement
public class User {
    
    private String name;
    private BufferedImage image;

    @XmlAttribute
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @XmlJavaTypeAdapter(value=ImageCacheAdapter.class)
    @XmlAttribute
    public BufferedImage getImage() {
        return image;
    }

    public void setImage(BufferedImage image) {
        this.image = image;
    }

    public User(String name, BufferedImage image) {
        this.name = name;
        this.image = image;
    }

    public User() {
        this("", null);
    }

}

```

### Adapter

To avoid creating the same image in memory twice (as well as downloading the data again), the adapter stores the images in a map.

For valid Java 7 code replace the `getImage` method with

```java
public BufferedImage getImage(URL url) {
    BufferedImage image = imageCache.get(url);
    if (image == null) {
        try {
            image = ImageIO.read(url);
        } catch (IOException ex) {
            Logger.getLogger(ImageCacheAdapter.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
        imageCache.put(url, image);
        reverseIndex.put(image, url);
    }
    return image;
}

```

```java
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.xml.bind.annotation.adapters.XmlAdapter;

public class ImageCacheAdapter extends XmlAdapter<String, BufferedImage> {

    private final Map<URL, BufferedImage> imageCache = new HashMap<>();
    private final Map<BufferedImage, URL> reverseIndex = new HashMap<>();

    public BufferedImage getImage(URL url) {
        // using a single lookup using Java 8 methods
        return imageCache.computeIfAbsent(url, s -> {
            try {
                BufferedImage img = ImageIO.read(s);
                reverseIndex.put(img, s);
                return img;
            } catch (IOException ex) {
                Logger.getLogger(ImageCacheAdapter.class.getName()).log(Level.SEVERE, null, ex);
                return null;
            }
        });
    }

    @Override
    public BufferedImage unmarshal(String v) throws Exception {
        return getImage(new URL(v));
    }

    @Override
    public String marshal(BufferedImage v) throws Exception {
        return reverseIndex.get(v).toExternalForm();
    }

}

```

### Example XMLs

The following 2 xmls are for **Jon Skeet** and his earth 2 counterpart, which both look exactly the same and therefore use the same avatar.

```java
<?xml version="1.0" encoding="UTF-8"?>

<user name="Jon Skeet" image="https://www.gravatar.com/avatar/6d8ebb117e8d83d74ea95fbdd0f87e13?s=328&amp;d=identicon&amp;r=PG"/>

```

```java
<?xml version="1.0" encoding="UTF-8"?>

<user name="Jon Skeet (Earth 2)" image="https://www.gravatar.com/avatar/6d8ebb117e8d83d74ea95fbdd0f87e13?s=328&amp;d=identicon&amp;r=PG"/>

```

### Using the adapter

```java
ImageCacheAdapter adapter = new ImageCacheAdapter();

JAXBContext context = JAXBContext.newInstance(User.class);

Unmarshaller unmarshaller = context.createUnmarshaller();

// specifiy the adapter instance to use for every
// @XmlJavaTypeAdapter(value=ImageCacheAdapter.class)
unmarshaller.setAdapter(ImageCacheAdapter.class, adapter);

User result1 = (User) unmarshaller.unmarshal(Main.class.getResource("user.xml"));

// unmarshal second xml using the same adapter instance
Unmarshaller unmarshaller2 = context.createUnmarshaller();
unmarshaller2.setAdapter(ImageCacheAdapter.class, adapter);
User result2 = (User) unmarshaller2.unmarshal(Main.class.getResource("user2.xml"));

System.out.println(result1.getName());
System.out.println(result2.getName());

// yields true, since image is reused
System.out.println(result1.getImage() == result2.getImage());

```



## Using XmlAdapter to trim string.


```java
package com.example.xml.adapters;

import javax.xml.bind.annotation.adapters.XmlAdapter;

public class StringTrimAdapter extends XmlAdapter<String, String> {
    @Override
    public String unmarshal(String v) throws Exception {
        if (v == null)
            return null;
        return v.trim();
    }

    @Override
    public String marshal(String v) throws Exception {
        if (v == null)
            return null;
        return v.trim();
    }
}

```

And in package-info.java add following declaration.

```java
@XmlJavaTypeAdapter(value = com.example.xml.adapters.StringTrimAdapter.class, type = String.class)
package com.example.xml.jaxb.bindings;// Packge where you intend to apply trimming filter

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

```



#### Syntax


<li>
JAXB.marshall(object, fileObjOfXML);
</li>
<li>
Object obj = JAXB.unmarshall(fileObjOfXML, className);
</li>



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|fileObjOfXML|`File` object of an XML file
|className|Name of a class with `.class` extension



#### Remarks


Using the XJC tool available in the JDK, java code for a xml structure described in a xml schema (`.xsd` file) can be automatically generated, see [XJC topic](http://stackoverflow.com/documentation/java/4538/xjc#t=201607290805511961821).

