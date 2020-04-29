---
metaTitle: "XOM - XML Object Model"
description: "Reading a XML file, Writing to a XML File"
---

# XOM - XML Object Model



## Reading a XML file


In order to load the XML data with [XOM](http://www.xom.nu/) you will need to make a `Builder` from which you can build it into a `Document`.

```java
Builder builder = new Builder();
Document doc = builder.build(file);

```

To get the root element, the highest parent in the xml file, you need to use the `getRootElement()` on the `Document` instance.

```java
Element root = doc.getRootElement();

```

Now the Element class has a lot of handy methods that make reading xml really easy. Some of the most useful are listed below:

- `getChildElements(String name)` - returns an `Elements` instance that acts as an array of elements
- `getFirstChildElement(String name)` - returns the first child element with that tag.
- `getValue()` - returns the value inside the element.
- `getAttributeValue(String name)` - returns the value of an attribute with the specified name.

When you call the `getChildElements()` you get a `Elements` instance. From this you can loop through and call the `get(int index)` method on it to retrieve all the elements inside.

```java
Elements colors = root.getChildElements("color");
for (int q = 0; q < colors.size(); q++){
    Element color = colors.get(q);
}

```

**Example:** Here is an example of reading an XML File:

XML File:

[<img src="http://i.stack.imgur.com/gHNjo.png" alt="example of xml" />](http://i.stack.imgur.com/gHNjo.png)

Code for reading and printing it:

```java
import java.io.File;
import java.io.IOException;
import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Elements;
import nu.xom.ParsingException;

public class XMLReader {
    
    public static void main(String[] args) throws ParsingException, IOException{
        File file = new File("insert path here");
        // builder builds xml data
        Builder builder = new Builder();
        Document doc = builder.build(file);
        
        // get the root element <example>
        Element root = doc.getRootElement();
        
        // gets all element with tag <person>
        Elements people = root.getChildElements("person");
        
        for (int q = 0; q < people.size(); q++){
            // get the current person element
            Element person = people.get(q);
            
            // get the name element and its children: first and last
            Element nameElement = person.getFirstChildElement("name");
            Element firstNameElement = nameElement.getFirstChildElement("first");
            Element lastNameElement = nameElement.getFirstChildElement("last");
            
            // get the age element
            Element ageElement = person.getFirstChildElement("age");
            
            // get the favorite color element
            Element favColorElement = person.getFirstChildElement("fav_color");
            
            String fName, lName, ageUnit, favColor;
            int age;
            
            try {
                fName = firstNameElement.getValue();
                lName = lastNameElement.getValue();
                age = Integer.parseInt(ageElement.getValue());
                ageUnit = ageElement.getAttributeValue("unit");
                favColor = favColorElement.getValue();
                
                System.out.println("Name: " + lName + ", " + fName);
                System.out.println("Age: " + age + " (" + ageUnit + ")");
                System.out.println("Favorite Color: " + favColor);
                System.out.println("----------------");
                
            } catch (NullPointerException ex){
                ex.printStackTrace();
            } catch (NumberFormatException ex){
                ex.printStackTrace();
            }
        }
    }
    
}

```

This will print out in the console:

```java
Name: Smith, Dan
Age: 23 (years)
Favorite Color: green
----------------
Name: Autry, Bob
Age: 3 (months)
Favorite Color: N/A
----------------

```



## Writing to a XML File


Writing to a XML File using [XOM](http://www.xom.nu/) is very similar to reading it except in this case we are making the instances instead of retrieving them off the root.

To make a new Element use the constructor `Element(String name)`. You will want to make a root element so that you can easily add it to a `Document`.

```java
Element root = new Element("root");

```

The `Element` class has some handy methods for editing elements. They are listed below:

- `appendChild(String name)` - this will basically set the value of the element to name.
- `appendChild(Node node)` - this will make `node` the elements parent. (Elements are nodes so you can parse elements).
- `addAttribute(Attribute attribute)` - will add an attribute to the element.

The `Attribute` class has a couple of different constructors. The simplest one is `Attribute(String name, String value)`.

Once you have all of your elements add to your root element you can turn it into a `Document`. `Document` will take a `Element` as an argument in it's constructor.

You can use a `Serializer` to write your XML to a file. You will need to make a new output stream to parse in the constructor of `Serializer`.

```java
FileOutputStream fileOutputStream = new FileOutputStream(file);
Serializer serializer = new Serializer(fileOutputStream, "UTF-8");
serializer.setIndent(4);
serializer.write(doc);

```

**Example**

Code:

```java
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import nu.xom.Attribute;
import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Elements;
import nu.xom.ParsingException;
import nu.xom.Serializer;

public class XMLWriter{
    
    public static void main(String[] args) throws UnsupportedEncodingException, 
            IOException{
        // root element <example>
        Element root = new Element("example");
        
        // make a array of people to store
        Person[] people = {new Person("Smith", "Dan", "years", "green", 23), 
            new Person("Autry", "Bob", "months", "N/A", 3)};
        
        // add all the people
        for (Person person : people){
            
            // make the main person element <person>
            Element personElement = new Element("person");
            
            // make the name element and it's children: first and last
            Element nameElement = new Element("name");
            Element firstNameElement = new Element("first");
            Element lastNameElement = new Element("last");
            
            // make age element
            Element ageElement = new Element("age");
            
            // make favorite color element
            Element favColorElement = new Element("fav_color");
            
            // add value to names
            firstNameElement.appendChild(person.getFirstName());
            lastNameElement.appendChild(person.getLastName());
            
            // add names to name
            nameElement.appendChild(firstNameElement);
            nameElement.appendChild(lastNameElement);
            
            // add value to age
            ageElement.appendChild(String.valueOf(person.getAge()));
            
            // add unit attribute to age
            ageElement.addAttribute(new Attribute("unit", person.getAgeUnit()));
            
            // add value to favColor
            favColorElement.appendChild(person.getFavoriteColor());
            
            // add all contents to person
            personElement.appendChild(nameElement);
            personElement.appendChild(ageElement);
            personElement.appendChild(favColorElement);
            
            // add person to root
            root.appendChild(personElement);
        }
        
        // create doc off of root
        Document doc = new Document(root);
        
        // the file it will be stored in
        File file = new File("out.xml");
        if (!file.exists()){
            file.createNewFile();
        }
        
        // get a file output stream ready
        FileOutputStream fileOutputStream = new FileOutputStream(file);
        
        // use the serializer class to write it all
        Serializer serializer = new Serializer(fileOutputStream, "UTF-8");
        serializer.setIndent(4);
        serializer.write(doc);
    }

    private static class Person {

        private String lName, fName, ageUnit, favColor;
        private int age;

        public Person(String lName, String fName, String ageUnit, String favColor, int age){
            this.lName = lName;
            this.fName = fName;
            this.age = age;
            this.ageUnit = ageUnit;
            this.favColor = favColor;
        }

        public String getLastName() { return lName; }
        public String getFirstName() { return fName; }
        public String getAgeUnit() { return ageUnit; }
        public String getFavoriteColor() { return favColor; }
        public int getAge() { return age; }
    }
    
}

```

This will be the contents of "out.xml":

[<img src="http://i.stack.imgur.com/k4tmR.png" alt="xml file" />](http://i.stack.imgur.com/k4tmR.png)

