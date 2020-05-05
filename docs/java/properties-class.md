---
metaTitle: "Java - Properties Class"
description: "Loading properties, Property files caveat: trailing whitespace, Saving Properties as XML"
---

# Properties Class


The properties object contains key and value pair both as a string. The java.util.Properties class is the subclass of Hashtable.

It can be used to get property value based on the property key. The Properties class provides methods to get data from properties file and store data into properties file. Moreover, it can be used to get properties of system.

Advantage of properties file

Recompilation is not required, if information is changed from properties file: If any information is changed from



## Loading properties


To load a properties file bundled with your application:

```java
public class Defaults {

    public static Properties loadDefaults() {
        try (InputStream bundledResource =
            Defaults.class.getResourceAsStream("defaults.properties")) {

            Properties defaults = new Properties();
            defaults.load(bundledResource);
            return defaults;
        } catch (IOException e) {
            // Since the resource is bundled with the application,
            // we should never get here.
            throw new UncheckedIOException(
                "defaults.properties not properly packaged"
                + " with application", e);
        }
    }

}

```



## Property files caveat: trailing whitespace


Take a close look at these two property files which are seemingly completely identical:

[<img src="http://i.stack.imgur.com/PogHE.png" alt="enter image description here" />](http://i.stack.imgur.com/PogHE.png)

except they are really not identical:

[<img src="http://i.stack.imgur.com/RV40f.png" alt="enter image description here" />](http://i.stack.imgur.com/RV40f.png)

(screenshots are from Notepad++)

Since trailing whitespace is preserved the value of `lastName` would be `"Smith"` in the first case and `"Smith "` in the second case.

Very rarely this is what users expect and one and can only speculate why this is the default behavior of `Properties` class. It is however easy to create an enhanced version of `Properties` that fixes this problem. The following class, **TrimmedProperties**, does just that. It is a drop-in replacement for standard Properties class.

```java
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Map.Entry;
import java.util.Properties;

/**
 * Properties class where values are trimmed for trailing whitespace if the
 * properties are loaded from a file.
 *
 * <p>
 * In the standard {@link java.util.Properties Properties} class trailing
 * whitespace is always preserved. When loading properties from a file such
 * trailing whitespace is almost always <i>unintentional</i>. This class fixes
 * this problem. The trimming of trailing whitespace only takes place if the
 * source of input is a file and only where the input is line oriented (meaning
 * that for example loading from XML file is <i>not</i> changed by this class).
 * For this reason this class is almost in all cases a safe drop-in replacement
 * for the standard <tt>Properties</tt>
 * class.
 *
 * <p>
 * Whitespace is defined here as any of space (U+0020) or tab (U+0009).
 * * 
 */
public class TrimmedProperties extends Properties {

    /**
     * Reads a property list (key and element pairs) from the input byte stream.
     * 
     * <p>Behaves exactly as {@link java.util.Properties#load(java.io.InputStream) }
     * with the exception that trailing whitespace is trimmed from property values
     * if <tt>inStream</tt> is an instance of <tt>FileInputStream</tt>.
     * 
     * @see java.util.Properties#load(java.io.InputStream) 
     * @param inStream the input stream.
     * @throws IOException if an error occurred when reading from the input stream.
     */
    @Override
    public void load(InputStream inStream) throws IOException {
        if (inStream instanceof FileInputStream) {
            // First read into temporary props using the standard way
            Properties tempProps = new Properties();
            tempProps.load(inStream);
            // Now trim and put into target
            trimAndLoad(tempProps);
        } else {
            super.load(inStream);
        }
    }

    /**
     * Reads a property list (key and element pairs) from the input character stream in a simple line-oriented format. 
     * 
     * <p>Behaves exactly as {@link java.util.Properties#load(java.io.Reader)}
     * with the exception that trailing whitespace is trimmed on property values
     * if <tt>reader</tt> is an instance of <tt>FileReader</tt>.
     * 
     * @see java.util.Properties#load(java.io.Reader) }
     * @param reader the input character stream.
     * @throws IOException if an error occurred when reading from the input stream.
     */
    @Override
    public void load(Reader reader) throws IOException {
        if (reader instanceof FileReader) {
            // First read into temporary props using the standard way
            Properties tempProps = new Properties();
            tempProps.load(reader);
            // Now trim and put into target
            trimAndLoad(tempProps);
        } else {
            super.load(reader);
        }
    }

    private void trimAndLoad(Properties p) {
        for (Entry<Object, Object> entry : p.entrySet()) {
            if (entry.getValue() instanceof String) {
                put(entry.getKey(), trimTrailing((String) entry.getValue()));
            } else {
                put(entry.getKey(), entry.getValue());
            }
        }
    }

    /**
     * Trims trailing space or tabs from a string.
     *
     * @param str
     * @return
     */
    public static String trimTrailing(String str) {
        if (str != null) {
            // read str from tail until char is no longer whitespace
            for (int i = str.length() - 1; i >= 0; i--) {
                if ((str.charAt(i) != ' ') && (str.charAt(i) != '\t')) {
                    return str.substring(0, i + 1);
                }
            }
        }
        return str;
    }
}

```



## Saving Properties as XML


**Storing Properties in a XML File**

The way you store properties files as XML files is very similar to the way you would store them as `.properties` files. Just instead of using the `store()` you would use `storeToXML()`.

```java
public void saveProperties(String location) throws IOException{
    // make new instance of properties
    Properties prop = new Properties();
    
    // set the property values
    prop.setProperty("name", "Steve");
    prop.setProperty("color", "green");
    prop.setProperty("age", "23");
    
    // check to see if the file already exists
    File file = new File(location);
    if (!file.exists()){
        file.createNewFile();
    }
    
    // save the properties
    prop.storeToXML(new FileOutputStream(file), "testing properties with xml");
}

```

When you open the file it will look like this.

[<img src="http://i.stack.imgur.com/svWEh.png" alt="screenshot of the file made" />](http://i.stack.imgur.com/svWEh.png)

**Loading Properties from a XML File**

Now to load this file as a `properties` you need to call the `loadFromXML()` instead of the `load()` that you would use with regular `.propeties` files.

```java
public static void loadProperties(String location) throws FileNotFoundException, IOException{
    // make new properties instance to load the file into
    Properties prop = new Properties();
    
    // check to make sure the file exists
    File file = new File(location);
    if (file.exists()){
        // load the file
        prop.loadFromXML(new FileInputStream(file));
        
        // print out all the properties
        for (String name : prop.stringPropertyNames()){
            System.out.println(name + "=" + prop.getProperty(name));
        }
    } else {
        System.err.println("Error: No file found at: " + location);
    }
}

```

When you run this code you will get the following in the console:

```java
age=23
color=green
name=Steve

```



#### Syntax


- In a properties file:
- key=value
- #comment



#### Remarks


A Properties object is a [Map](http://docs.oracle.com/javase/8/docs/api/java/util/Map.html) whose keys and values are Strings by convention.  Although the methods of Map can be used to access the data, the more type-safe methods [getProperty](http://docs.oracle.com/javase/8/docs/api/java/util/Properties.html#getProperty-java.lang.String-), [setProperty](http://docs.oracle.com/javase/8/docs/api/java/util/Properties.html#setProperty-java.lang.String-java.lang.String-), and [stringPropertyNames](http://docs.oracle.com/javase/8/docs/api/java/util/Properties.html#stringPropertyNames--) are usually used instead.

Properties are frequently stored in Java property files, which are simple text files.  Their format is documented thoroughly in the [Properties.load method](http://docs.oracle.com/javase/8/docs/api/java/util/Properties.html#load-java.io.Reader-).  In summary:

- Each key/value pair is a line of text with whitespace, equals (`=`), or colon (`:`) between the key and the value. The equals or colon may have any amount of whitespace before and after it, which is ignored.
- Leading whitespace is always ignored, trailing whitespace is always included.
- A backslash can be used to escape any character (except lowercase `u`).
- A backslash at the end of the line indicates the next line is a continuation of the current line.  However, as with all lines, leading whitespace in the continuation line is ignored.
- Just like in Java source code, `\u` followed by four hexadecimal digits represents a UTF-16 character.

Most frameworks, including Java SE’s own facilities like java.util.ResourceBundle, load property files as InputStreams.  When loading a property file from an InputStream, that file is may only contain ISO 8859-1 characters (that is, characters in the 0–255 range).  Any other characters must be represented as `\u` escapes.  However, you can write a text file in any encoding and use the [native2ascii](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/native2ascii.html) tool (which comes with every JDK) to do that escaping for you.

If you are loading a property file with your own code, it can be in any encoding, as long as you create a Reader (such as an [InputStreamReader](http://docs.oracle.com/javase/8/docs/api/java/io/InputStreamReader.html)) based on the corresponding [Charset](http://docs.oracle.com/javase/8/docs/api/java/nio/charset/Charset.html).  You can then load the file using [load(Reader)](http://docs.oracle.com/javase/8/docs/api/java/util/Properties.html#load-java.io.Reader-) instead of the legacy load(InputStream) method.

You can also store properties in a simple XML file, which allows the file itself to define the encoding.  Such a file can be loaded with the [loadFromXML](http://docs.oracle.com/javase/8/docs/api/java/util/Properties.html#loadFromXML-java.io.InputStream-) method.  The DTD describing the structure of such XML files is located at [http://java.sun.com/dtd/properties.dtd](http://java.sun.com/dtd/properties.dtd) .

