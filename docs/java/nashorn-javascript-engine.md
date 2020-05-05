---
metaTitle: "Nashorn JavaScript engine"
description: "Execute JavaScript file, Intercept script output, Hello Nashorn, Evaluate Arithmetic Strings, Set global variables, Set and get global variables, Usage of Java objects in JavaScript in Nashorn, Implementing an interface from script"
---

# Nashorn JavaScript engine


[Nashorn](https://en.wikipedia.org/wiki/Nashorn_(JavaScript_engine)) is a JavaScript engine developed in Java by Oracle, and has been released with Java 8. Nashorn allows embedding Javascript in Java applications via JSR-223 and allows to develop standalone Javascript applications, and [it provides better](http://www.oracle.com/technetwork/articles/java/jf14-nashorn-2126515.html) runtime performance and better compliance with the ECMA normalized Javascript specification.



## Execute JavaScript file


```java
// Required imports
import javax.script.ScriptEngineManager;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import java.io.FileReader;
import java.io.FileNotFoundException;

// Obtain an instance of the JavaScript engine
ScriptEngineManager manager = new ScriptEngineManager();
ScriptEngine engine = manager.getEngineByName("nashorn");

// Load and execute a script from the file 'demo.js'
try {
    engine.eval(new FileReader("demo.js"));
} catch (FileNotFoundException ex) {
    ex.printStackTrace();
} catch (ScriptException ex) {
    // This is the generic Exception subclass for the Scripting API
    ex.printStackTrace();
}

// Outcome:
// 'Script from file!' printed on standard output

```

**demo.js**:

```java
print('Script from file!');

```



## Intercept script output


```java
// Obtain an instance of JavaScript engine
ScriptEngineManager manager = new ScriptEngineManager();
ScriptEngine engine = manager.getEngineByName("nashorn");

// Setup a custom writer
StringWriter stringWriter = new StringWriter();
// Modify the engine context so that the custom writer is now the default
// output writer of the engine
engine.getContext().setWriter(stringWriter);

// Execute some script
try {
    engine.eval("print('Redirected text!');");
} catch (ScriptException ex) {
    ex.printStackTrace();
}

// Outcome:
// Nothing printed on standard output, but
// stringWriter.toString() contains 'Redirected text!'

```



## Hello Nashorn


```java
// Obtain an instance of JavaScript engine
ScriptEngineManager manager = new ScriptEngineManager();
ScriptEngine engine = manager.getEngineByName("nashorn");

// Execute an hardcoded script
try {
    engine.eval("print('Hello Nashorn!');");
} catch (ScriptException ex) {
    // This is the generic Exception subclass for the Scripting API
    ex.printStackTrace();
}

// Outcome:
// 'Hello Nashorn!' printed on standard output

```



## Evaluate Arithmetic Strings


```java
// Obtain an instance of JavaScript engine
ScriptEngineManager manager = new ScriptEngineManager();
ScriptEngine engine = manager.getEngineByName("JavaScript");

//String to be evaluated
String str = "3+2*4+5";
//Value after doing Arithmetic operation with operator precedence will be 16

//Printing the value
try {
    System.out.println(engine.eval(str));
} catch (ScriptException ex) {
    ex.printStackTrace();
}

//Outcome:
//Value of the string after arithmetic evaluation is printed on standard output.
//In this case '16.0' will be printed on standard output.

```



## Set global variables


```java
// Obtain an instance of JavaScript engine
ScriptEngineManager manager = new ScriptEngineManager();
ScriptEngine engine = manager.getEngineByName("nashorn");

// Define a global variable
engine.put("textToPrint", "Data defined in Java.");

// Print the global variable
try {
    engine.eval("print(textToPrint);");
} catch (ScriptException ex) {
    ex.printStackTrace();
}

// Outcome:
// 'Data defined in Java.' printed on standard output

```



## Set and get global variables


```java
// Obtain an instance of JavaScript engine
ScriptEngineManager manager = new ScriptEngineManager();
ScriptEngine engine = manager.getEngineByName("nashorn");

try {
    // Set value in the global name space of the engine
    engine.put("name","Nashorn");
    // Execute an hardcoded script
    engine.eval("var value='Hello '+name+'!';");
    // Get value
    String value=(String)engine.get("value");
    System.out.println(value);
} catch (ScriptException ex) {
    // This is the generic Exception subclass for the Scripting API
    ex.printStackTrace();
}

// Outcome:
// 'Hello Nashorn!' printed on standard output

```



## Usage of Java objects in JavaScript in Nashorn


It's possible to pass Java objects to Nashorn engine to be processed in Java code. At the same time, there are some JavaScript (and Nashorn) specific constructions, and it's not always clear how they work with java objects.

Below there is a table which describes behaviour of native Java objects inside JavaScript constructions.

Tested constructions:

<li>Expression in if clause.
In JS expression in if clause doesn't have to be boolean unlike Java. It's evaluated as false for so called falsy values (null, undefined, 0, empty strings etc)</li>
<li>for each statement
Nashorn has a special kind of loop - for each - which can iterate over different JS and Java object.</li>
<li>Getting object size.
In JS objects have a property length, which returns size of an array or a string.</li>

Results:

|Type|If|for each|.length
|---|---|---|---|---|---|---|---|---|---
|Java null|false|No iterations|**Exception**
|Java empty string|false|No iterations|0
|Java string|true|Iterates over string characters|Length of the string
|Java Integer/Long|value != 0|No iterations|undefined
|Java ArrayList|true|Iterates over elements|Length of the list
|Java HashMap|true|Iterates over values|null
|Java HashSet|true|Iterates over items|undefined

**Recommendatons:**

- It's advisable to use `if (some_string)` to check if a string is not null and not empty
- `for each` can be safely used to iterate over any collection, and it doesn't raise exceptions if the collection is not iterable, null or undefined
- Before getting length of an object it must be checked for null or undefined (the same is true for any attempt of calling a method or getting a property of Java object)



## Implementing an interface from script


```java
import java.io.FileReader;
import java.io.IOException;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class InterfaceImplementationExample {
    public static interface Pet {
        public void eat();
    }

    public static void main(String[] args) throws IOException {
        // Obtain an instance of JavaScript engine
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("nashorn");

        try {
            //evaluate a script
            /* pet.js */
            /*
                var Pet = Java.type("InterfaceImplementationExample.Pet");
                
                new Pet() {
                    eat: function() { print("eat"); }
                }            
            */
            
            Pet pet = (Pet) engine.eval(new FileReader("pet.js"));
            
            pet.eat();
        } catch (ScriptException ex) {
            ex.printStackTrace();
        }

        // Outcome:
        // 'eat' printed on standard output
    }
}

```



#### Syntax


- ScriptEngineManager // Provides a discovery and installation mechanism for ScriptEngine classes; uses a SPI (Service Provider Interface)
- ScriptEngineManager.ScriptEngineManager() // Recommended constructor
- ScriptEngine // Provides the interface to the scripting language
- ScriptEngine ScriptEngineManager.getEngineByName(String shortName) // Factory method for the given implementation
- Object ScriptEngine.eval(String script) // Executes the specified script
- Object ScriptEngine.eval(Reader reader) // Loads and then executes a script from the specified source
- ScriptContext ScriptEngine.getContext() // Returns the default bindings, readers and writers provider
- void ScriptContext.setWriter(Writer writer) // Sets the destination to send script output to



#### Remarks


Nashorn is a JavaScript engine written in Java and included in Java 8. Everything you need is bundled in the `javax.script` package.

Note that the `ScriptEngineManager` provides a generic API allowing you to obtain script engines for various scripting languages (i.e. not only Nashorn, not only JavaScript).

