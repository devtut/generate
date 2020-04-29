---
metaTitle: "JavaBean"
description: "Basic Java Bean"
---

# JavaBean


JavaBeans (TM) is a pattern for designing Java class APIs that allows instances (beans) to be used in various contexts and using various tools **without** explicitly writing Java code.  The patterns consists of conventions for defining getters and setters for **properties**, for defining constructors, and for defining event listener APIs.



## Basic Java Bean


```java
public class BasicJavaBean implements java.io.Serializable{
    
    private int value1;
    private String value2;
    private boolean value3;

    public BasicJavaBean(){}

    public void setValue1(int value1){
        this.value1 = value1;
    }

    public int getValue1(){
        return value1;
    }
    
    public void setValue2(String value2){
        this.value2 = value2;
    }
    
    public String getValue2(){
        return value2;
    }

    public void setValue3(boolean value3){
        this.value3 = value3;
    }
    
    public boolean isValue3(){
        return value3;
    }
}

```



#### Syntax


- **JavaBean Property Naming Rules**
<li>If the property is not a boolean, the getter method's prefix must be
get. For example, getSize()is a valid JavaBeans getter name for a
property named "size." Keep in mind that you do not need to have a
variable named size. The name of the property is inferred from the
getters and setters, not through any variables in your class. What you return
from getSize() is up to you.</li>
<li>If the property is a boolean, the getter method's prefix is either
get or is. For example, getStopped() or isStopped() are both valid
JavaBeans names for a boolean property.</li>
<li>The setter method's prefix must be set. For example, setSize() is the
valid JavaBean name for a property named size.</li>
<li>To complete the name of a getter or setter method, change the first
letter of the property name to uppercase, and then append it to the
appropriate prefix (get, is, or set).</li>
<li>Setter method signatures must be marked public, with a void return
type and an argument that represents the property type.</li>
<li>Getter method signatures must be marked public, take no arguments,
and have a return type that matches the argument type of the setter
method for that property.</li>
- **JavaBean Listener Naming Rules**
<li>Listener method names used to "register" a listener with an event
source must use the prefix add, followed by the listener type. For
example, addActionListener() is a valid name for a method that an
event source will have to allow others to register for Action events.</li>
<li>Listener method names used to remove ("unregister") a listener must
use the prefix remove, followed by the listener type (using the same
rules as the registration add method).</li>
<li>The type of listener to be added or removed must be passed as the
argument to the method.</li>
- Listener method names must end with the word "Listener".



#### Remarks


In order for a class to be a Java Bean must follow this [standard](http://www.oracle.com/technetwork/java/javase/documentation/spec-136004.html) - in summary:

- All of its properties must be private and only accessible through getters and setters.
- It must have a public no-argument constructor.
- Must implement the `java.io.Serializable` interface.

