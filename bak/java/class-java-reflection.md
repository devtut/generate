---
metaTitle: "Java - Class - Java Reflection"
description: "getClass() method of Object class"
---

# Class - Java Reflection


The java.lang.Class class provides many methods that can be used to get metadata, examine and change the run time behavior of a class.

The java.lang and java.lang.reflect packages provide classes for java reflection.

Where it is used

The Reflection API is mainly used in:

IDE (Integrated Development Environment) e.g. Eclipse, MyEclipse, NetBeans etc.
Debugger
Test Tools etc.



## getClass() method of Object class


```java
class Simple { }  
  
class Test {  
     void printName(Object obj){  
         Class c = obj.getClass();    
         System.out.println(c.getName());  
     }  
     public static void main(String args[]){  
         Simple s = new Simple();  
   
         Test t = new Test();  
         t.printName(s);
    }  
}

```

