---
metaTitle: "Hashtable"
description: "Hashtable"
---

# Hashtable


**Hashtable** is a class in Java collections which implements Map interface and extends the Dictionary Class

Contains only unique elements and its synchronized



## Hashtable


```java
import java.util.*;  
public class HashtableDemo {  
   public static void main(String args[]) {  
   // create and populate hash table  
   Hashtable<Integer, String> map = new Hashtable<Integer, String>();           
   map.put(101,"C Language");  
   map.put(102, "Domain");  
   map.put(104, "Databases");  
   System.out.println("Values before remove: "+ map);    
   // Remove value for key 102  
   map.remove(102);  
   System.out.println("Values after remove: "+ map);  
   }      
}

```

