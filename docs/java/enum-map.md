---
metaTitle: "Enum Map"
description: "Enum Map Book Example"
---

# Enum Map


Java EnumMap class is the specialized Map implementation for enum keys. It inherits Enum and AbstractMap classes.

the Parameters for java.util.EnumMap class.

K: It is the type of keys maintained by this map.
V: It is the type of mapped values.



## Enum Map Book Example


```java
import java.util.*;    
class Book {    
int id;    
String name,author,publisher;    
int quantity;    
public Book(int id, String name, String author, String publisher, int quantity) {    
    this.id = id;    
    this.name = name;    
    this.author = author;    
    this.publisher = publisher;    
    this.quantity = quantity;    
}    
}    
public class EnumMapExample {   
// Creating enum  
    public enum Key{  
           One, Two, Three  
           };  
public static void main(String[] args) {    
    EnumMap<Key, Book> map = new EnumMap<Key, Book>(Key.class);  
    // Creating Books    
    Book b1=new Book(101,"Let us C","Yashwant Kanetkar","BPB",8);    
    Book b2=new Book(102,"Data Communications & Networking","Forouzan","Mc Graw Hill",4);    
    Book b3=new Book(103,"Operating System","Galvin","Wiley",6);    
    // Adding Books to Map   
       map.put(Key.One, b1);  
       map.put(Key.Two, b2);  
       map.put(Key.Three, b3);  
    // Traversing EnumMap  
       for(Map.Entry<Key, Book> entry:map.entrySet()){      
            Book b=entry.getValue();    
            System.out.println(b.id+" "+b.name+" "+b.author+" "+b.publisher+" "+b.quantity);     
        }       
}    
} 

```

