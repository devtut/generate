---
metaTitle: "Java - Local Inner Class"
description: "Local Inner Class"
---

# Local Inner Class


A class i.e. created inside a method is called local inner class in java. If you want to invoke the methods of local inner class, you must instantiate this class inside the method.



## Local Inner Class


```java
public class localInner1{  
 private int data=30;//instance variable  
 void display(){  
  class Local{  
   void msg(){System.out.println(data);}  
  }  
  Local l=new Local();  
  l.msg();  
 }  
 public static void main(String args[]){  
  localInner1 obj=new localInner1();  
  obj.display();  
 }  
}  

```

