---
metaTitle: "Spring - Spring Expression Language (SpEL)"
description: "Syntax Reference"
---

# Spring Expression Language (SpEL)




## Syntax Reference


You can use `@Value("#{expression}")` to inject value at runtime, in which the `expression` is a SpEL expression.

### **Literal expressions**

Supported types include strings, dates, numeric values (int, real, and hex), boolean and null.

```java
"#{'Hello World'}"  //strings
"#{3.1415926}"      //numeric values (double)
"#{true}"           //boolean
"#{null}"           //null

```

### **Inline list**

```java
"#{1,2,3,4}"              //list of number
"#{{'a','b'},{'x','y'}}"  //list of list

```

### **Inline Maps**

```java
"#{name:'Nikola',dob:'10-July-1856'}" 
"#{name:{first:'Nikola',last:'Tesla'},dob:{day:10,month:'July',year:1856}}" //map of maps

```

### **Invoking Methods**

```java
"#{'abc'.length()}"      //evaluates to 3
"#{f('hello')}"   //f is a method in the class to which this expression belongs, it has a string parameter

```

