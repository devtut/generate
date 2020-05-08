---
metaTitle: "Android - Annotation Processor"
description: "@NonNull Annotation, Types of Annotations, Creating and Using  Custom Annotations"
---

# Annotation Processor


Annotation processor is a tool build in javac for scanning and processing annotations at compile time.

Annotations are a class of metadata that can be associated with classes, methods, fields, and even other annotations.There are two ways to access these annotations at runtime via reflection and at compile time via annotation processors.



## @NonNull Annotation


```java
public class Foo {
    private String name;
    public Foo(@NonNull String name){...};
    ...
}

```

Here @NonNull is annotation which is processed compile time by the android studio to warn you that the particular function needs non null parameter.



## Types of Annotations


There are three types of annotations.

<li>
**Marker Annotation** - annotation that has no method

```java
@interface CustomAnnotation {}

```


</li>

<li>
**Single-Value Annotation** - annotation that has one method

```java
@interface CustomAnnotation {  
    int value();  
}

```


</li>

<li>
**Multi-Value Annotation** - annotation that has more than one method

```java
@interface CustomAnnotation{  
    int value1();  
    String value2();  
    String value3();  
}

```


</li>



## Creating and Using  Custom Annotations


For creating custom annotations we need to decide

<li>Target - on which these annotations will work on like field level,
method level, type level etc.</li>
- Retention - to what level annotation will be available.

For this, we have built in custom annotations. Check out these mostly used ones:

**@Target**

[<img src="https://i.stack.imgur.com/JTB0w.png" alt="Target and what it means" />](https://i.stack.imgur.com/JTB0w.png)

**@Retention**

[<img src="https://i.stack.imgur.com/P4Ota.png" alt="Retention and what it means" />](https://i.stack.imgur.com/P4Ota.png)

**Creating Custom Annotation**

```java
@Retention(RetentionPolicy.SOURCE) // will not be available in compiled class   
@Target(ElementType.METHOD) // can be applied to methods only
@interface CustomAnnotation{  
      int value();    
}

```

**Using Custom Annotation**

```java
class Foo{  
  @CustomAnnotation(value = 1)  // will be used by an annotation processor
  public void foo(){..}  
}

```

the value provided inside `@CustomAnnotation` will be consumed by an Annotationprocessor may be to generate code at compile time etc.

