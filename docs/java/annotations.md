---
metaTitle: "Annotations"
description: "Defining annotation types, Runtime annotation checks via reflection, Built-in annotations, The idea behind Annotations, Compile time processing using annotation processor, Repeating Annotations, Inherited Annotations, Getting Annotation values at run-time, Annotations for 'this' and receiver parameters, Add multiple annotation values"
---

# Annotations


In Java, an [annotation](https://en.wikipedia.org/wiki/Java_annotation) is a form of syntactic metadata that can be added to Java source code. [It provides data](https://docs.oracle.com/javase/tutorial/java/annotations/) about a program that is not part of the program itself. Annotations have no direct effect on the operation of the code they annotate. Classes, methods, variables, parameters and packages are allowed to be annotated.



## Defining annotation types


Annotation types are defined with `@interface`. Parameters are defined similar to methods of a regular interface.

```java
@interface MyAnnotation {
    String param1();
    boolean param2();
    int[] param3();  // array parameter 
}

```

### Default values

```java
@interface MyAnnotation {
    String param1() default "someValue";
    boolean param2() default true;
    int[] param3() default {};
}

```

### Meta-Annotations

Meta-annotations are annotations that can be applied to annotation types. Special predefined meta-annotation define how annotation types can be used.

### @Target

The `@Target` meta-annotation restricts the types the annotation can be applied to.

```java
@Target(ElementType.METHOD)
@interface MyAnnotation {
    // this annotation can only be applied to methods
}

```

Multiple values can be added using array notation, e.g. `@Target({ElementType.FIELD, ElementType.TYPE})`

### Available Values

|ElementType|target|example usage on target element
|------
|ANNOTATION_TYPE|annotation types|<pre>`@Retention(RetentionPolicy.RUNTIME) <br>@interface MyAnnotation`</pre>
|CONSTRUCTOR|constructors|<pre>`@MyAnnotation<br>public MyClass() {}`</pre>
|FIELD|fields, enum constants|<pre>`@XmlAttribute<br>private int count;`</pre>
|LOCAL_VARIABLE|variable declarations inside methods|<pre>`for (@LoopVariable int i = 0; i < 100; i++) {<br>    @Unused<br>    String resultVariable;<br>}`</pre>
|PACKAGE|package (in `package-info.java`)|<pre>`@Deprecated<br>package very.old;`</pre>
|METHOD|methods|<pre>`@XmlElement<br>public int getCount() {...}`</pre>
|PARAMETER|method/constructor parameters|<pre>`public Rectangle(<br>       @NamedArg("width") double width,<br>       @NamedArg("height") double height) {<br>    ...<br>}`</pre>
|TYPE|classes, interfaces, enums|<pre>`@XmlRootElement<br>public class Report {}`</pre>

|ElementType|target|example usage on target element
|------
|TYPE_PARAMETER|Type parameter declarations|<pre>`public <@MyAnnotation T> void f(T t) {}`</pre>
|TYPE_USE|Use of a type|<pre>`Object o = "42";<br>String s = (@MyAnnotation String) o;`</pre>

### @Retention

The `@Retention` meta-annotation defines the annotation visibility during the applications compilation process or execution. By default, annotations are included in `.class` files, but are not visible at runtime. To make an annotation accessible at runtime, `RetentionPolicy.RUNTIME` has to be set on that annotation.

```java
@Retention(RetentionPolicy.RUNTIME)
@interface MyAnnotation {
    // this annotation can be accessed with reflections at runtime
}

```

### Available values

|RetentionPolicy|Effect
|------
|CLASS|The annotation is available in the `.class` file, but not at runtime
|RUNTIME|The annotation is available at runtime and can be accessed via reflection
|SOURCE|The annotation is available at compile time, but not added to the `.class` files. The annotation can be used e.g. by an annotation processor.

### @Documented

The `@Documented` meta-annotation is used to mark annotations whose usage should be documented by API documentation generators like [javadoc](http://www.oracle.com/technetwork/java/javase/documentation/index-jsp-135444.html). It has no values. With `@Documented`, all classes that use the annotation will list it on their generated documentation page. Without `@Documented`, it's not possible to see which classes use the annotation in the documentation.

### @Inherited

The `@Inherited` meta-annotation is relevant to annotations that are applied to classes.  It has no values.  Marking an annotation as `@Inherited` alters the way that annotation querying works.

- For a non-inherited annotation, the query only examines the class being examined.
- For an inherited annotation, the query will also check the super-class chain (recursively) until an instance of the annotation is found.

Note that only the super-classes are queried: any annotations attached to interfaces in the classes hierarchy will be ignored.

### @Repeatable

The `@Repeatable` meta-annotation was added in Java 8.  It indicates that multiple instances of the annotation can be attached to the annotation's target.  This meta-annotation has no values.



## Runtime annotation checks via reflection


Java's Reflection API allows the programmer to perform various checks and operations on class fields, methods and annotations during runtime. However, in order for an annotation to be at all visible at runtime, the `RetentionPolicy` must be changed to `RUNTIME`, as demonstrated in the example below:

```java
@interface MyDefaultAnnotation {

}

@Retention(RetentionPolicy.RUNTIME)
@interface MyRuntimeVisibleAnnotation {

}

public class AnnotationAtRuntimeTest {

    @MyDefaultAnnotation
    static class RuntimeCheck1 {
    }
    
    @MyRuntimeVisibleAnnotation
    static class RuntimeCheck2 {
    }
    
    public static void main(String[] args) {
        Annotation[] annotationsByType = RuntimeCheck1.class.getAnnotations();
        Annotation[] annotationsByType2 = RuntimeCheck2.class.getAnnotations();

        System.out.println("default retention: " + Arrays.toString(annotationsByType));
        System.out.println("runtime retention: " + Arrays.toString(annotationsByType2));
    }
}

```



## Built-in annotations


The Standard Edition of Java comes with some annotations predefined. You do not need to define them by yourself and you can use them immediately. They allow the compiler to enable some fundamental checking of methods, classes and code.

**@Override**

This annotation applies to a method and says that this method must override a superclass' method or implement an abstract superclass' method definition. If this annotation is used with any other kind of method, the compiler will throw an error.

Concrete superclass

```java
public class Vehicle {
   public void drive() {
        System.out.println("I am driving");
   }
}

class Car extends Vehicle {
    // Fine
    @Override
    public void drive() {
        System.out.prinln("Brrrm, brrm");
    }
}

```

Abstract class

```java
abstract class Animal  {
   public abstract void makeNoise(); 
}

class Dog extends Animal {
    // Fine
    @Override
    public void makeNoise() {
        System.out.prinln("Woof");
    }
}

```

Does not work

```java
class Logger1 {
    public void log(String logString) {
        System.out.prinln(logString);
    }
}

class Logger2 {
    // This will throw compile-time error. Logger2 is not a subclass of Logger1. 
    // log method is not overriding anything
    @Override
    public void log(String logString) {
        System.out.println("Log 2" + logString);
    }
}

```

The main purpose is to catch mistyping, where you think you are overriding a method, but are actually defining a new one.

```java
class Vehicle {
   public void drive() {
        System.out.println("I am driving");
   }
}

class Car extends Vehicle {
    // Compiler error. "dirve" is not the correct method name to override.
    @Override
    public void dirve() {
        System.out.prinln("Brrrm, brrm");
    }
}

```

Note that the meaning of `@Override` has changed over time:

- In Java 5, it meant that the annotated method had to override a non-abstract method declared in the superclass chain.
- From Java 6 onward, it is **also** satisfied if the annotated method implements an abstract method declared in the classes superclass / interface hierarchy.

(This can occasionally cause problems when back-porting code to Java 5.)

**@Deprecated**

This marks the method as deprecated. There can be several reasons for this:

<li>
the API is flawed and is impractical to fix,
</li>
<li>
usage of the API is likely to lead to errors,
</li>
<li>
the API has been superseded by another API,
</li>
<li>
the API is obsolete,
</li>
<li>
the API is experimental and is subject to incompatible changes,
</li>
<li>
or any combination of the above.
</li>

The specific reason for deprecation can usually be found in the documentation of the API.

The annotation will cause the compiler to emit an error if you use it. IDEs may also highlight this method somehow as deprecated

```java
class ComplexAlgorithm {
    @Deprecated
    public void oldSlowUnthreadSafeMethod() {
        // stuff here
    }
    
    public void quickThreadSafeMethod() {
        // client code should use this instead
    }
}

```

**@SuppressWarnings**

In almost all cases, when the compiler emits a warning, the most appropriate action is to fix the cause. In some instances (Generics code using untype-safe pre-generics code, for example) this may not
be possible and it's better to suppress those warnings that you expect and cannot fix, so you can more clearly see unexpected warnings.

This annotation can be applied to a whole class, method or line. It takes the category of warning as a parameter.

```java
@SuppressWarnings("deprecation")
public class RiddledWithWarnings {
    // several methods calling deprecated code here
}

@SuppressWarning("finally")
public boolean checkData() {
    // method calling return from within finally block
}

```

It is better to limit the scope of the annotation as much as possible, to prevent unexpected warnings also being suppressed. For example, confining the scope of the annotation to a single-line:

```java
ComplexAlgorithm algorithm = new ComplexAlgorithm();
@SuppressWarnings("deprecation") algoritm.slowUnthreadSafeMethod(); 
// we marked this method deprecated in an example above

@SuppressWarnings("unsafe") List<Integer> list = getUntypeSafeList(); 
// old library returns, non-generic List containing only integers

```

The warnings supported by this annotation may vary from compiler to compiler. Only the `unchecked` and `deprecation` warnings are specifically mentioned in the JLS. Unrecognized warning types will be ignored.

**@SafeVarargs**

Because of type erasure, `void method(T... t)` will be converted to `void method(Object[] t)` meaning that the compiler is not always able to verify that the use of varargs is type-safe. For instance:

```java
private static <T> void generatesVarargsWarning(T... lists) {

```

There are instances where the use is safe, in which case you can annotate the method with the `SafeVarargs` annotation to suppress the warning. This obviously hides the warning if your use is unsafe too.

**@FunctionalInterface**

This is an optional annotation used to mark a FunctionalInterface. It will cause the compiler to complain if it does not conform to the FunctionalInterface spec (has a single     abstract method)

```java
@FunctionalInterface
public interface ITrade {
  public boolean check(Trade t);
}

@FunctionalInterface
public interface Predicate<T> {
  boolean test(T t);
}

```



## The idea behind Annotations


The [Java Language Specification](https://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html#jls-9.7) describes Annotations as follows:

> 
An annotation is a marker which associates information with a program construct, but has no effect at run time.


Annotations may appear before types or declarations. It is possible for them to appear in a place where they could apply to both a type or a declaration.<br />
What exactly an annotation applies to is governed by the "meta-annotation" `@Target`. See ["Defining annotation types"](https://stackoverflow.com/documentation/java/157/annotations/2060/defining-annotation-types#t=201609121335564579697) for more information.

Annotations are used for a multitude of purposes. Frameworks like Spring and Spring-MVC make use of annotations to define where Dependencies should be injected or where requests should be routed.

Other frameworks use annotations for code-generation. Lombok and JPA are prime examples, that use annotations to generate Java (and SQL) code.

This topic aims to provide a comprehensive overview of:

<li>
How to define your own Annotations?
</li>
<li>
What Annotations does the Java Language provide?
</li>
<li>
How are Annotations used in practice?
</li>



## Compile time processing using annotation processor


This example demonstrates how to do compile time checking of an annotated element.

### The annotation

The `@Setter` annotation is a marker can be applied to methods. The annotation will be discarded during compilation not be available afterwards.

```java
package annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.METHOD)
public @interface Setter {
}

```

### The annotation processor

The `SetterProcessor` class is used by the compiler to process the annotations. It checks, if the methods annotated with the `@Setter` annotation are `public`, non-`static` methods with a name starting with `set` and having a uppercase letter as 4th letter. If one of these conditions isn't met, a error is written to the `Messager`. The compiler writes this to stderr, but other tools could use this information differently. E.g. the NetBeans IDE allows the user specify annotation processors that are used to display error messages in the editor.

```java
package annotation.processor;

import annotation.Setter;
import java.util.Set;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;

@SupportedAnnotationTypes({"annotation.Setter"})
@SupportedSourceVersion(SourceVersion.RELEASE_8)
public class SetterProcessor extends AbstractProcessor {

    private Messager messager;

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        // get elements annotated with the @Setter annotation
        Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(Setter.class);

        for (Element element : annotatedElements) {
            if (element.getKind() == ElementKind.METHOD) {
                // only handle methods as targets
                checkMethod((ExecutableElement) element);
            }
        }

        // don't claim annotations to allow other processors to process them
        return false;
    }

    private void checkMethod(ExecutableElement method) {
        // check for valid name
        String name = method.getSimpleName().toString();
        if (!name.startsWith("set")) {
            printError(method, "setter name must start with \"set\"");
        } else if (name.length() == 3) {
            printError(method, "the method name must contain more than just \"set\"");
        } else if (Character.isLowerCase(name.charAt(3))) {
            if (method.getParameters().size() != 1) {
                printError(method, "character following \"set\" must be upper case");
            }
        }

        // check, if setter is public
        if (!method.getModifiers().contains(Modifier.PUBLIC)) {
            printError(method, "setter must be public");
        }

        // check, if method is static
        if (method.getModifiers().contains(Modifier.STATIC)) {
            printError(method, "setter must not be static");
        }
    }

    private void printError(Element element, String message) {
        messager.printMessage(Diagnostic.Kind.ERROR, message, element);
    }

    @Override
    public void init(ProcessingEnvironment processingEnvironment) {
        super.init(processingEnvironment);

        // get messager for printing errors
        messager = processingEnvironment.getMessager();
    }

}

```

### Packaging

To be applied by the compiler, the annotation processor needs to be made available to the SPI (see [ServiceLoader](http://stackoverflow.com/documentation/java/5433/serviceloader#t=201608231754351162351)).

To do this a text file `META-INF/services/javax.annotation.processing.Processor` needs to be added to the jar file containing the annotation processor and the annotation in addition to the other files. The file needs to include the fully qualified name of the annotation processor, i.e. it should look like this

```java
annotation.processor.SetterProcessor

```

**We'll assume the jar file is called `AnnotationProcessor.jar` below.**

### Example annotated class

The following class is example class in the default package with the annotations being applied to the correct elements according to the retention policy. However only the annotation processor only considers the second method a valid annotation target.

```java
import annotation.Setter;

public class AnnotationProcessorTest {
    
    @Setter
    private void setValue(String value) {}

    @Setter
    public void setString(String value) {}
    
    @Setter
    public static void main(String[] args) {}
    
}

```

### Using the annotation processor with javac

If the annotation processor is discovered using the SPI, it is automatically used to process annotated elements. E.g. compiling the `AnnotationProcessorTest` class using

```java
javac -cp AnnotationProcessor.jar AnnotationProcessorTest.java

```

yields the following output

```java
AnnotationProcessorTest.java:6: error: setter must be public
    private void setValue(String value) {}
                 ^
AnnotationProcessorTest.java:12: error: setter name must start with "set"
    public static void main(String[] args) {}
                       ^
2 errors

```

instead of compiling normally. No `.class` file is created.

This could be prevented by specifying the `-proc:none` option for `javac`. You could also forgo the usual compilation by specifying `-proc:only` instead.

### IDE integration

### Netbeans

Annotation processors can be used in the NetBeans editor. To do this the annotation processor needs to be specified in the project settings:

<li>
go to `Project Properties` > `Build` > `Compiling`
</li>
<li>
add check marks for `Enable Annotation Processing` and `Enable Annotation Processing in Editor`
</li>
<li>
click `Add` next to the annotation processor list
</li>
<li>
in the popup that appears enter the fully qualified class name of the annotation processor and click `Ok`.
</li>

### Result

[<img src="https://i.stack.imgur.com/fO8Xv.png" alt="Editor window with custom error message" />](https://i.stack.imgur.com/fO8Xv.png)



## Repeating Annotations


Until Java 8, two instances of the same annotation could not be applied to a single element. The standard workaround was to use a container annotation holding an array of some other annotation:

```java
// Author.java
@Retention(RetentionPolicy.RUNTIME)
public @interface Author {
    String value();
}

// Authors.java
@Retention(RetentionPolicy.RUNTIME)
public @interface Authors {
    Author[] value();
}

// Test.java
@Authors({
    @Author("Mary"),
    @Author("Sam")
})
public class Test {
    public static void main(String[] args) {
        Author[] authors = Test.class.getAnnotation(Authors.class).value();
        for (Author author : authors) {
            System.out.println(author.value());
            // Output:
            // Mary
            // Sam
        }
    }
}

```

Java 8 provides a cleaner, more transparent way of using container annotations, using the `@Repeatable` annotation. First we add this to the `Author` class:

```java
@Repeatable(Authors.class)

```

This tells Java to treat multiple `@Author` annotations as though they were surrounded by the `@Authors` container. We can also use `Class.getAnnotationsByType()` to access the `@Author` array by its own class, instead of through its container:

```java
@Author("Mary")
@Author("Sam")
public class Test {
    public static void main(String[] args) {
        Author[] authors = Test.class.getAnnotationsByType(Author.class);
        for (Author author : authors) {
            System.out.println(author.value());
            // Output:
            // Mary
            // Sam
        }
    }
}

```



## Inherited Annotations


By default class annotations do not apply to types extending them. This can be changed by adding the `@Inherited` annotation to the annotation definition

### Example

Consider the following 2 Annotations:

```java
@Inherited
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface InheritedAnnotationType {
}

```

and

```java
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface UninheritedAnnotationType {
}

```

If three classes are annotated like this:

```java
@UninheritedAnnotationType
class A {
}

@InheritedAnnotationType
class B extends A {
}

class C extends B {
}

```

running this code

```java
System.out.println(new A().getClass().getAnnotation(InheritedAnnotationType.class));
System.out.println(new B().getClass().getAnnotation(InheritedAnnotationType.class));
System.out.println(new C().getClass().getAnnotation(InheritedAnnotationType.class));
System.out.println("_________________________________");
System.out.println(new A().getClass().getAnnotation(UninheritedAnnotationType.class));
System.out.println(new B().getClass().getAnnotation(UninheritedAnnotationType.class));
System.out.println(new C().getClass().getAnnotation(UninheritedAnnotationType.class));

```

will print a result similar to this (depending on the packages of the annotation):

```java
null
@InheritedAnnotationType()
@InheritedAnnotationType()
_________________________________
@UninheritedAnnotationType()
null
null

```

Note that annotations can only be inherited from classes, not interfaces.



## Getting Annotation values at run-time


You can fetch the current properties of the Annotation by using [Reflection](http://stackoverflow.com/documentation/java/629/reflection#t=201607211900283806605) to fetch the Method or Field or Class which has an Annotation applied to it, and then fetching the desired properties.

```java
@Retention(RetentionPolicy.RUNTIME)
@interface MyAnnotation {
    String key() default "foo";
    String value() default "bar";
}


class AnnotationExample {
    // Put the Annotation on the method, but leave the defaults
    @MyAnnotation
    public void testDefaults() throws Exception {
        // Using reflection, get the public method "testDefaults", which is this method with no args
        Method method = AnnotationExample.class.getMethod("testDefaults", null);

        // Fetch the Annotation that is of type MyAnnotation from the Method
        MyAnnotation annotation = (MyAnnotation)method.getAnnotation(MyAnnotation.class);

        // Print out the settings of the Annotation
        print(annotation);
    }

    //Put the Annotation on the method, but override the settings
    @MyAnnotation(key="baz", value="buzz")
    public void testValues() throws Exception {
        // Using reflection, get the public method "testValues", which is this method with no args
        Method method = AnnotationExample.class.getMethod("testValues", null);

        // Fetch the Annotation that is of type MyAnnotation from the Method
        MyAnnotation annotation = (MyAnnotation)method.getAnnotation(MyAnnotation.class);

        // Print out the settings of the Annotation
        print(annotation);
    }

    public void print(MyAnnotation annotation) {
        // Fetch the MyAnnotation 'key' & 'value' properties, and print them out 
        System.out.println(annotation.key() + " = " + annotation.value());
    }

    public static void main(String[] args) {
        AnnotationExample example = new AnnotationExample();
        try {
            example.testDefaults();
            example.testValues();
        } catch( Exception e ) {
            // Shouldn't throw any Exceptions
            System.err.println("Exception [" + e.getClass().getName() + "] - " + e.getMessage());
            e.printStackTrace(System.err);
        }
    }
}

```

The output will be

```java
foo = bar
baz = buzz

```



## Annotations for 'this' and receiver parameters


When Java annotations were first introduced there was no provision for annotating the target of an instance method or the hidden constructor parameter for an inner classes constructor.  This was remedied in Java 8 with addition of **receiver parameter** declarations; see [JLS 8.4.1](http://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4.1-220).

> 
The receiver parameter is an optional syntactic device for an instance method or an inner class's constructor. For an instance method, the receiver parameter represents the object for which the method is invoked. For an inner class's constructor, the receiver parameter represents the immediately enclosing instance of the newly constructed object. Either way, the receiver parameter exists solely to allow the type of the represented object to be denoted in source code, so that the type may be annotated. The receiver parameter is not a formal parameter; more precisely, it is not a declaration of any kind of variable (§4.12.3), it is never bound to any value passed as an argument in a method invocation expression or qualified class instance creation expression, and it has no effect whatsoever at run time.


The following example illustrates the syntax for both kinds of receiver parameter:

```java
public class Outer {
    public class Inner {
        public Inner (Outer this) {
           // ...
        }
        public void doIt(Inner this) {
           // ...
        }
    }
}

```

The sole purpose of receiver parameters is to allow you to add annotations.  For example, you might have a custom annotation `@IsOpen` whose purpose is to assert that a `Closeable` object has not been closed when a method is called.  For example:

```java
public class MyResource extends Closeable {
    public void update(@IsOpen MyResource this, int value) {
        // ...
    }

    public void close() {
        // ...
    }
}

```

At one level, the `@IsOpen` annotation on `this` could simply serve as documentation.  However, we could potentially do more.  For example:

- An annotation processor could insert a runtime check that `this` is not in closed state when `update` is called.
- A code checker could perform a static code analysis to find cases where `this` **could be** closed when `update` is called.



## Add multiple annotation values


An Annotation parameter can accept multiple values if it is defined as an array. For example the standard annotation `@SuppressWarnings` is defined like this:

```java
public @interface SuppressWarnings {
    String[] value();
}

```

The `value` parameter is an array of Strings. You can set multiple values by using a notation similar to Array initializers:

```java
@SuppressWarnings({"unused"})
@SuppressWarnings({"unused", "javadoc"})

```

If you only need to set a single value, the brackets can be omitted:

```java
@SuppressWarnings("unused") 

```



#### Syntax


- @AnnotationName // 'Marker annotation' (no parameters)
- @AnnotationName(someValue) // sets parameter with the name 'value'
- @AnnotationName(param1 = value1) // named parameter
- @AnnotationName(param1 = value1, param2 = value2) // multiple named parameters
- @AnnotationName(param1 = {1, 2, 3}) // named array parameter
- @AnnotationName({value1}) // array with single element as parameter with the name 'value'



#### Remarks


### Parameter types

Only constant expressions of following types are allowed for parameters, as well as arrays of these types:

- `String`
- `Class`
- primitive types
- Enum types
- Annotation Types

