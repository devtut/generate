---
metaTitle: "Java - Generics"
description: "Creating a Generic Class, Deciding between `T`, `? super T`, and `? extends T`, The Diamond, Declaring a Generic Method, Requiring multiple upper bounds (extends A & B), Benefits of Generic class and interface, Instantiating a generic type, Obtain class that satisfies generic parameter at runtime, Creating a Bounded Generic Class, Referring to the declared generic type within its own declaration, Binding generic parameter to more than 1 type, Using Generics to auto-cast, Use of instanceof with Generics, Different ways for implementing a Generic Interface (or extending a Generic Class)"
---

# Generics


[Generics](https://en.wikipedia.org/wiki/Generics_in_Java) are a facility of generic programming that extend Java's type system to allow a type or method to operate on objects of various types while providing compile-time type safety. In particular, the Java collections framework supports generics to specify the type of objects stored in a collection instance.



## Creating a Generic Class


[Generics](https://docs.oracle.com/javase/tutorial/java/generics/) enable classes, interfaces, and methods to take other classes and interfaces as type parameters.

This example uses generic class `Param` to take a single **type parameter** `T`, delimited by angle brackets (`<>`):

```java
public class Param<T> {
    private T value;

    public T getValue() {
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }
}

```

To instantiate this class, provide a **type argument** in place of `T`. For example, `Integer`:

```java
Param<Integer> integerParam = new Param<Integer>();

```

The type argument can be any reference type, including arrays and other generic types:

```java
Param<String[]> stringArrayParam;
Param<int[][]> int2dArrayParam;
Param<Param<Object>> objectNestedParam;

```

In Java SE 7 and later, the type argument can be replaced with an empty set of type arguments (`<>`) called the [**diamond**](http://stackoverflow.com/documentation/java/92/generics/457/diamond-operator#t=201607241742205098515):

```java
Param<Integer> integerParam = new Param<>();

```

Unlike other identifiers, type parameters have no naming constraints. However their names are commonly the first letter of their purpose in upper case. (This is true even throughout the official JavaDocs.)<br />
Examples include [`T` for "type"](https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html), [`E` for "element"](https://docs.oracle.com/javase/8/docs/api/java/util/List.html) and [`K`/`V` for "key"/"value"](https://docs.oracle.com/javase/8/docs/api/java/util/Map.html).

### Extending a generic class

```java
public abstract class AbstractParam<T> {
    private T value;

    public T getValue() {
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }
}

```

`AbstractParam` is an [abstract class](http://stackoverflow.com/documentation/java/87/inheritance/397/abstract-classes#t=201607241739430410595) declared with a type parameter of `T`. When extending this class, that type parameter can be replaced by a type argument written inside `<>`, or the type parameter can remain unchanged. In the first and second examples below, `String` and `Integer` replace the type parameter. In the third example, the type parameter remains unchanged. The fourth example doesn't use generics at all, so it's similar to if the class had an `Object` parameter. The compiler will warn about `AbstractParam` being a raw type, but it will compile the `ObjectParam` class. The fifth example has 2 type parameters (see "multiple type parameters" below), choosing the second parameter as the type parameter passed to the superclass.

```java
public class Email extends AbstractParam<String> {
    // ...
}

public class Age extends AbstractParam<Integer> {
    // ...
}

public class Height<T> extends AbstractParam<T> {
    // ...
}

public class ObjectParam extends AbstractParam {
    // ...
}

public class MultiParam<T, E> extends AbstractParam<E> {
    // ...
}

```

The following is the usage:

```java
Email email = new Email();
email.setValue("test@example.com");
String retrievedEmail = email.getValue();

Age age = new Age();
age.setValue(25);
Integer retrievedAge = age.getValue();
int autounboxedAge = age.getValue();

Height<Integer> heightInInt = new Height<>();
heightInInt.setValue(125);

Height<Float> heightInFloat = new Height<>();
heightInFloat.setValue(120.3f);

MultiParam<String, Double> multiParam = new MultiParam<>();
multiParam.setValue(3.3);

```

Notice that in the `Email` class, the `T getValue()` method acts as if it had a signature of `String getValue()`, and the `void setValue(T)` method acts as if it was declared `void setValue(String)`.

It is also possible to instantiate with anonymous inner class with an empty curly braces (`{}`):

```java
AbstractParam<Double> height = new AbstractParam<Double>(){};
height.setValue(198.6);

```

Note that [using the diamond with anonymous inner classes is not allowed.](http://stackoverflow.com/questions/22200647/why-cant-java-7-diamond-operator-be-used-with-anonymous-classes)

### Multiple type parameters

Java provides the ability to use more than one type parameter in a generic class or interface. Multiple type parameters can be used in a class or interface by placing a **comma-separated list** of types between the angle brackets. Example:

```java
public class MultiGenericParam<T, S> {
    private T firstParam;
    private S secondParam;
   
    public MultiGenericParam(T firstParam, S secondParam) {
        this.firstParam = firstParam;
        this.secondParam = secondParam;
    }
    
    public T getFirstParam() {
        return firstParam;
    }
    
    public void setFirstParam(T firstParam) {
        this.firstParam = firstParam;
    }
    
    public S getSecondParam() {
        return secondParam;
    }
    
    public void setSecondParam(S secondParam) {
        this.secondParam = secondParam;
    }
}

```

The usage can be done as below:

```java
MultiGenericParam<String, String> aParam = new MultiGenericParam<String, String>("value1", "value2");
MultiGenericParam<Integer, Double> dayOfWeekDegrees = new MultiGenericParam<Integer, Double>(1, 2.6);

```



## Deciding between `T`, `? super T`, and `? extends T`


The syntax for Java generics bounded wildcards, representing the unknown type by `?` is:

<li>
`? extends T` represents an upper bounded wildcard. The unknown type represents a type that must be a subtype of T, or type T itself.
</li>
<li>
`? super T` represents  a lower bounded wildcard. The unknown type represents a type that must be a supertype of T, or type T itself.
</li>

As a rule of thumb, you should use

- `? extends T` if you only need "read" access ("input")
- `? super T` if you need "write" access ("output")
- `T` if you need both ("modify")

Using `extends` or `super` is usually **better** because it makes your code more flexible (as in: allowing the use of subtypes and supertypes), as you will see below.

```java
class Shoe {}
class IPhone {}
interface Fruit {}
class Apple implements Fruit {}
class Banana implements Fruit {}
class GrannySmith extends Apple {}

   public class FruitHelper {

        public void eatAll(Collection<? extends Fruit> fruits) {}

        public void addApple(Collection<? super Apple> apples) {}
}

```

The compiler will now be able to detect certain bad usage:

```

public class GenericsTest {
      public static void main(String[] args){
  FruitHelper fruitHelper = new FruitHelper() ;
    List<Fruit> fruits = new ArrayList<Fruit>();
    fruits.add(new Apple()); // Allowed, as Apple is a Fruit
    fruits.add(new Banana()); // Allowed, as Banana is a Fruit
    fruitHelper.addApple(fruits); // Allowed, as "Fruit super Apple"
    fruitHelper.eatAll(fruits); // Allowed

    Collection<Banana> bananas = new ArrayList<>();
    bananas.add(new Banana()); // Allowed
    //fruitHelper.addApple(bananas); // Compile error: may only contain Bananas!
    fruitHelper.eatAll(bananas); // Allowed, as all Bananas are Fruits

    Collection<Apple> apples = new ArrayList<>();
    fruitHelper.addApple(apples); // Allowed
    apples.add(new GrannySmith()); // Allowed, as this is an Apple
    fruitHelper.eatAll(apples); // Allowed, as all Apples are Fruits.
    
    Collection<GrannySmith> grannySmithApples = new ArrayList<>();
    fruitHelper.addApple(grannySmithApples); //Compile error: Not allowed.
                                   // GrannySmith is not a supertype of Apple
    apples.add(new GrannySmith()); //Still allowed, GrannySmith is an Apple
    fruitHelper.eatAll(grannySmithApples);//Still allowed, GrannySmith is a Fruit

    Collection<Object> objects = new ArrayList<>();
    fruitHelper.addApple(objects); // Allowed, as Object super Apple
    objects.add(new Shoe()); // Not a fruit
    objects.add(new IPhone()); // Not a fruit
    //fruitHelper.eatAll(objects); // Compile error: may contain a Shoe, too!
}

```

Choosing the right `T`, `? super T` or `? extends T` is **necessary** to allow the use with subtypes. The compiler can then ensure type safety; you should not need to cast (which is not type safe, and may cause programming errors) if you use them properly.

If it is not easy to understand, please remember **PECS** rule:

> 
**P**roducer uses "**E**xtends" and **C**onsumer uses "**S**uper".


(Producer has only write access, and Consumer has only read access)



## The Diamond


Java 7 introduced the [**Diamond**](http://docs.oracle.com/javase/7/docs/technotes/guides/language/type-inference-generic-instance-creation.html)<sup>1</sup> to remove some boiler-plate around generic class instantiation. With Java 7+ you can write:

```java
List<String> list = new LinkedList<>();

```

Where you had to write in previous versions, this:

```java
List<String> list = new LinkedList<String>();

```

One limitation is for [Anonymous Classes](http://stackoverflow.com/documentation/java/3317/nested-and-inner-classes/18796/anonymous-inner-classes#t=201612181613574439584), where you still must provide the type parameter in the instantiation:

```java
// This will compile:

Comparator<String> caseInsensitiveComparator = new Comparator<String>() {
    @Override
    public int compare(String s1, String s2) {
        return s1.compareToIgnoreCase(s2);
    }
};

// But this will not:

Comparator<String> caseInsensitiveComparator = new Comparator<>() {
    @Override
    public int compare(String s1, String s2) {
        return s1.compareToIgnoreCase(s2);
    }
};

```

Although using the diamond with [Anonymous Inner Classes](http://stackoverflow.com/documentation/java/3317/nested-and-inner-classes/18796/anonymous-inner-classes#t=201612181613574439584) is not supported in Java 7 and 8, **[it will be included as a new feature in Java 9](https://bugs.openjdk.java.net/browse/JDK-8062373)**.

Footnote:

<sup>1 - Some people call the `<>` usage the "diamond **operator**".  This is incorrect.  The diamond does not behave as an operator, and is not described or listed anywhere in the JLS or the (official) Java Tutorials as an operator.  Indeed, `<>` is not even a distinct Java token.  Rather it is a `<` token followed by a `>` token, and it is legal (though bad style) to have whitespace or comments between the two. </sup>
<sup>The JLS and the Tutorials consistently refer to `<>` as "the diamond", and that is therefore the correct term for it.</sup>



## Declaring a Generic Method


Methods can also have [generic](https://docs.oracle.com/javase/tutorial/java/generics/) type parameters.

```java
public class Example {

    // The type parameter T is scoped to the method
    // and is independent of type parameters of other methods.
    public <T> List<T> makeList(T t1, T t2) {
        List<T> result = new ArrayList<T>();
        result.add(t1);
        result.add(t2);
        return result;
    }

    public void usage() {
        List<String> listString = makeList("Jeff", "Atwood");
        List<Integer> listInteger = makeList(1, 2);
    }
}

```

Notice that we don't have to pass an actual type argument to a generic method. The compiler infers the type argument for us, based on the target type (e.g. the variable we assign the result to), or on the types of the actual arguments. It will generally infer the most specific type argument that will make the call type-correct.

Sometimes, albeit rarely, it can be necessary to override this type inference with explicit type arguments:

```java
void usage() {
    consumeObjects(this.<Object>makeList("Jeff", "Atwood").stream());
}

void consumeObjects(Stream<Object> stream) { ... }

```

It's necessary in this example because the compiler can't "look ahead" to see that `Object` is desired for `T` after calling `stream()` and it would otherwise infer `String` based on the `makeList` arguments. Note that the Java language doesn't support omitting the class or object on which the method is called (`this` in the above example) when type arguments are explicitly provided.



## Requiring multiple upper bounds ("extends A & B")


You can require a generic type to extend multiple upper bounds.

Example: we want to sort a list of numbers but `Number` doesn't implement `Comparable`.

```java
public <T extends Number & Comparable<T>> void sortNumbers( List<T> n ) {
  Collections.sort( n );
}

```

In this example `T` must extend `Number` **and** implement `Comparable<T>` which should fit all "normal" built-in number implementations like `Integer` or `BigDecimal` but doesn't fit the more exotic ones like `Striped64`.

Since multiple inheritance is not allowed, you can use at most one class as a bound and it must be the first listed. For example, `<T extends Comparable<T> & Number>` is not allowed because Comparable is an interface, and not a class.



## Benefits of Generic class and interface


Code that uses generics has many benefits over non-generic code. Below are the main benefits

### Stronger type checks at compile time

A Java compiler applies strong type checking to generic code and issues errors if the code violates type safety. Fixing compile-time errors is easier than fixing runtime errors, which can be difficult to find.

### Elimination of casts

The following code snippet without generics requires casting:

```java
List list = new ArrayList();
list.add("hello");
String s = (String) list.get(0);

```

When re-written to **use generics**, the code does not require casting:

```java
List<String> list = new ArrayList<>();
list.add("hello");
String s = list.get(0);   // no cast

```

### Enabling programmers to implement generic algorithms

By using generics, programmers can implement generic algorithms that work on collections of different types, can be customized, and are type safe and easier to read.



## Instantiating a generic type


Due to type erasure the following will not work:

```java
public <T> void genericMethod() {
    T t = new T(); // Can not instantiate the type T.
}

```

The type `T` is erased. Since, at runtime, the JVM does not know what `T` originally was, it does not know which constructor to call.

### Workarounds

<li>
Passing `T`'s class when calling `genericMethod`:

```java
public <T> void genericMethod(Class<T> cls) {
    try {
        T t = cls.newInstance();
    } catch (InstantiationException | IllegalAccessException e) {
         System.err.println("Could not instantiate: " + cls.getName());
    }
}

```


 

```java
genericMethod(String.class);

```


Which throws exceptions, since there is no way to know if the passed class has an accessible default constructor.
</li>

<li>
Passing a [reference](http://stackoverflow.com/documentation/java/91/lambda-expressions/5080/method-references) to `T`'s constructor:

```java
public <T> void genericMethod(Supplier<T> cons) {
    T t = cons.get();
}

```




```java
genericMethod(String::new);

```


</li>



## Obtain class that satisfies generic parameter at runtime


Many unbound generic parameters, like those used in a static method, cannot be recovered at runtime (see **Other Threads** on **Erasure**). However there is a common strategy employed for accessing the type satisfying a generic parameter on a class at runtime. This allows for generic code that depends on access to type **without** having to thread type information through every call.

**Background**

Generic parameterization on a class can be inspected by creating an anonymous inner class. This class will capture the type information. In general this mechanism is referred to as **super type tokens**, which are detailed in [Neal Gafter's blog post](http://gafter.blogspot.com/2006/12/super-type-tokens.html).

**Implementations**

Three common implementations in Java are:

- [Guava's TypeToken](https://github.com/google/guava/wiki/ReflectionExplained)
- [Spring's ParameterizedTypeReference](http://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/core/ParameterizedTypeReference.html)
- [Jackson's TypeReference](http://fasterxml.github.io/jackson-core/javadoc/2.0.0/com/fasterxml/jackson/core/type/TypeReference.html)

**Example usage**

```java
public class DataService<MODEL_TYPE> {
     private final DataDao dataDao = new DataDao();
     private final Class<MODEL_TYPE> type = (Class<MODEL_TYPE>) new TypeToken<MODEL_TYPE>
                                                                (getClass()){}.getRawType();
     public List<MODEL_TYPE> getAll() {
         return dataDao.getAllOfType(type);
    }
}

// the subclass definitively binds the parameterization to User
// for all instances of this class, so that information can be 
// recovered at runtime
public class UserService extends DataService<User> {}

public class Main {
    public static void main(String[] args) {
          UserService service = new UserService();
          List<User> users = service.getAll();
    }
}

```



## Creating a Bounded Generic Class


You can restrict the valid types used in a **generic class** by bounding that type in the class definition. Given the following simple type hierarchy:

```java
public abstract class Animal {
    public abstract String getSound();
}

public class Cat extends Animal {
    public String getSound() {
        return "Meow";
    }
}

public class Dog extends Animal {
    public String getSound() {
        return "Woof";
    }
}

```

Without **bounded generics**, we cannot make a container class that is both generic and knows that each element is an animal:

```java
public class AnimalContainer<T> {

    private Collection<T> col;

    public AnimalContainer() {
        col = new ArrayList<T>();
    }

    public void add(T t) {
        col.add(t);
    }

    public void printAllSounds() {
        for (T t : col) {
            // Illegal, type T doesn't have makeSound()
            // it is used as an java.lang.Object here
            System.out.println(t.makeSound()); 
        }
    }
}

```

With generic bound in class definition, this is now possible.

```java
public class BoundedAnimalContainer<T extends Animal> { // Note bound here.

    private Collection<T> col;

    public BoundedAnimalContainer() {
        col = new ArrayList<T>();
    }

    public void add(T t) {
        col.add(t);
    }

    public void printAllSounds() {
        for (T t : col) {
            // Now works because T is extending Animal
            System.out.println(t.makeSound()); 
        }
    }
}

```

This also restricts the valid instantiations of the generic type:

```java
// Legal
AnimalContainer<Cat> a = new AnimalContainer<Cat>();

// Legal
AnimalContainer<String> a = new AnimalContainer<String>();

```

```java
// Legal because Cat extends Animal
BoundedAnimalContainer<Cat> b = new BoundedAnimalContainer<Cat>();

// Illegal because String doesn't extends Animal
BoundedAnimalContainer<String> b = new BoundedAnimalContainer<String>();

```



## Referring to the declared generic type within its own declaration


How do you go about using an instance of a (possibly further) inherited generic type within a method declaration in the generic type itself being declared? This is one of the problems you will face when you dig a bit deeper into generics, but still a fairly common one.

Assume we have a `DataSeries<T>` type (interface here), which defines a generic data series containing values of type `T`. It is cumbersome to work with this type directly when we want to perform a lot of operations with e.g. double values, so we define `DoubleSeries extends DataSeries<Double>`. Now assume, the original `DataSeries<T>` type has a method `add(values)` which adds another series of the same length and returns a new one. How do we enforce the type of `values` and the type of the return to be `DoubleSeries` rather than `DataSeries<Double>` in our derived class?

The problem can be solved by adding a generic type parameter referring back to and extending the type being declared (applied to an interface here, but the same stands for classes):

```java
public interface DataSeries<T, DS extends DataSeries<T, DS>> {
    DS add(DS values);
    List<T> data();
}

```

Here `T` represents the data type the series holds, e.g. `Double` and `DS` the series itself. An inherited type (or types) can now be easily implemented by substituting the above mentioned parameter by a corresponding derived type, thus, yielding a concrete `Double`-based definition of the form:

```java
public interface DoubleSeries extends DataSeries<Double, DoubleSeries> {
    static DoubleSeries instance(Collection<Double> data) {
        return new DoubleSeriesImpl(data);
    }
}

```

At this moment even an IDE will implement the above interface with correct types in place, which, after a bit of content filling may look like this:

```java
class DoubleSeriesImpl implements DoubleSeries {
    private final List<Double> data;

    DoubleSeriesImpl(Collection<Double> data) {
        this.data = new ArrayList<>(data);
    }

    @Override
    public DoubleSeries add(DoubleSeries values) {
        List<Double> incoming = values != null ? values.data() : null;
        if (incoming == null || incoming.size() != data.size()) {
            throw new IllegalArgumentException("bad series");
        }
        List<Double> newdata = new ArrayList<>(data.size());
        for (int i = 0; i < data.size(); i++) {
            newdata.add(this.data.get(i) + incoming.get(i)); // beware autoboxing
        }
        return DoubleSeries.instance(newdata);
    }

    @Override
    public List<Double> data() {
        return Collections.unmodifiableList(data);
    }
}

```

As you can see the `add` method is declared as `DoubleSeries add(DoubleSeries values)` and the compiler is happy.

The pattern can be further nested if required.



## Binding generic parameter to more than 1 type


Generic parameters can also be bound to more than one type using the `T extends Type1 & Type2 & ...` syntax.

Let's say you want to create a class whose Generic type should implement both `Flushable` and `Closeable`, you can write

```java
class ExampleClass<T extends Flushable & Closeable> {
}

```

Now, the `ExampleClass` only accepts as generic parameters, types which implement both `Flushable` **and** `Closeable`.

```java
ExampleClass<BufferedWriter> arg1; // Works because BufferedWriter implements both Flushable and Closeable

ExampleClass<Console> arg4; // Does NOT work because Console only implements Flushable
ExampleClass<ZipFile> arg5; // Does NOT work because ZipFile only implements Closeable

ExampleClass<Flushable> arg2; // Does NOT work because Closeable bound is not satisfied.
ExampleClass<Closeable> arg3; // Does NOT work because Flushable bound is not satisfied.

```

The class methods can choose to infer generic type arguments as either `Closeable` or `Flushable`.

```java
class ExampleClass<T extends Flushable & Closeable> {
    /* Assign it to a valid type as you want. */
    public void test (T param) {
        Flushable arg1 = param; // Works
        Closeable arg2 = param; // Works too.
    }

    /* You can even invoke the methods of any valid type directly. */
    public void test2 (T param) {
        param.flush(); // Method of Flushable called on T and works fine.
        param.close(); // Method of Closeable called on T and works fine too.
    }
}

```

### Note:

You cannot bind the generic parameter to either of the type using **OR** (`|`) clause. Only the **AND** (`&`) clause is supported.
Generic type can extends only one class and many interfaces. Class must be
placed at the beginning of the list.



## Using Generics to auto-cast


With generics, it's possible to return whatever the caller expects:

```java
private Map<String, Object> data;
public <T> T get(String key) {
    return (T) data.get(key);
}

```

The method will compile with a warning. The code is actually more safe than it looks because the Java runtime will do a cast when you use it:

```java
Bar bar = foo.get("bar");

```

It's less safe when you use generic types:

```java
List<Bar> bars = foo.get("bars");

```

Here, the cast will work when the returned type is any kind of `List` (i.e. returning `List<String>` would not trigger a `ClassCastException`; you'd eventually get it when taking elements out of the list).

To work around this problem, you can create an API which uses typed keys:

```java
public final static Key<List<Bar>> BARS = new Key<>("BARS");

```

along with this `put()` method:

```java
public <T> T put(Key<T> key, T value);

```

With this approach, you can't put the wrong type into the map, so the result will always be correct (unless you accidentally create two keys with the same name but different types).

Related:

- [Type-safe Map](https://blog.pdark.de/2010/05/28/type-safe-object-map/)



## Use of instanceof with Generics


**Using generics to define the type in instanceof**

Consider the following generic class `Example` declared with the formal parameter `<T>`:

```java
class Example<T> {
    public boolean isTypeAString(String s) {
        return s instanceof T; // Compilation error, cannot use T as class type here
    }
}

```

This will always give a Compilation error because as soon as the compiler compiles the **Java source** into **Java bytecode** it applies a process known as **type erasure**, which converts all generic code into non-generic code, making impossible to distinguish among T types at runtime. The type used with `instanceof` has to be **[reifiable](https://docs.oracle.com/javase/tutorial/java/generics/nonReifiableVarargsType.html)**, which means that all information about the type has to be available at runtime, and this is usually not the case for generic types.

The following class represents what two different classes of `Example`, `Example<String>` and `Example<Number>`, look like after generics has stripped off by **type erasure**:

```java
class Example { // formal parameter is gone
    public boolean isTypeAString(String s) {
        return s instanceof Object; // Both <String> and <Number> are now Object
    }
}

```

Since types are gone, it's not possible for the JVM to know which type is `T`.

**Exception to the previous rule**

You can always use **unbounded wildcard** (?) for specifying a type in the `instanceof` as follows:

```

   public boolean isAList(Object obj) {
        return obj instanceof List<?>;
    }

```

This can be useful to evaluate whether an instance `obj` is a `List` or not:

```java
System.out.println(isAList("foo")); // prints false
System.out.println(isAList(new ArrayList<String>()); // prints true
System.out.println(isAList(new ArrayList<Float>()); // prints true

```

In fact, unbounded wildcard is considered a reifiable type.

**Using a generic instance with instanceof**

The other side of the coin is that using an instance `t` of `T` with `instanceof` is legal, as shown in the following example:

```java
class Example<T> {
    public boolean isTypeAString(T t) {
        return t instanceof String; // No compilation error this time
    }
}

```

because after the type erasure the class will look like the following:

```java
class Example { // formal parameter is gone
    public boolean isTypeAString(Object t) {
        return t instanceof String; // No compilation error this time
    }
}

```

Since, even if the type erasure happen anyway, now the JVM can distinguish among different types in memory, even if they use the same reference type (`Object`), as the following snippet shows:

```java
Object obj1 = new String("foo"); // reference type Object, object type String
Object obj2 = new Integer(11); // reference type Object, object type Integer
System.out.println(obj1 instanceof String); // true
System.out.println(obj2 instanceof String); // false, it's an Integer, not a String

```



## Different ways for implementing a Generic Interface (or extending a Generic Class)


Suppose the following generic interface has been declared:

```java
public interface MyGenericInterface<T> {
    public void foo(T t);
}

```

Below are listed the possible ways to implement it.

**Non-generic class implementation with a specific type**

Choose a specific type to replace the formal type parameter `<T>` of `MyGenericClass` and implement it, as the following example does:

```java
public class NonGenericClass implements MyGenericInterface<String> {
    public void foo(String t) { } // type T has been replaced by String
}

```

This class only deals with `String`, and this means that using `MyGenericInterface` with different parameters (e.g. `Integer`, `Object` etc.) won't compile, as the following snippet shows:

```java
NonGenericClass myClass = new NonGenericClass();
myClass.foo("foo_string"); // OK, legal
myClass.foo(11); // NOT OK, does not compile
myClass.foo(new Object()); // NOT OK, does not compile

```

**Generic class implementation**

Declare another generic interface with the formal type parameter `<T>` which implements `MyGenericInterface`, as follows:

```java
public class MyGenericSubclass<T> implements MyGenericInterface<T> {
    public void foo(T t) { } // type T is still the same
    // other methods...
}

```

Note that a different formal type parameter may have been used, as follows:

```java
public class MyGenericSubclass<U> implements MyGenericInterface<U> { // equivalent to the previous declaration
    public void foo(U t) { }
    // other methods...
}

```

**Raw type class implementation**

Declare a non-generic class which implements `MyGenericInteface` as a **raw type** (not using generic at all), as follows:

```java
public class MyGenericSubclass implements MyGenericInterface {
    public void foo(Object t) { } // type T has been replaced by Object
    // other possible methods
}

```

This way is **not** recommended, since it is not 100% safe at runtime because it mixes up **raw type** (of the subclass) with **generics** (of the interface) and it is also confusing. Modern Java compilers will raise a warning with this kind of implementation, nevertheless the code - for compatibility reasons with older JVM (1.4 or earlier) - will compile.

All the ways listed above are also allowed when using a generic class as a supertype instead of a generic interface.



#### Syntax


- class ArrayList<E> {} // a generic class with type parameter E
- class HashMap<K, V> {} // a generic class with two type parameters K and V
- <E> void print(E element) {} // a generic method with type parameter E
- ArrayList<String> names; // declaration of a generic class
- ArrayList<?> objects; // declaration of a generic class with an unknown type parameter
- new ArrayList<String>() // instantiation of a generic class
- new ArrayList<>() // instantiation with type inference "diamond" (Java 7 or later)



#### Remarks


Generics are implemented in Java through Type erasure, which means that during runtime the Type information specified in the instantiation of a generic class is not available. For example, the statement `List<String> names = new ArrayList<>();` produces a list object from which the element type `String` cannot be recovered at runtime. However, if the list is stored in a field of type `List<String>`, or passed to a method/constructor parameter of this same type, or returned from a method of that return type, then the full type information **can** be recovered at runtime through the Java Reflection API.

This also means that when casting to a generic type (e.g.: `(List<String>) list`), the cast is an **unchecked cast**. Because the parameter `<String>` is erased, the JVM cannot check if a cast from a `List<?>` to a `List<String>` is correct; the JVM only sees a cast for `List` to `List` at runtime.

