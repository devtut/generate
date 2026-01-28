---
metaTitle: "Java - Autoboxing"
description: "Using int and Integer interchangeably, Auto-unboxing may lead to NullPointerException, Using Boolean in if statement, Memory and Computational Overhead of Autoboxing, Different Cases When Integer and int can be used interchangeably"
---

# Autoboxing


[Autoboxing](http://docs.oracle.com/javase/7/docs/technotes/guides/language/autoboxing.html) is the automatic conversion that Java compiler makes between primitive types and their corresponding object wrapper classes. Example, converting int -> Integer, double -> Double... If the conversion goes the other way, this is called unboxing. Typically, this is used in Collections that cannot hold other than Objects, where boxing primitive types is needed before setting them in the collection.



## Using int and Integer interchangeably


As you use generic types with utility classes, you may often find that number types aren't very helpful when specified as the object types, as they aren't equal to their primitive counterparts.

```java
List<Integer> ints = new ArrayList<Integer>();

```

```java
List<Integer> ints = new ArrayList<>();

```

Fortunately, expressions that evaluate to `int` can be used in place of an `Integer` when it is needed.

```java
for (int i = 0; i < 10; i++)
    ints.add(i);

```

The `ints.add(i);` statement is equivalent to:

```java
ints.add(Integer.valueOf(i));

```

And retains properties from `Integer#valueOf` such as having the same `Integer` objects cached by the JVM when it is within the number caching range.

This also applies to:

- `byte` and `Byte`
- `short` and `Short`
- `float` and `Float`
- `double` and `Double`
- `long` and `Long`
- `char` and `Character`
- `boolean` and `Boolean`

Care must be taken, however, in ambiguous situations. Consider the following code:

```java
List<Integer> ints = new ArrayList<Integer>();
ints.add(1);
ints.add(2);
ints.add(3);
ints.remove(1); // ints is now [1, 3]

```

The `java.util.List` interface contains both a `remove(int index)` (`List` interface method) and a `remove(Object o)` (method inherited from `java.util.Collection`). In this case no boxing takes place and `remove(int index)` is called.

One more example of strange Java code behavior caused by autoboxing Integers with values in range from `-128` to `127`:

```java
Integer a = 127;
Integer b = 127;
Integer c = 128;
Integer d = 128;
System.out.println(a == b); // true
System.out.println(c <= d); // true
System.out.println(c >= d); // true
System.out.println(c == d); // false

```

This happens because `>=` operator implicitly calls `intValue()` which returns `int` while `==` compares **references**, not the `int` values.

By default, Java caches values in range `[-128, 127]`, so the operator `==` works because the `Integers` in this range reference to the same objects if their values are same. Maximal value of the cacheable range can be defined with `-XX:AutoBoxCacheMax` JVM option. So, if you run the program with `-XX:AutoBoxCacheMax=1000`, the following code will print `true`:

```java
Integer a = 1000;
Integer b = 1000;
System.out.println(a == b); // true

```



## Auto-unboxing may lead to NullPointerException


This code compiles:

```java
Integer arg = null;
int x = arg;

```

But it will crash at runtime with a `java.lang.NullPointerException` on the second line.

The problem is that a primitive `int` cannot have a `null` value.

This is a minimalistic example,
but in practice it often manifests in more sophisticated forms.
The `NullPointerException` is not very intuitive and is often little help in locating such bugs.

Rely on autoboxing and auto-unboxing with care,
make sure that unboxed values will not have `null` values at runtime.



## Using Boolean in if statement


Due to auto unboxing, one can use a `Boolean` in an `if` statement:

```java
Boolean a = Boolean.TRUE;
if (a) { // a gets converted to boolean
    System.out.println("It works!");
}

```

That works for `while`, `do while` and the condition in the `for` statements as well.

Note that, if the `Boolean` is `null`, a `NullPointerException` will be thrown in the conversion.



## Memory and Computational Overhead of Autoboxing


Autoboxing can come at a substantial memory overhead. For example:

```java
Map<Integer, Integer> square = new HashMap<Integer, Integer>();
for(int i = 256; i < 1024; i++) {
    square.put(i, i * i); // Autoboxing of large integers
}

```

will typically consume substantial amount of memory (about 60kb for 6k of actual data).

Furthermore, boxed integers usually require additional round-trips in the memory, and thus make CPU caches less effective. In above example, the memory accessed is spread out to five different locations that may be in entirely different regions of the memory: 1. the `HashMap` object, 2. the map's `Entry[] table` object, 3. the `Entry` object, 4. the entrys `key` object (boxing the primitive key), 5. the entrys `value` object (boxing the primitive value).

```java
class Example {
  int primitive; // Stored directly in the class `Example`
  Integer boxed; // Reference to another memory location
}

```

Reading `boxed` requires two memory accesses, accessing `primitive` only one.

When getting data from this map, the seemingly innocent code

```java
int sumOfSquares = 0;
for(int i = 256; i < 1024; i++) {
    sumOfSquares += square.get(i);
}

```

is equivalent to:

```java
int sumOfSquares = 0;
for(int i = 256; i < 1024; i++) {
    sumOfSquares += square.get(Integer.valueOf(i)).intValue();
}

```

Typically, the above code causes the **creation and garbage collection** of an `Integer` object for every `Map#get(Integer)` operation.  (See Note below for more details.)

To reduce this overhead, several libraries offer optimized collections for primitive types that do **not** require boxing. In addition to avoiding the boxing overhead, these collection will require about 4x less memory per entry. While Java Hotspot **may** be able to optimize the autoboxing by working with objects on the stack instead of the heap, it is not possible to optimize the memory overhead and resulting memory indirection.

Java 8 streams also have optimized interfaces for primitive data types, such as `IntStream` that do not require boxing.

Note: a typical Java runtime maintains a simple cache of `Integer` and other primitive wrapper object that is used by the `valueOf` factory methods, and by autoboxing.  For `Integer`, the default range of this cache is -128 to +127.  Some JVMs provide a JVM command-line option for changing the cache size / range.



## Different Cases When Integer and int can be used interchangeably


**Case 1:**
While using in the place of method arguments.

If a method requires an object of wrapper class as argument.Then interchangeably the  argument can be passed a variable of the respective primitive type and vice versa.

Example:

```java
int i;
Integer j;
void ex_method(Integer i)//Is a valid statement
void ex_method1(int j)//Is a valid statement

```

**Case 2:**
While passing return values:

When a method returns a primitive type variable then an object of corresponding wrapper class can be passed as  the return value interchangeably and vice versa.

Example:

```java
int i;
Integer j;
int ex_method()
{...
return j;}//Is a valid statement
Integer ex_method1()
{...
return i;//Is a valid statement
}

```

**Case 3:**
While performing operations.

Whenever performing operations on numbers the primitive type variable and object of respective wrapper class can be used interchangeably.

```java
int i=5;
Integer j=new Integer(7);
int k=i+j;//Is a valid statement
Integer m=i+j;//Is also a valid statement

```

**Pitfall**:Remember to initialize or assign a value to an object of the wrapper class.

While using wrapper class object and primitive variable interchangeably never forget or miss to initialize or assign a value to the wrapper class object else it may lead to null pointer exception at runtime.

Example:

```java
public class Test{
    Integer i;
    int j;
    public void met()
    {j=i;//Null pointer exception
    SOP(j);
    SOP(i);}   
    public static void main(String[] args)
    {Test t=new Test();
    t.go();//Null pointer exception
    }

```

In the above example, the value of the object is unassigned and uninitialized and thus at runtime the program will run into null pointer exception.So as clear from the above example the value of object should never be left uninitialized and unassigned.



#### Remarks


Autoboxing can have performance issues when used frequently in your code.

- [http://docs.oracle.com/javase/1.5.0/docs/guide/language/autoboxing.html](http://docs.oracle.com/javase/1.5.0/docs/guide/language/autoboxing.html)
- [Integer auto-unboxing and auto-boxing gives performance issues?](http://stackoverflow.com/questions/6037389/integer-auto-unboxing-and-auto-boxing-gives-performance-issues)

