---
metaTitle: "Java - Java Pitfalls - Nulls and NullPointerException"
description: "Pitfall - Making good unexpected nulls, Pitfall - Using null to represent an empty array or collection, Pitfall - Returning null instead of throwing an exception, Pitfall - Not checking if an I/O stream isn't even initialized when closing it, Pitfall - Unnecessary use of Primitive Wrappers can lead to NullPointerExceptions, Pitfall - Using Yoda notation to avoid NullPointerException"
---

# Java Pitfalls - Nulls and NullPointerException



## Pitfall - "Making good" unexpected nulls


On StackOverflow, we often see code like this in Answers:

```java
public String joinStrings(String a, String b) {
    if (a == null) {
        a = "";
    }
    if (b == null) {
        b = "";
    }
    return a + ": " + b;
}

```

Often, this is accompanied with an assertion that is "best practice" to test for `null` like this to avoid `NullPointerException`.

Is it best practice?  In short: No.

There are some underlying assumptions that need to be questioned before we can say if it is a good idea to do this in our `joinStrings`:

### What does it mean for "a" or "b" to be null?

A `String` value can be zero or more characters, so we already have a way of representing an empty string.  Does `null` mean something different to `""`?  If no, then it is problematic to have two ways to represent an empty string.

### Did the null come from an uninitialized variable?

A `null` can come from an uninitialized field, or an uninitialized array element.  The value could be uninitialized by design, or by accident.  If it was by accident then this is a bug.

### Does the null represent a "don't know" or "missing value"?

Sometimes a `null` can have a genuine meaning; e.g. that the real value of a variable is unknown or unavailable or "optional".  In Java 8, the `Optional` class provides a better way of expressing that.

### If this is a bug (or a design error) should we "make good"?

One interpretation of the code is that we are "making good" an unexpected `null` by using an empty string in its place.  Is the correct strategy?  Would it be better to let the `NullPointerException` happen, and then catch the exception further up the stack and log it as a bug?

The problem with "making good" is that it is liable to either hide the problem, or make it harder to diagnose.

### Is this efficient / good for code quality?

If the "make good" approach is used consistently, your code is going to contain a lot of "defensive" null tests.  This is going to make it longer and harder to read.  Furthermore, all of this testing and "making good" is liable to impact on the performance of your application.

### In summary

If `null` is a meaningful value, then testing for the `null` case is the correct approach.  The corollary is that if a `null` value is meaningful, then this should be clearly documented in the javadocs of any methods that accept the `null` value or return it.

Otherwise, it is a better idea to treat an unexpected `null` as a programming error, and let the `NullPointerException` happen so that the developer gets to know there is a problem in the code.



## Pitfall - Using null to represent an empty array or collection


Some programmers think that it is a good idea to save space by using a `null` to represent an empty array or collection.  While it is true that you can save a small amount of space, the flipside is that it makes your code more complicated, and more fragile.  Compare these two versions of a method for summing an array:

The first version is how you would normally code the method:

```java
/**
 * Sum the values in an array of integers.
 * @arg values the array to be summed
 * @return the sum
 **/
public int sum(int[] values) {
    int sum = 0;
    for (int value : values) {
        sum += value;
    }
    return sum;
}

```

The second version is how you need to code the method if you are in the habit of using `null` to represent an empty array.

```java
/**
 * Sum the values in an array of integers.
 * @arg values the array to be summed, or null.
 * @return the sum, or zero if the array is null.
 **/
public int sum(int[] values) {
    int sum = 0;
    if (values != null) {
        for (int value : values) {
            sum += value;
        }
    }
    return sum;
}

```

As you can see, the code is a bit more complicated.  This is directly attributable to the decision to use `null` in this way.

Now consider if this array that might be a `null` is used in lots of places.  At each place where you use it, you need to consider whether you need to test for `null`.  If you miss a `null` test that needs to be there, you risk a `NullPointerException`.  Hence, the strategy of using `null` in this way leads to your application being more fragile; i.e. more vulnerable to the consequences of programmer errors.

The lesson here is to use empty arrays and empty lists when that is what you mean.

```java
int[] values = new int[0];                     // always empty
List<Integer> list = new ArrayList();          // initially empty
List<Integer> list = Collections.emptyList();  // always empty

```

The space overhead is small, and there are other ways to minimize it if this this is a worthwhile thing to do.



## Pitfall - Returning null instead of throwing an exception


Some Java programmers have a general aversion to throwing or propagating exceptions.  This leads to code like the following:

```java
public Reader getReader(String pathname) {
    try {
        return new BufferedReader(FileReader(pathname));
    } catch (IOException ex) {
        System.out.println("Open failed: " + ex.getMessage());
        return null;
    }

```

}

So what is the problem with that?

The problem is that the `getReader` is returning a `null` as a special value to indicate that the `Reader` could not be opened.  Now the returned value needs to be tested to see if it is `null` before it is used.  If the test is left out, the result will be a `NullPointerException`.

There are actually three problems here:

1. The `IOException` was caught too soon.
1. The structure of this code means that there is a risk of leaking a resource.
1. A `null` was used then returned because no "real" `Reader` was available to return.

In fact, assuming that the exception did need to be caught early like this, there were a couple of alternatives to returning `null`:

1. It would be possible to implement a `NullReader` class; e.g. one where API's operations behaves as if the reader was already at the "end of file" position.
1. With Java 8, it would be possible to declare `getReader` as returning an `Optional<Reader>`.



## Pitfall - Not checking if an I/O stream isn't even initialized when closing it


To prevent memory leaks, one should not forget to close an input stream or an output stream whose job is done. This is usually done with a `try`-`catch`-`finally` statement without the `catch` part:

```java
void writeNullBytesToAFile(int count, String filename) throws IOException {
    FileOutputStream out = null;
    try {
        out = new FileOutputStream(filename);
        for(; count > 0; count--)
            out.write(0);
    } finally {
        out.close();
    }
}

```

While the above code might look innocent, it has a flaw that can make debugging impossible. If the line where `out` is initialized (`out = new FileOutputStream(filename)`) throws an exception, then `out` will be `null` when `out.close()` is executed, resulting in a nasty `NullPointerException`!

To prevent this, simply make sure the stream isn't `null` before trying to close it.

```java
void writeNullBytesToAFile(int count, String filename) throws IOException {
    FileOutputStream out = null;
    try {
        out = new FileOutputStream(filename);
        for(; count > 0; count--)
            out.write(0);
    } finally {
        if (out != null)
            out.close();
    }
}

```

An even better approach is to `try`-with-resources, since it'll automatically close the stream with a probability of 0 to throw an NPE without the need of a `finally` block.

```java
void writeNullBytesToAFile(int count, String filename) throws IOException {
    try (FileOutputStream out = new FileOutputStream(filename)) {
        for(; count > 0; count--)
            out.write(0);
    }
}

```



## Pitfall - Unnecessary use of Primitive Wrappers can lead to NullPointerExceptions


Sometimes, programmers who are new Java will use primitive types and wrappers interchangeably.  This can lead to problems.  Consider this example:

```java
public class MyRecord {
    public int a, b;
    public Integer c, d;
}

...
MyRecord record = new MyRecord();
record.a = 1;               // OK
record.b = record.b + 1;    // OK
record.c = 1;               // OK
record.d = record.d + 1;    // throws a NullPointerException

```

Our `MyRecord` class<sup>1</sup> relies on default initialization to initialize the values on its fields.  Thus, when we `new` a record, the `a` and `b` fields will be set to zero, and the `c` and `d` fields will be set to `null`.

When we try to use the default initialized fields, we see that the `int` fields works all of the time, but the `Integer` fields work in some cases and not others.  Specifically, in the case that fails (with `d`), what happens is that the expression on the right-hand side attempts to unbox a `null` reference, and that is what causes the `NullPointerException` to be thrown.

There are a couple of ways to look at this:

<li>
If the fields `c` and `d` need to be primitive wrappers, then either we should not be relying on default initialization, or we should be testing for `null`.  For former is the correct approach **unless** there is a definite meaning for the fields in the `null` state.
</li>
<li>
If the fields don't need to be primitive wrappers, then it is a mistake to make them primitive wrappers.  In addition to this problem, the primitive wrappers have extra overheads relative to primitive types.
</li>

The lesson here is to not use primitive wrapper types unless you really need to.

<sup>1 - This class is not an example of good coding practice.  For instance, a well-designed class would not have public fields.  However, that is not the point of this example.</sup>



## Pitfall - Using "Yoda notation" to avoid NullPointerException


A lot of example code posted on StackOverflow includes snippets like this:

```java
if ("A".equals(someString)) {
    // do something
}

```

This does "prevent" or "avoid" a possible `NullPointerException` in the case that `someString` is `null`.  Furthermore, it is arguable that

```

   "A".equals(someString)

```

is better than:

```

   someString != null && someString.equals("A")

```

(It is more concise, and in some circumstances it might be more efficient.  However, as we argue below, conciseness could be a negative.)

However, the real pitfall is using the Yoda test **to avoid `NullPointerExceptions`** as a matter of habit.

When you write `"A".equals(someString)` you are actually "making good" the case where `someString` happens to be `null`.  But as another example ([Pitfall - "Making good" unexpected nulls](http://stackoverflow.com/documentation/java/5680/java-pitfalls-nulls-and-nullpointerexception/20151/pitfall-making-good-unexpected-nulls) ) explains, "making good" `null` values can be harmful for a variety of reasons.

This means that Yoda conditions are not "best practice"<sup>1</sup>.  Unless the `null` is expected, it is better to let the `NullPointerException` happen so that you can get a unit test failure (or a bug report).  That allows you to find and fix the bug that caused the unexpected / unwanted `null` to appear.

Yoda conditions should only be used in cases where the `null` is **expected** because the object you are testing has come from an API that is **documented** as returning a `null`.  And arguably, it could be better to use one of the less pretty ways expressing the test because that helps to highlight the `null` test to someone who is reviewing your code.

<sup>1 - According to [Wikipedia](https://en.wikipedia.org/wiki/Best_coding_practices): **"Best coding practices are a set of informal rules that the software development community has learned over time which can help improve the quality of software."**.  Using Yoda notation does not achieve this.  In a lot of situations, it makes the code worse.</sup>



#### Remarks


The value `null` is the default value for an uninitialized value of a field whose type is a reference type.

`NullPointerException` (or NPE) is the exception that is thrown when you attempt to perform an inappropriate operation on the `null` object reference.  Such operations include:

- calling an instance method on a `null` target object,
- accessing a field of a `null` target object,
- attempting to index a `null` array object or access its length,
- using a `null` object reference as the mutex in a `synchronized` block,
- casting a `null` object reference,
- unboxing a `null` object reference, and
- throwing a `null` object reference.

The most common root causes for NPEs:

- forgetting to initialize a field with a reference type,
- forgetting to initialize elements of an array of a reference type, or
- not testing the results of certain API methods that are **specified** as returning `null` in certain circumstances.

Examples of commonly used methods that return `null` include:

- The `get(key)` method in the `Map` API will return a `null` if you call it with a key that doesn't have a mapping.
- The `getResource(path)` and `getResourceAsStream(path)` methods in the `ClassLoader` and `Class` APIs will return `null` if the resource cannot be found.
- The `get()` method in the `Reference` API will return `null` if the garbage collector has cleared the reference.
- Various `getXxxx` methods in the Java EE servlet APIs will return `null` if you attempt fetch a non-existent request parameter, session or session attribute and so on.

There are strategies for avoiding unwanted NPEs, such as explicitly testing for `null` or using "Yoda Notation", but these strategies often have the undesirable result of **hiding** problems in your code that really ought to be fixed.

