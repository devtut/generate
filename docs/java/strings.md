---
metaTitle: "Strings"
description: "Comparing Strings, Changing the case of characters within a String, Finding a String Within Another String, Splitting Strings, String pool and heap storage, Joining Strings with a delimiter, String concatenation and StringBuilders, Substrings, Platform independent new line separator, Adding toString() method for custom objects, Reversing Strings, Remove Whitespace from the Beginning and End of a String, Replacing parts of Strings, Case insensitive switch, Getting the length of a String, Getting the nth character in a String, Counting occurrences of a substring or character in a string"
---

# Strings


Strings (`java.lang.String`) are pieces of text stored in your program.  Strings are **not** a [primitive data type in Java](http://stackoverflow.com/questions/10430582/primitive-data-types-in-java), however, they are very common in Java programs.

In Java, Strings are immutable, meaning that they cannot be changed. (Click [here](http://stackoverflow.com/questions/279507/what-is-meant-by-immutable) for a more thorough explanation of immutability.)



## Comparing Strings


In order to compare Strings for equality, you should use the String object's [`equals`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#equals-java.lang.Object-) or [`equalsIgnoreCase`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#equalsIgnoreCase-java.lang.String-) methods.

For example, the following snippet will determine if the two instances of [`String`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html) are equal on all characters:

```java
String firstString = "Test123";
String secondString = "Test" + 123;

if (firstString.equals(secondString)) {
   // Both Strings have the same content.
}

```

[Live demo](https://ideone.com/TjaYMR)

This example will compare them, independent of their case:

```java
String firstString = "Test123";
String secondString = "TEST123";

if (firstString.equalsIgnoreCase(secondString)) {
    // Both Strings are equal, ignoring the case of the individual characters.
}

```

[Live demo](https://ideone.com/XxKmM1)

**Note that** `equalsIgnoreCase` does not let you specify a `Locale`. For instance, if you compare the two words `"Taki"` and `"TAKI"` in English they are equal; however, in Turkish they are different (in Turkish, the lowercase `I` is `ı`). For cases like this, converting both strings to lowercase (or uppercase) with `Locale` and then comparing with `equals` is the solution.

```java
String firstString = "Taki";
String secondString = "TAKI";

System.out.println(firstString.equalsIgnoreCase(secondString)); //prints true

Locale locale = Locale.forLanguageTag("tr-TR");

System.out.println(firstString.toLowerCase(locale).equals(
                   secondString.toLowerCase(locale))); //prints false

```

[Live demo](https://ideone.com/uWc348)

### Do not use the == operator to compare Strings

Unless you can guarantee that all strings have been interned (see below), you **should not** use the `==` or `!=` operators to compare Strings.  These operators actually test references, and since multiple `String` objects can represent the same String, this is liable to give the wrong answer.

Instead, use the `String.equals(Object)` method, which will compare the String objects based on their values.  For a detailed explanation, please refer to [Pitfall: using == to compare strings](http://stackoverflow.com/documentation/java/4388/java-pitfalls/16290/pitfall-using-to-compare-strings).

### Comparing Strings in a switch statement

As of Java 1.7, it is possible to compare a String variable to literals in a `switch` statement. Make sure that the String is not null, otherwise it will always throw a [`NullPointerException`](http://stackoverflow.com/documentation/java/1003/nullpointerexception#t=201608020755368222531). Values are compared using `String.equals`, i.e. case sensitive.

```java
String stringToSwitch = "A";

switch (stringToSwitch) {
    case "a":
        System.out.println("a");
        break;
    case "A":
        System.out.println("A"); //the code goes here
        break;
    case "B":
        System.out.println("B");
        break;
    default:
        break;
}

```

[Live demo](https://ideone.com/fbWBUR)

### Comparing Strings with constant values

When comparing a `String` to a constant value, you can put the constant value on the left side of `equals` to ensure that you won't get a `NullPointerException` if the other String is `null`.

```java
"baz".equals(foo)

```

While `foo.equals("baz")` will throw a `NullPointerException` if `foo` is `null`,  `"baz".equals(foo)` will evaluate to `false`.

A more readable alternative is to use `Objects.equals()`, which does a null check on both parameters: `Objects.equals(foo, "baz")`.

(**Note:** It is debatable as to whether it is better to avoid `NullPointerExceptions` in general, or let them happen and then fix the root cause; see [here](http://stackoverflow.com/documentation/java/5680/java-pitfalls-nulls-and-nullpointerexception/20151/pitfall-making-good-unexpected-nulls) and [here](http://stackoverflow.com/documentation/java/5680/java-pitfalls-nulls-and-nullpointerexception/23490/pitfall-using-yoda-conditions-to-avoid-nullpointerexception).  Certainly, calling the avoidance strategy "best practice" is not justifiable.)

### String orderings

The `String` class implements `Comparable<String>` with the `String.compareTo` method (as described at the start of this example).  This makes the natural ordering of `String` objects case-sensitive order.  The `String` class provide a `Comparator<String>` constant called `CASE_INSENSITIVE_ORDER` suitable for case-insensitive sorting.

### Comparing with interned Strings

The Java Language Specification ([JLS 3.10.6](https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.10.5)) states the following:

> 
"Moreover, a string literal always refers to the same instance of class `String`. This is because string literals - or, more generally, strings that are the values of constant expressions - are **interned** so as to share unique instances, using the method `String.intern`."


This means it is safe to compare references to two string **literals** using `==`.  Moreover, the same is true for references to `String` objects that have been produced using the `String.intern()` method.

For example:

```java
String strObj = new String("Hello!");
String str = "Hello!";

// The two string references point two strings that are equal
if (strObj.equals(str)) {
    System.out.println("The strings are equal");
}

// The two string references do not point to the same object
if (strObj != str) {
    System.out.println("The strings are not the same object");
}

// If we intern a string that is equal to a given literal, the result is
// a string that has the same reference as the literal.
String internedStr = strObj.intern();

if (internedStr == str) {
    System.out.println("The interned string and the literal are the same object");
}

```

Behind the scenes, the interning mechanism maintains a hash table that contains all interned strings that are still **reachable**.  When you call `intern()` on a `String`, the method looks up the object in the hash table:

- If the string is found, then that value is returned as the interned string.
- Otherwise, a copy of the string is added to the hash table and that string is returned as the interned string.

It is possible to use interning to allow strings to be compared using `==`.  However, there are significant problems with doing this;
see [Pitfall - Interning strings so that you can use == is a bad idea](http://stackoverflow.com/documentation/java/5455/java-pitfalls-performance-issues/23991/pitfall-interning-strings-so-that-you-can-use-is-a-bad-idea#t=201610101338294726756) for details.   It is not recommended in most cases.



## Changing the case of characters within a String


The [`String`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html) type provides two methods for converting strings between upper case and lower case:

- [`toUpperCase`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--) to convert all characters to upper case
- [`toLowerCase`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--) to convert all characters to lower case

These methods both return the converted strings as new `String` instances: the original `String` objects are not modified because `String` is immutable in Java. See this for more on immutability : [Immutability of Strings in Java](http://stackoverflow.com/questions/1552301/immutability-of-strings-in-java)

```java
String string = "This is a Random String";
String upper = string.toUpperCase();
String lower = string.toLowerCase();

System.out.println(string);   // prints "This is a Random String"
System.out.println(lower);    // prints "this is a random string"
System.out.println(upper);    // prints "THIS IS A RANDOM STRING"

```

Non-alphabetic characters, such as digits and punctuation marks, are unaffected by these methods. Note that these methods may also incorrectly deal with certain Unicode characters under certain conditions.

**Note**: These methods are **locale-sensitive**, and may produce unexpected results if used on strings that are intended to be interpreted independent of the locale. Examples are programming language identifiers, protocol keys, and `HTML` tags.

For instance, `"TITLE".toLowerCase()` in a Turkish locale returns "`tıtle`", where `ı (\u0131)` is the [LATIN SMALL LETTER DOTLESS I](http://www.fileformat.info/info/unicode/char/0131/index.htm) character. To obtain correct results for locale insensitive strings, pass `Locale.ROOT` as a parameter to the corresponding case converting method (e.g. `toLowerCase(Locale.ROOT)` or `toUpperCase(Locale.ROOT)`).

Although using `Locale.ENGLISH` is also correct for most cases, the **language invariant** way is `Locale.ROOT`.

A detailed list of Unicode characters that require special casing can be found [on the Unicode Consortium website](http://unicode.org/Public/UNIDATA/SpecialCasing.txt).

**Changing case of a specific character within an ASCII string:**

To change the case of a specific character of an ASCII string following algorithm can be used:

Steps:

1. Declare a string.
1. Input the string.
1. Convert the string into a character array.
1. Input the character that is to be searched.
1. Search for the character into the character array.
<li>If found,check if the character is lowercase or uppercase.
<ul>
1. If Uppercase, add 32 to the ASCII code of the character.
1. If Lowercase, subtract 32 from the ASCII code of the character.
</ul>
</li>
1. Change the original character from the Character array.
1. Convert the character array back into the string.

Voila, the Case of the character is changed.

An example of the code for the algorithm is:

```java
Scanner scanner = new Scanner(System.in);
System.out.println("Enter the String");
String s = scanner.next();
char[] a = s.toCharArray();
System.out.println("Enter the character you are looking for");
System.out.println(s);
String c = scanner.next();
char d = c.charAt(0);

for (int i = 0; i <= s.length(); i++) {
    if (a[i] == d) {
        if (d >= 'a' && d <= 'z') {
            d -= 32;
        } else if (d >= 'A' && d <= 'Z') {
            d += 32;
        }
        a[i] = d;
        break;
    }
}
s = String.valueOf(a);
System.out.println(s);

```



## Finding a String Within Another String


To check whether a particular String `a` is being contained in a String `b` or not, we can use the method [`String.contains()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains-java.lang.CharSequence-) with the following syntax:

```java
b.contains(a); // Return true if a is contained in b, false otherwise

```

The [`String.contains()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#contains-java.lang.CharSequence-) method can be used to verify if a `CharSequence` can be found in the String. The method looks for the String `a` in the String `b` in a case-sensitive way.

```java
String str1 = "Hello World";
String str2 = "Hello";
String str3 = "helLO";

System.out.println(str1.contains(str2)); //prints true
System.out.println(str1.contains(str3)); //prints false

```

[Live Demo on Ideone](https://ideone.com/Tdef6b)

To find the exact position where a String starts within another String, use [`String.indexOf()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#indexOf-java.lang.String-):

```java
String s = "this is a long sentence";
int i = s.indexOf('i');    // the first 'i' in String is at index 2
int j = s.indexOf("long"); // the index of the first occurrence of "long" in s is 10
int k = s.indexOf('z');    // k is -1 because 'z' was not found in String s
int h = s.indexOf("LoNg"); // h is -1 because "LoNg" was not found in String s

```

[Live Demo on Ideone](https://ideone.com/RHHcF0)

The [`String.indexOf()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#indexOf-java.lang.String-) method returns the first index of a `char` or `String` in another `String`. The method returns `-1` if it is not found.

**Note**: The [`String.indexOf()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#indexOf-java.lang.String-) method is case sensitive.

Example of search ignoring the case:

```java
String str1 = "Hello World";
String str2 = "wOr";
str1.indexOf(str2);                               // -1
str1.toLowerCase().contains(str2.toLowerCase());  // true
str1.toLowerCase().indexOf(str2.toLowerCase());   // 6

```

[Live Demo on Ideone](https://ideone.com/TQtcMf)



## Splitting Strings


You can split a [`String`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html) on a particular delimiting character or a [Regular Expression](http://stackoverflow.com/documentation/regex/topics), you can use the [`String.split()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#split-java.lang.String-) method that has the following signature:

```java
public String[] split(String regex)

```

Note that delimiting character or regular expression gets removed from the resulting String Array.

Example using delimiting character:

```java
String lineFromCsvFile = "Mickey;Bolton;12345;121216";
String[] dataCells = lineFromCsvFile.split(";");
// Result is dataCells = { "Mickey", "Bolton", "12345", "121216"};

```

Example using regular expression:

```java
String lineFromInput = "What    do you need    from me?";
String[] words = lineFromInput.split("\\s+"); // one or more space chars
// Result is words = {"What", "do", "you", "need", "from", "me?"};

```

You can even directly split a `String` literal:

```java
String[] firstNames = "Mickey, Frank, Alicia, Tom".split(", ");
// Result is firstNames = {"Mickey", "Frank", "Alicia", "Tom"};

```

**Warning**: Do not forget that the parameter is always treated as a regular expression.

```java
"aaa.bbb".split("."); // This returns an empty array

```

In the previous example `.` is treated as the regular expression wildcard that matches any character, and since every character is a delimiter, the result is an empty array.

**Splitting based on a delimiter which is a regex meta-character**

The following characters are considered special (aka meta-characters) in regex

```

 < > - = ! ( ) [ ] { } \ ^ $ | ? * + . 

```

To split a string based on one of the above delimiters, you need to either **escape** them using `\\` or use `Pattern.quote()`:

<li>
Using `Pattern.quote()`:

```java
 String s = "a|b|c";
 String regex = Pattern.quote("|");
 String[] arr = s.split(regex);

```


</li>
<li>
Escaping the special characters:

```java
 String s = "a|b|c";
 String[] arr = s.split("\\|");

```


</li>

**Split removes empty values**

`split(delimiter)` by default removes trailing empty strings from result array. To turn this mechanism off we need to use overloaded version of `split(delimiter, limit)` with limit set to negative value like

```java
String[] split = data.split("\\|", -1);

```

`split(regex)` internally returns result of `split(regex, 0)`.

The limit parameter controls the number of times the pattern is applied and therefore affects the length of the resulting array.<br />
If the limit **n** is greater than zero then the pattern will be applied at most **n - 1** times, the array's length will be no greater than **n**, and the array's last entry will contain all input beyond the last matched delimiter.<br />
If **n** is negative, then the pattern will be applied as many times as possible and the array can have any length.<br />
If **n** is zero then the pattern will be applied as many times as possible, the array can have any length, and trailing empty strings will be discarded.

**Splitting with a `StringTokenizer`**

Besides the [`split()`](http://stackoverflow.com/documentation/regex/topics) method Strings can also be split using a `StringTokenizer`.

`StringTokenizer` is even more restrictive than `String.split()`, and also a bit harder to use. It is essentially designed for pulling out tokens delimited by a fixed set of characters (given as a `String`). Each character will act as a separator. Because of this restriction, it's about twice as fast as `String.split()`.

Default set of characters are empty spaces (`\t\n\r\f`). The following example will print out each word separately.

```java
String str = "the lazy fox jumped over the brown fence";
StringTokenizer tokenizer = new StringTokenizer(str);
while (tokenizer.hasMoreTokens()) {
    System.out.println(tokenizer.nextToken());
}

```

This will print out:

```java
the
lazy 
fox 
jumped 
over 
the 
brown 
fence

```

You can use different character sets for separation.

```java
String str = "jumped over";
// In this case character `u` and `e` will be used as delimiters 
StringTokenizer tokenizer = new StringTokenizer(str, "ue");
while (tokenizer.hasMoreTokens()) {
    System.out.println(tokenizer.nextToken());
}

```

This will print out:

```java
j
mp 
d ov
r

```



## String pool and heap storage


Like many Java objects, **all** `String` instances are created on the heap, even literals. When the JVM finds a `String` literal that has no equivalent reference in the heap, the JVM creates a corresponding `String` instance on the heap **and** it also stores a reference to the newly created `String` instance in the String pool. Any other references to the same `String` literal are replaced with the previously created `String` instance in the heap.

Let's look at the following example:

```java
class Strings
{
    public static void main (String[] args)
    {
        String a = "alpha";
        String b = "alpha";
        String c = new String("alpha");

        //All three strings are equivalent
        System.out.println(a.equals(b) && b.equals(c));

        //Although only a and b reference the same heap object
        System.out.println(a == b);
        System.out.println(a != c);
        System.out.println(b != c);
    }
}

```

The output of the above is:

```java
true
true
true
true

```

[<img src="https://i.stack.imgur.com/S8Ouk.png" alt="Diagram of Java Heap and String pool" />](https://i.stack.imgur.com/S8Ouk.png)
When we use double quotes to create a String, it first looks for String with same value in the String pool, if found it just returns the reference else it creates a new String in the pool and then returns the reference.

However using new operator, we force String class to create a new String object in heap space. We can use intern() method to put it into the pool or refer to other String object from string pool having same value.

The String pool itself is also created on the heap.

Before Java 7, `String` **literals** were stored in the runtime constant pool in the method area of `PermGen`, that had a fixed size.

The String pool also resided in `PermGen`.

[RFC: 6962931](http://www.oracle.com/technetwork/java/javase/jdk7-relnotes-418459.html)

> 
In JDK 7, interned strings are no longer allocated in the permanent generation of the Java heap, but are instead allocated in the main part of the Java heap (known as the young and old generations), along with the other objects created by the application. This change will result in more data residing in the main Java heap, and less data in the permanent generation, and thus may require heap sizes to be adjusted. Most applications will see only relatively small differences in heap usage due to this change, but larger applications that load many classes or make heavy use of the `String.intern()` method will see more significant differences.




## Joining Strings with a delimiter


An array of strings can be joined using the static method [`String.join()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#join-java.lang.CharSequence-java.lang.CharSequence...-):

```java
String[] elements = { "foo", "bar", "foobar" };
String singleString = String.join(" + ", elements);

System.out.println(singleString); // Prints "foo + bar + foobar"     

```

Similarly, there's an overloaded [`String.join()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#join-java.lang.CharSequence-java.lang.Iterable-) method for `Iterable`s.

To have a fine-grained control over joining, you may use [StringJoiner](https://docs.oracle.com/javase/8/docs/api/java/util/StringJoiner.html) class:

```java
StringJoiner sj = new StringJoiner(", ", "[", "]"); 
    // The last two arguments are optional, 
    // they define prefix and suffix for the result string

sj.add("foo");
sj.add("bar");
sj.add("foobar");

System.out.println(sj); // Prints "[foo, bar, foobar]"

```

To join a stream of strings, you may use the [joining collector](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#joining-java.lang.CharSequence-):

```java
Stream<String> stringStream = Stream.of("foo", "bar", "foobar");
String joined = stringStream.collect(Collectors.joining(", "));
System.out.println(joined); // Prints "foo, bar, foobar"

```

There's an option to define [prefix and suffix](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#joining-java.lang.CharSequence-java.lang.CharSequence-java.lang.CharSequence-) here as well:

```java
Stream<String> stringStream = Stream.of("foo", "bar", "foobar");
String joined = stringStream.collect(Collectors.joining(", ", "{", "}"));
System.out.println(joined); // Prints "{foo, bar, foobar}"

```



## String concatenation and StringBuilders


String concatenation can be performed using the `+` operator. For example:

```java
String s1 = "a";
String s2 = "b";
String s3 = "c";
String s = s1 + s2 + s3; // abc

```

Normally a compiler implementation will perform the above concatenation using methods involving a [`StringBuilder`](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html) under the hood. When compiled, the code would look similar to the below:

```java
StringBuilder sb = new StringBuilder("a");
String s = sb.append("b").append("c").toString();

```

`StringBuilder` has several overloaded methods for appending different types, for example, to append an `int` instead of a `String`. For example, an implementation can convert:

```java
String s1 = "a";
String s2 = "b";    
String s = s1 + s2 + 2; // ab2

```

to the following:

```java
StringBuilder sb = new StringBuilder("a");
String s = sb.append("b").append(2).toString();

```

The above examples illustrate a simple concatenation operation that is effectively done in a single place in the code. The concatenation involves a single instance of the `StringBuilder`. In some cases, a concatenation is carried out in a cumulative way such as in a loop:

```java
String result = "";
for(int i = 0; i < array.length; i++) {
    result += extractElement(array[i]);
}
return result;

```

In such cases, the compiler optimization is usually not applied, and each iteration will create a new `StringBuilder` object. This can be optimized by explicitly transforming the code to use a single `StringBuilder`:

```java
StringBuilder result = new StringBuilder();
for(int i = 0; i < array.length; i++) {
    result.append(extractElement(array[i]));
}
return result.toString();

```

A `StringBuilder` will be initialized with an empty space of only 16 characters. If you know in advance that you will be building larger strings, it can be beneficial to initialize it with sufficient size in advance, so that the internal buffer does not need to be resized:

```java
StringBuilder buf = new StringBuilder(30); // Default is 16 characters
buf.append("0123456789");
buf.append("0123456789"); // Would cause a reallocation of the internal buffer otherwise
String result = buf.toString(); // Produces a 20-chars copy of the string

```

If you are producing many strings, it is advisable to reuse `StringBuilder`s:

```java
StringBuilder buf = new StringBuilder(100);
for (int i = 0; i < 100; i++) {
    buf.setLength(0); // Empty buffer
    buf.append("This is line ").append(i).append('\n');
    outputfile.write(buf.toString());
}

```

If (and only if) multiple threads are writing to the **same** buffer, use [StringBuffer](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuffer.html), which is a `synchronized` version of `StringBuilder`. But because usually only a single thread writes to a buffer, it is usually faster to use `StringBuilder` without synchronization.

**Using concat() method:**

```java
String string1 = "Hello ";
String string2 = "world";
String string3 = string1.concat(string2);  // "Hello world"

```

This returns a new string that is string1 with string2 added to it at the end. You can also use the concat() method with string literals, as in:

```java
"My name is ".concat("Buyya");

```



## Substrings


```java
String s = "this is an example";
String a = s.substring(11); // a will hold the string starting at character 11 until the end ("example")
String b = s.substring(5, 10); // b will hold the string starting at character 5 and ending right before character 10 ("is an")
String b = s.substring(5, b.length()-3); // b will hold the string starting at character 5 ending right before b' s lenght is out of 3  ("is an exam")

```

Substrings may also be applied to slice and add/replace character into its original String. For instance, you faced a Chinese date containing Chinese characters but you want to store it as a well format Date String.

```java
String datestring = "2015年11月17日"
datestring = datestring.substring(0, 4) + "-" + datestring.substring(5,7) + "-" + datestring.substring(8,10);
//Result will be 2015-11-17

```

The [substring](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-) method extracts a piece of a `String`. When provided one parameter, the parameter is the start and the piece extends until the end of the `String`. When given two parameters, the first parameter is the starting character and the second parameter is the index of the character right after the end (the character at the index is not included). An easy way to check is the subtraction of the first parameter from the second should yield the expected length of the string.

In JDK <7u6 versions the `substring` method instantiates a `String` that shares the same backing `char[]` as the original `String` and has the internal `offset` and `count` fields set to the result start and length.
Such sharing may cause memory leaks, that can be prevented by calling `new String(s.substring(...))` to force creation of a copy, after which the `char[]` can be garbage collected.

From JDK 7u6 the `substring` method always copies the entire underlying `char[]` array, making the complexity linear compared to the previous constant one but guaranteeing the absence of memory leaks at the same time.



## Platform independent new line separator


Since the new line separator varies from platform to platform (e.g. `\n` on Unix-like systems or `\r\n` on Windows) it is often necessary to have a platform-independent way of accessing it. In Java it can be retrieved from a system property:

```java
System.getProperty("line.separator")

```

Because the new line separator is so commonly needed, from Java 7 on a shortcut method returning exactly the same result as the code above is available:

```java
System.lineSeparator()

```

**Note**: Since it is very unlikely that the new line separator changes during the program's execution, it is a good idea to store it in in a static final variable instead of retrieving it from the system property every time it is needed.

When using `String.format`, use `%n` rather than `\n` or '\r\n' to output a platform independent new line separator.

```java
System.out.println(String.format('line 1: %s.%nline 2: %s%n', lines[0],lines[1]));

```



## Adding toString() method for custom objects


Suppose you have defined the following `Person` class:

```java
public class Person {

    String name;
    int age;
    
    public Person (int age, String name) {
        this.age = age;
        this.name = name;
    }
}

```

If you instantiate a new `Person` object:

```java
Person person = new Person(25, "John");

```

and later in your code you use the following statement in order to print the object:

```java
System.out.println(person.toString());

```

[Live Demo on Ideone](https://ideone.com/tAl58G)

you'll get an output similar to the following:

```java
Person@7ab89d

```

This is the result of the implementation of the `toString()` method defined in the `Object` class, a superclass of `Person`. The documentation of `Object.toString()` states:

> 
The toString method for class Object returns a string consisting of the name of the class of which the object is an instance, the at-sign character `@', and the unsigned hexadecimal representation of the hash code of the object. In other words, this method returns a string equal to the value of:

```java
getClass().getName() + '@' + Integer.toHexString(hashCode())

```




So, for meaningful output, you'll have to **override** the `toString()` method:

```java
@Override
public String toString() {
    return "My name is " + this.name + " and my age is " + this.age;
}

```

Now the output will be:

```java
My name is John and my age is 25

```

You can also write

```java
System.out.println(person);

```

[Live Demo on Ideone](https://ideone.com/51al3w)

In fact, `println()` implicitly invokes the `toString` method on the object.



## Reversing Strings


There are a couple ways you can reverse a string to make it backwards.

<li>
StringBuilder/StringBuffer:

```java
 String code = "code";
 System.out.println(code);

 StringBuilder sb = new StringBuilder(code); 
 code = sb.reverse().toString();

 System.out.println(code);

```


</li>
<li>
Char array:

```java
String code = "code";
System.out.println(code);

char[] array = code.toCharArray();
for (int index = 0, mirroredIndex = array.length - 1; index < mirroredIndex; index++, mirroredIndex--) {
    char temp = array[index];
    array[index] = array[mirroredIndex];
    array[mirroredIndex] = temp;
}

// print reversed
System.out.println(new String(array));

```


</li>



## Remove Whitespace from the Beginning and End of a String


The [`trim()`](http://docs.oracle.com/javase/8/docs/api/java/lang/String.html#trim--) method returns a new String with the leading and trailing whitespace removed.

```java
String s = new String("   Hello World!!  ");
String t = s.trim();  // t = "Hello World!!"

```

If you `trim` a String that doesn't have any whitespace to remove, you will be returned the same String instance.

Note that the [`trim()`](http://docs.oracle.com/javase/8/docs/api/java/lang/String.html#trim--) method [has its own notion of whitespace](http://stackoverflow.com/q/1437933/2170192), which differs from the notion used by the [`Character.isWhitespace()`](http://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#isWhitespace-char-) method:

<li>
All ASCII control characters with codes `U+0000` to `U+0020` are considered whitespace and are removed by `trim()`. This includes `U+0020 'SPACE'`, `U+0009 'CHARACTER TABULATION'`, `U+000A 'LINE FEED'` and `U+000D 'CARRIAGE RETURN'` characters, but also the characters like `U+0007 'BELL'`.
</li>
<li>
Unicode whitespace like `U+00A0 'NO-BREAK SPACE'` or `U+2003 'EM SPACE'` are **not** recognized by `trim()`.
</li>



## Replacing parts of Strings


Two ways to replace: by regex or by exact match.

**Note:** the original String object will be unchanged, the return value holds the changed String.

### Exact match

### Replace single character with another single character:

```java
String replace(char oldChar, char newChar) 

```

> 
Returns a new string resulting from replacing all occurrences of oldChar in this string with newChar.


```java
String s = "popcorn";
System.out.println(s.replace('p','W'));

```

Result:

```java
WoWcorn

```

### Replace sequence of characters with another sequence of characters:

```java
String replace(CharSequence target, CharSequence replacement) 

```

> 
Replaces each substring of this string that matches the literal target sequence with the specified literal replacement sequence.


```java
String s = "metal petal et al.";
System.out.println(s.replace("etal","etallica"));

```

Result:

```java
metallica petallica et al.

```

### Regex

**Note**: the grouping uses the `$` character to reference the groups, like `$1`.

### Replace all matches:

```java
String replaceAll(String regex, String replacement) 

```

> 
Replaces each substring of this string that matches the given regular expression with the given replacement.


```java
String s = "spiral metal petal et al.";
System.out.println(s.replaceAll("(\\w*etal)","$1lica"));

```

Result:

```java
spiral metallica petallica et al.

```

### Replace first match only:

```java
String replaceFirst(String regex, String replacement) 

```

> 
Replaces the first substring of this string that matches the given regular expression with the given replacement


```java
String s = "spiral metal petal et al.";
System.out.println(s.replaceAll("(\\w*etal)","$1lica"));

```

Result:

```java
spiral metallica petal et al.

```



## Case insensitive switch


`switch` itself can not be parameterised to be case insensitive, but if absolutely required, can behave insensitive to the input string by using `toLowerCase()` or `toUpperCase`:

```java
switch (myString.toLowerCase()) {
     case "case1" :
        ...            
     break;
     case "case2" :
        ...            
     break;
}

```

**Beware**

- `Locale` might affect how [changing cases happen](http://stackoverflow.com/documentation/java/109/strings/515/changing-the-case-of-characters-within-a-string)!
- Care must be taken not to have any uppercase characters in the labels - those will never get executed!



## Getting the length of a String


In order to get the length of a `String` object, call the `length()` method on it.
The length is equal to the number of UTF-16 code units (chars) in the string.

```java
String str = "Hello, World!";
System.out.println(str.length()); // Prints out 13

```

[Live Demo on Ideone](https://ideone.com/0RWKcA)

A `char` in a String is UTF-16 value.  Unicode codepoints whose values are ≥ 0x1000 (for example, most emojis) use two char positions.  To count the number of Unicode codepoints in a String, regardless of whether each codepoint fits in a UTF-16 `char` value, you can use the `codePointCount` method:

```java
int length = str.codePointCount(0, str.length());

```

You can also use a Stream of codepoints, as of Java 8:

```java
int length = str.codePoints().count();

```



## Getting the nth character in a String


```java
String str = "My String";

System.out.println(str.charAt(0)); // "M"
System.out.println(str.charAt(1)); // "y"
System.out.println(str.charAt(2)); // " "
System.out.println(str.charAt(str.length-1)); // Last character "g"

```

To get the nth character in a string, simply call `charAt(n)` on a `String`, where `n` is the index of the character you would like to retrieve

**NOTE:** index `n` is starting at `0`, so the first element is at n=0.



## Counting occurrences of a substring or character in a string


`countMatches` method from [org.apache.commons.lang3.StringUtils](http://org.apache.commons.lang3.StringUtils) is typically used to count occurences of a substring or character in a `String`:

```java
import org.apache.commons.lang3.StringUtils;

String text = "One fish, two fish, red fish, blue fish";

// count occurrences of a substring
String stringTarget = "fish";
int stringOccurrences = StringUtils.countMatches(text, stringTarget); // 4

// count occurrences of a char
char charTarget = ',';
int charOccurrences = StringUtils.countMatches(text, charTarget); // 3

```

Otherwise for does the same with standard Java API's you could use Regular Expressions:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;
 
String text = "One fish, two fish, red fish, blue fish";
System.out.println(countStringInString("fish", text)); // prints 4
System.out.println(countStringInString(",", text)); // prints 3
 

public static int countStringInString(String search, String text) {
    Pattern pattern = Pattern.compile(search);
    Matcher matcher = pattern.matcher(text);
    
    int stringOccurrences = 0;
    while (matcher.find()) {
      stringOccurrences++;
    }
    return stringOccurrences;
}

```



#### Remarks


Since Java strings are [immutable](https://en.wikipedia.org/wiki/Immutable_object), all methods which manipulate a `String` will **return a new `String` object**.  They do not change the original `String`. This includes to substring and replacement methods that C and C++ programers would expect to mutate the target `String` object.

Use a [`StringBuilder`](https://docs.oracle.com/javase/8/docs/api/index.html?java/lang/StringBuilder.html) instead of `String` if you want to concatenate more than two `String` objects whose values cannot be determined at compile-time. This technique is more performant than creating new `String` objects and concatenating them because `StringBuilder` is mutable.

[`StringBuffer`](https://docs.oracle.com/javase/8/docs/api/index.html?java/lang/StringBuffer.html) can also be used to concatenate `String` objects.  However, this class is less performant because it is designed to be thread-safe, and acquires a mutex before each operation.  Since you almost never need thread-safety when concatenating strings, it is best to use `StringBuilder`.

If you can express a string concatenation as a single expression, then it is better to use the `+` operator.  The Java compiler will convert an expression containing `+` concatenations into an efficient sequence of operations using either `String.concat(...)` or `StringBuilder`.  The advice to use `StringBuilder` explicitly only applies when the concatenation involves a multiple expressions.

Don't store sensitive information in strings. If someone is able to obtain a memory dump of your running application, then they will be able to find all of the existing `String` objects and read their contents.  This includes `String` objects that are unreachable and are awaiting garbage collection.  If this is a concern, you will need to wipe sensitive string data as soon as you are done with it.  You cannot do this with `String` objects since they are immutable.  Therefore, it is advisable to use a `char[]` objects to hold sensitive character data, and wipe them (e.g. overwrite them with `'\000'` characters) when you are done.

**All** `String` instances are created on the heap, even instances that correspond to string literals. The special thing about string literals is that the JVM ensures that all literals that are equal (i.e. that consists of the same characters) are represented by a single `String` object (this behavior is specified in JLS).
This is implemented by JVM class loaders.  When a class loader loads a class, it scans for string literals that are used in the class definition, each time it sees one, it checks if there is already a record in the string pool for this literal (using the literal as a key). If there is already an entry for the literal, the reference to a `String` instance stored as the pair for that literal is used. Otherwise, a new  `String` instance is created and a reference to the instance is stored for the literal (used as a key) in the string pool. (Also see [string interning](https://en.wikipedia.org/wiki/String_interning)).

The string pool is held in the Java heap, and is subject to normal garbage collection.

In releases of Java before Java 7, the string pool was held in a special part of the heap known as "PermGen".  This part was only collected occasionally.

In Java 7, the string pool was moved off from "PermGen".

Note that string literals are implicitly reachable from any method that uses them.  This means that the corresponding `String` objects can only be garbage collected if the code itself is garbage collected.

Up until Java 8, `String` objects are implemented as a UTF-16 char array (2 bytes per char). There is a proposal in Java 9 to implement `String` as a byte array with an encoding flag field to note if the string is encoded as bytes (LATIN-1) or chars (UTF-16).

