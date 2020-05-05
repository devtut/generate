---
metaTitle: "Java - StringBuilder"
description: "Comparing StringBuffer, StringBuilder, Formatter and StringJoiner, Repeat a String n times"
---

# StringBuilder


Java StringBuilder class is used to create mutable (modifiable) string. The Java StringBuilder class is same as StringBuffer class except that it is non-synchronized. It is available since JDK 1.5.



## Comparing StringBuffer, StringBuilder, Formatter and StringJoiner


The `StringBuffer`, `StringBuilder`, `Formatter` and `StringJoiner` classes are Java SE utility classes that are primarily used for assembling strings from other information:

<li>
The `StringBuffer` class has been present since Java 1.0, and provides a variety of methods for building and modifying a "buffer" containing a sequence of characters.
</li>
<li>
The `StringBuilder` class was added in Java 5 to address performance issues with the original `StringBuffer` class.  The APIs for the two clases are essentially the same.  The main difference between `StringBuffer` and `StringBuilder` is that the former is thread-safe and synchronized and the latter is not.
</li>

This example shows how `StringBuilder` is can be used:

```java
int one = 1;
String color = "red";
StringBuilder sb = new StringBuilder();
sb.append("One=").append(one).append(", Color=").append(color).append('\n');
System.out.print(sb);
// Prints "One=1, Colour=red" followed by an ASCII newline.

```

(The `StringBuffer` class is used the same way: just change `StringBuilder` to `StringBuffer` in the above)

The `StringBuffer` and `StringBuilder` classes are suitable for both assembling and modifying strings; i.e they provide methods for replacing and removing characters as well as adding them in various.  The remining two classes are specific to the task of assembling strings.

<li>
The `Formatter` class was added in Java 5, and is loosely modeled on the `sprintf` function in the C standard library.  It takes a **format** string with embedded **format specifiers** and a sequences of other arguments, and generates a string by converting the arguments into text and substituting them in place of the format specifiers.  The details of the format specifiers say how the arguments are converted into text.
</li>
<li>
The `StringJoiner` class was added in Java 8.  It is a special purpose formatter that succinctly formats a sequence of strings with separators between them.  It is designed with a **fluent** API, and can be used with Java 8 streams.
</li>

Here are some typical examples of `Formatter` usage:

```java
// This does the same thing as the StringBuilder example above
int one = 1;
String color = "red";
Formatter f = new Formatter();
System.out.print(f.format("One=%d, colour=%s%n", one, color));
// Prints "One=1, Colour=red" followed by the platform's line separator

// The same thing using the `String.format` convenience method
System.out.print(String.format("One=%d, color=%s%n", one, color));

```

The `StringJoiner` class is not ideal for the above task, so here is an example of a formatting an array of strings.

```java
StringJoiner sj = new StringJoiner(", ", "[", "]");
for (String s : new String[]{"A", "B", "C"}) {
    sj.add(s);
}
System.out.println(sj);
// Prints "[A, B, C]"

```

The use-cases for the 4 classes can be summarized:

- `StringBuilder` suitable for any string assembly OR string modification task.
- `StringBuffer` use (only) when you require a thread-safe version of `StringBuilder`.
<li>`Formatter` provides much richer string formatting functionality, but is not as efficient as `StringBuilder`. This is because each call to `Formatter.format(...)` entails:
<ul>
- parsing the `format` string,
- creating and populate a **varargs** array, and
- autoboxing any primitive type arguments.



## Repeat a String n times


Problem: Create a `String` containing `n` repetitions of a `String s`.

The trivial approach would be repeatedly concatenating the `String`

```java
final int n = ...
final String s = ...
String result = "";

for (int i = 0; i < n; i++) {
    result += s;
}

```

This creates `n` new string instances containing 1 to `n` repetitions of `s` resulting in a runtime of `O(s.length() * n²) = O(s.length() * (1+2+...+(n-1)+n))`.

To avoid this `StringBuilder` should be used, which allows creating the `String` in `O(s.length() * n)` instead:

```java
final int n = ...
final String s = ...

StringBuilder builder = new StringBuilder();

for (int i = 0; i < n; i++) {
    builder.append(s);
}

String result = builder.toString();

```



#### Syntax


<li>
new StringBuilder ()
</li>
<li>
new StringBuilder (int capacity)
</li>
<li>
new StringBuilder (CharSequence seq)
</li>
<li>
new StringBuilder (StringBuilder builder)
</li>
<li>
new StringBuilder (String string)
</li>
<li>
new StringJoiner (CharSequence delimiter)
</li>
<li>
new StringJoiner (CharSequence delimiter, CharSequence prefix, CharSequence suffix)
</li>



#### Remarks


Creating a new `StringBuilder` with type `char` as a parameter would result in calling the constructor with argument `int capacity` and not the one with argument `String string`:

```

StringBuilder v = new StringBuilder('I'); //'I' is a character, "I" is a String.
 System.out.println(v.capacity()); --> output 73
 System.out.println(v.toString()); --> output nothing

```

