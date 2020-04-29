---
metaTitle: "Regular Expressions"
description: "Using capture groups, Using regex with custom behaviour by compiling the Pattern with flags, Escape Characters, Not matching a given string, Matching with a regex literal., Matching a backslash"
---

# Regular Expressions


A regular expression is a special sequence of characters that helps in matching or finding other strings or sets of strings, using a specialized syntax held in a pattern. Java has support for regular expression usage through the [`java.util.regex`](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) package. This topic is to introduce and help developers understand more with examples on how Regular Expressions must be used in Java.



## Using capture groups


If you need to extract a part of string from the input string, we can use **capture groups** of regex.

For this example, we'll start with a simple phone number regex:

```java
\d{3}-\d{3}-\d{4}

```

If parentheses are added to the regex, each set of parentheses is considered a **capturing group**. In this case, we are using what are called numbered capture groups:

```java
(\d{3})-(\d{3})-(\d{4})
^-----^ ^-----^ ^-----^
Group 1 Group 2 Group 3

```

Before we can use it in Java, we must not forget to follow the rules of Strings, escaping the backslashes, resulting in the following pattern:

```java
"(\\d{3})-(\\d{3})-(\\d{4})"

```

We first need to compile the regex pattern to make a `Pattern` and then we need a `Matcher` to match our input string with the pattern:

```java
Pattern phonePattern = Pattern.compile("(\\d{3})-(\\d{3})-(\\d{4})");
Matcher phoneMatcher = phonePattern.matcher("abcd800-555-1234wxyz");

```

Next, the Matcher needs to find the first subsequence that matches the regex:

```java
phoneMatcher.find();

```

Now, using the group method, we can extract the data from the string:

```java
String number = phoneMatcher.group(0); //"800-555-1234" (Group 0 is everything the regex matched)
String aCode = phoneMatcher.group(1); //"800"
String threeDigit = phoneMatcher.group(2); //"555"
String fourDigit = phoneMatcher.group(3); //"1234"

```

**Note:** `Matcher.group()` can be used in place of `Matcher.group(0)`.

Java 7 introduced named capture groups. Named capture groups function the same as numbered capture groups (but with a name instead of a number), although there are slight syntax changes. Using named capture groups improves readability.

We can alter the above code to use named groups:

```java
(?<AreaCode>\d{3})-(\d{3})-(\d{4})
^----------------^ ^-----^ ^-----^
AreaCode           Group 2 Group 3

```

To get the contents of "AreaCode", we can instead use:

```java
String aCode = phoneMatcher.group("AreaCode"); //"800"

```



## Using regex with custom behaviour by compiling the Pattern with flags


A `Pattern` can be compiled with flags, if the regex is used as a literal `String`, use inline modifiers:

```java
Pattern pattern = Pattern.compile("foo.", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
pattern.matcher("FOO\n").matches(); // Is true.

/* Had the regex not been compiled case insensitively and singlelined,
 * it would fail because FOO does not match /foo/ and \n (newline)
 * does not match /./.
 */

Pattern anotherPattern = Pattern.compile("(?si)foo");
anotherPattern.matcher("FOO\n").matches(); // Is true.

"foOt".replaceAll("(?si)foo", "ca"); // Returns "cat".

```



## Escape Characters


**Generally**

To use regular expression specific characters (`?+|` etc.) in their literal meaning they need to be escaped. In common regular expression this is done by a backslash `\`. However, as it has a special meaning in Java Strings, you have to use a double backslash `\\`.

These two examples will not work:

```java
"???".replaceAll ("?", "!"); //java.util.regex.PatternSyntaxException
"???".replaceAll ("\?", "!"); //Invalid escape sequence

```

This example works

```java
"???".replaceAll ("\\?", "!"); //"!!!"

```

**Splitting a Pipe Delimited String**

This does not return the expected result:

```java
"a|b".split ("|"); // [a, |, b]

```

This returns the expected result:

```java
"a|b".split ("\\|"); // [a, b]

```

**Escaping backslash `\`**

This will give an error:

```java
"\\".matches("\\"); // PatternSyntaxException
"\\".matches("\\\"); // Syntax Error

```

This works:

```java
"\\".matches("\\\\"); // true

```



## Not matching a given string


To match something that does **not** contain a given string, one can use negative lookahead:

Regex syntax: `(?!string-to-not-match)`

**Example:**

```java
//not matching "popcorn"
String regexString = "^(?!popcorn).*$";
System.out.println("[popcorn] " + ("popcorn".matches(regexString) ? "matched!" : "nope!"));
System.out.println("[unicorn] " + ("unicorn".matches(regexString) ? "matched!" : "nope!"));

```

**Output:**

```java
[popcorn] nope!
[unicorn] matched!

```



## Matching with a regex literal.


If you need to match characters that are a part of the regular expression syntax you can mark all or part of the pattern as a regex literal.

`\Q` marks the beginning of the regex literal.
`\E` marks the end of the regex literal.

```java
// the following throws a PatternSyntaxException because of the un-closed bracket
"[123".matches("[123");

// wrapping the bracket in \Q and \E allows the pattern to match as you would expect.
"[123".matches("\\Q[\\E123"); // returns true

```

An easier way of doing it without having to remember the `\Q` and `\E` escape sequences is to use `Pattern.quote()`

```java
"[123".matches(Pattern.quote("[") + "123"); // returns true

```



## Matching a backslash


If you want to match a backslash in your regular expression, you'll have to escape it.

Backslash is an escape character in regular expressions. You can use '\\' to refer to a single backslash in a regular expression.

However, backslash is **also** an escape character in Java literal strings. To make a regular expression from a string literal, you have to escape each of **its** backslashes. In a string literal '\\\\' can be used to create a regular expression with '\\', which in turn can match '\'.

For example, consider matching strings like "C:\dir\myfile.txt". A regular expression `([A-Za-z]):\\(.*)` will match, and provide the drive letter as a capturing group. Note the doubled backslash.

To express that pattern in a Java string literal, each of the backslashes in the regular expression needs to be escaped.

```

   String path = "C:\\dir\\myfile.txt";
    System.out.println( "Local path: " + path ); // "C:\dir\myfile.txt"
    
    String regex = "([A-Za-z]):\\\\.*"; // Four to match one
    System.out.println("Regex:      " + regex ); // "([A-Za-z]):\\(.*)"
    
    Pattern pattern = Pattern.compile( regex );
    Matcher matcher = pattern.matcher( path );
    if ( matcher.matches()) {
        System.out.println( "This path is on drive " + matcher.group( 1 ) + ":.");
        // This path is on drive C:.
    }

```

If you want to match **two** backslashes, you'll find yourself using eight in a literal string, to represent four in the regular expression, to match two.

```

   String path = "\\\\myhost\\share\\myfile.txt";
    System.out.println( "UNC path: " + path ); // \\myhost\share\myfile.txt"
    
    String regex = "\\\\\\\\(.+?)\\\\(.*)"; // Eight to match two
    System.out.println("Regex:    " + regex ); // \\\\(.+?)\\(.*) 
    
    Pattern pattern = Pattern.compile( regex );
    Matcher matcher = pattern.matcher( path );
    
    if ( matcher.matches()) {
        System.out.println( "This path is on host '" + matcher.group( 1 ) + "'.");
        // This path is on host 'myhost'.
    }

```



#### Syntax


- Pattern patternName = Pattern.compile(regex);
- Matcher matcherName = patternName.matcher(textToSearch);
- matcherName.matches() //Returns true if the textToSearch exactly matches the regex
- matcherName.find() //Searches through textToSearch for first instance of a substring matching the regex. Subsequent calls will search the remainder of the String.
- matcherName.group(groupNum) //Returns the substring inside of a capturing group
- matcherName.group(groupName) //Returns the substring inside of a named capturing group (Java 7+)



#### Remarks


### Imports

You will need to add the following imports before you can use Regex:

```java
import java.util.regex.Matcher
import java.util.regex.Pattern

```

### Pitfalls

In java, a backslash is escaped with a double backslash, so a backslash in the regex string should be inputted as a double backslash. If you need to escape a double backslash (to match a single backslash with the regex, you need to input it as a quadruple backslash.

### Important Symbols Explained

|Character|Description
|------
|`*`|Match the preceding character or subexpression 0 or more times
|`+`|Match the preceding character or subexpression 1 or more times
|`?`|Match the preceding character or subexpression 0 or 1 times

### Further reading

The [regex topic](http://stackoverflow.com/documentation/regex/topics) contains more information about regular expressions.

