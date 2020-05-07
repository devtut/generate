---
metaTitle: "Kotlin - Regex"
description: "Idioms for Regex Matching in When Expression, Introduction to regular expressions in Kotlin"
---

# Regex




## Idioms for Regex Matching in When Expression


### Using immutable locals:

Uses less horizontal space but more vertical space than the "anonymous temporaries" template.  Preferable over the "anonymous temporaries" template if the `when` expression is in a loop--in that case, regex definitions should be placed outside the loop.

```kotlin
import kotlin.text.regex

var string = /* some string */

val regex1 = Regex( /* pattern */ )
val regex2 = Regex( /* pattern */ )
/* etc */

when {
    regex1.matches(string) -> /* do stuff */
    regex2.matches(string) -> /* do stuff */
    /* etc */
}

```

### Using anonymous temporaries:

Uses less vertical space but more horizontal space than the "immutable locals" template.  Should not be used if then `when` expression is in a loop.

```kotlin
import kotlin.text.regex

var string = /* some string */

when {  
    Regex( /* pattern */ ).matches(string) -> /* do stuff */
    Regex( /* pattern */ ).matches(string) -> /* do stuff */
    /* etc */
}

```

### Using the visitor pattern:

Has the benefit of closely emulating the "argument-ful" `when` syntax.  This is beneficial because it more clearly indicates the argument of the `when` expression, and also precludes certain programmer mistakes that could arise from having to repeat the `when` argument in every `whenEntry`.  Either the "immutable locals" or the "anonymous temporaries" template may be used with this implementation the visitor pattern.

```kotlin
import kotlin.text.regex

var string = /* some string */

when (RegexWhenArgument(string)) {
    Regex( /* pattern */ ) -> /* do stuff */
    Regex( /* pattern */ ) -> /* do stuff */
    /* etc */
}

```

And the minimal definition of the wrapper class for the `when` expression argument:

```kotlin
class RegexWhenArgument (val whenArgument: CharSequence) {
    operator fun equals(whenEntry: Regex) = whenEntry.matches(whenArgument)
    override operator fun equals(whenEntry: Any?) = (whenArgument == whenEntry)
}

```



## Introduction to regular expressions in Kotlin


This post shows how to use most of the functions in the `Regex` class, work with null safely related to the `Regex` functions, and how raw strings makes it easier to write and read regex patterns.

### The RegEx class

To work with regular expressions in Kotlin, you need to use the `Regex(pattern: String)` class and invoke functions like `find(..)` or `replace(..)` on that regex object.

An example on how to use the `Regex` class that returns true if the `input` string contains c or d:

```kotlin
val regex = Regex(pattern = "c|d")
val matched = regex.containsMatchIn(input = "abc")    // matched: true

```

The essential thing to understand with all the `Regex` functions is that the result is based on matching the regex `pattern` and the `input` string. Some of the functions requires a full match, while the rest requires only a partial match. The `containsMatchIn(..)` function used in the example requires a partial match and is explained later in this post.

### Null safety with regular expressions

Both `find(..)` and `matchEntire(..)` will return a `MatchResult?` object. The `?` character after `MatchResult` is necessary for Kotlin to handle [null safely](https://kotlinlang.org/docs/reference/null-safety.html).

An example that demonstrates how Kotlin handles null safely from a `Regex` function, when the `find(..)` function returns null:

```kotlin
val matchResult = 
    Regex("c|d").find("efg")           // matchResult: null
val a = matchResult?.value             // a: null
val b = matchResult?.value.orEmpty()   // b: ""
a?.toUpperCase()                       // Still needs question mark. => null    
b.toUpperCase()                        // Accesses the function directly. => ""

```

With the `orEmpty()` function, `b` can't be null and the `?` character is unnecessary when you call functions on `b`.

If you don't care about this safe handling of null values, Kotlin allows you to work with null values like in Java with the `!!` characters:

```kotlin
a!!.toUpperCase()                      // => KotlinNullPointerException

```

### Raw strings in regex patterns

Kotlin provides an improvement over Java with a [raw string](https://kotlinlang.org/docs/reference/basic-types.html#string-literals) that makes it possible to write pure regex patterns without double backslashes, that are necessary with a Java string. A raw string is represented with a triple quote:

```kotlin
"""\d{3}-\d{3}-\d{4}""" // raw Kotlin string
"\\d{3}-\\d{3}-\\d{4}"  // standard Java string

```

### find(input: CharSequence, startIndex: Int): MatchResult?

The `input` string will be matched against the `pattern` in the `Regex` object. It returns a `Matchresult?` object with the first matched text after the `startIndex`, or `null` if the pattern didn't match the `input` string. The result string is retrieved from the `MatchResult?` object's `value` property. The `startIndex` parameter is optional with the default value 0.

To extract the first valid phone number from a string with contact details:

```kotlin
val phoneNumber :String? = Regex(pattern = """\d{3}-\d{3}-\d{4}""")
    .find(input = "phone: 123-456-7890, e..")?.value // phoneNumber: 123-456-7890

```

With no valid phone number in the `input` string, the variable `phoneNumber` will be `null`.

### findAll(input: CharSequence, startIndex: Int): Sequence

Returns all the matches from the `input` string that matches the regex `pattern`.

To print out all numbers separated with space, from a text with letters and digits:

```kotlin
val matchedResults = Regex(pattern = """\d+""").findAll(input = "ab12cd34ef")
val result = StringBuilder()
for (matchedText in matchedResults) {
    result.append(matchedText.value + " ")
}

println(result) // => 12 34

```

The `matchedResults` variable is a sequence with `MatchResult` objects. With an `input` string without digits, the `findAll(..)` function will return an empty sequence.

### matchEntire(input: CharSequence): MatchResult?

If all the characters in the `input` string matches the regex `pattern`, a string equal to the `input` will be returned. Else, `null` will be returned.

Returns the input string if the whole input string is a number:

```kotlin
val a = Regex("""\d+""").matchEntire("100")?.value             // a: 100
val b = Regex("""\d+""").matchEntire("100 dollars")?.value     // b: null

```

### matches(input: CharSequence): Boolean

Returns true if the whole input string matches the regex pattern. False otherwise.

Tests if two strings contains only digits:

```kotlin
val regex = Regex(pattern = """\d+""")
regex.matches(input = "50")             // => true
regex.matches(input = "50 dollars")     // => false

```

### containsMatchIn(input: CharSequence): Boolean

Returns true if part of the input string matches the regex pattern. False otherwise.

Test if two strings contains at least one digit:

```kotlin
Regex("""\d+""").containsMatchIn("50 dollars")       // => true
Regex("""\d+""").containsMatchIn("Fifty dollars")    // => false

```

### split(input: CharSequence, limit: Int): List

Returns a new list without all the regex matches.

To return lists without digits:

```kotlin
val a = Regex("""\d+""").split("ab12cd34ef")     // a: [ab, cd, ef]
val b = Regex("""\d+""").split("This is a test") // b: [This is a test]

```

There is one element in the list for each split. The first `input` string has three numbers. That results in a list with three elements.

### replace(input: CharSequence, replacement: String): String

Replaces all matches of the regex `pattern` in the `input` string with the replacement string.

To replace all digits in a string with an x:

```kotlin
val result = Regex("""\d+""").replace("ab12cd34ef", "x") // result: abxcdxef

```

