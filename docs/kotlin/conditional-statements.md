---
metaTitle: "Kotlin - Conditional Statements"
description: "When-statement argument matching, When-statement as expression, Standard if-statement, If-statement as an expression, When-statement instead of if-else-if chains, When-statement with enums"
---

# Conditional Statements



## When-statement argument matching


When given an argument, the `when`-statement matches the argument against the branches in sequence. The matching is done using the `==` operator which performs null checks and compares the operands using the `equals` function. The first matching one will be executed.

```kotlin
when (x) {
    "English" -> print("How are you?")
    "German" -> print("Wie geht es dir?")
    else -> print("I don't know that language yet :(")
}

```

The when statement also knows some more advanced matching options:

```kotlin
val names = listOf("John", "Sarah", "Tim", "Maggie")
when (x) {
    in names -> print("I know that name!")
    !in 1..10 -> print("Argument was not in the range from 1 to 10")
    is String -> print(x.length) // Due to smart casting, you can use String-functions here
}

```



## When-statement as expression


Like if, when can also be used as an expression:

```kotlin
val greeting = when (x) {
    "English" -> "How are you?"
    "German" -> "Wie geht es dir?"
    else -> "I don't know that language yet :("
}
print(greeting)

```

To be used as an expression, the when-statement must be exhaustive, i.e. either have an else branch or cover all possibilities with the branches in another way.



## Standard if-statement


```kotlin
val str = "Hello!"
if (str.length == 0) {
    print("The string is empty!")
} else if (str.length > 5) {
    print("The string is short!")
} else {
    print("The string is long!")
}

```

The else-branches are optional in normal if-statements.



## If-statement as an expression


If-statements can be expressions:

```kotlin
val str = if (condition) "Condition met!" else "Condition not met!"

```

Note that the `else`-branch is not optional if the `if`-statement is used as an expression.

This can also been done with a multi-line variant with curly brackets and multiple `else if` statements.

```kotlin
val str = if (condition1){
    "Condition1 met!" 
   } else if (condition2) {
    "Condition2 met!" 
   } else {
    "Conditions not met!"
   }

```

> 
TIP: Kotlin can infer the type of the variable for you but if you want to be sure of the type just annotate it on the variable like: `val str: String =` this will enforce the type and will make it easier to read.




## When-statement instead of if-else-if chains


The when-statement is an alternative to an if-statement with multiple else-if-branches:

```kotlin
when {
    str.length == 0 -> print("The string is empty!")
    str.length > 5  -> print("The string is short!")
    else            -> print("The string is long!")
}

```

Same code written using an **if-else-if** chain:

```kotlin
if (str.length == 0) {
    print("The string is empty!")
} else if (str.length > 5) {
    print("The string is short!")
} else {
    print("The string is long!")
}

```

Just like with the if-statement, the else-branch is optional, and you can add as many or as few branches as you like.
You can also have multiline-branches:

```kotlin
when {
    condition -> {
        doSomething()
        doSomeMore()
    }
    else -> doSomethingElse()
}

```



## When-statement with enums


`when` can be used to match `enum` values:

```kotlin
enum class Day {
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday
}

fun doOnDay(day: Day) {
    when(day) {
        Day.Sunday ->     // Do something
        Day.Monday, Day.Tuesday ->     // Do other thing
        Day.Wednesday ->  // ...
        Day.Thursday ->   // ...
        Day.Friday ->     // ...
        Day.Saturday ->   // ...
    }
}

```

As you can see in second case line (`Monday` and `Tuedsay`) it is also possible to combine two or more `enum` values.

If your cases are not exhaustive the compile will show an error. You can use `else` to handle default cases:

```kotlin
fun doOnDay(day: Day) {
    when(day) {
        Day.Monday ->     // Work
        Day.Tuesday ->    // Work hard
        Day.Wednesday ->  // ...
        Day.Thursday ->   //
        Day.Friday ->     //
        else ->           // Party on weekend
    }
} 

```

Though the same can be done using `if-then-else` construct, `when` takes care of missing `enum` values and makes it more natural.

Check [here](http://stackoverflow.com/documentation/kotlin/2286/enum) for more information about kotlin `enum`



#### Remarks


In contrast to Java's `switch`, the `when` statement has no fall-through behavior. This means, that if a branch is matched, the control flow returns after its execution and no `break` statement is required. If you want to combine the bahaviors for multiple arguments, you can write multiple arguments separated by commas:

```kotlin
when (x) {
    "foo", "bar" -> println("either foo or bar")
    else -> println("didn't match anything")
}

```

