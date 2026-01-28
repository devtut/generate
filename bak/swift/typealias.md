---
metaTitle: "Swift - Typealias"
description: "typealias for closures with parameters, typealias for empty closures, typealias for other types"
---

# Typealias




## typealias for closures with parameters


```swift
typealias SuccessHandler = (NSURLSessionDataTask, AnyObject?) -> Void

```

This code block creates a type alias named `SuccessHandler`, just in the same way `var string = ""` creates a variable with the name `string`.

Now whenever you use `SuccessHandler`, for example:

```swift
func example(_ handler: SuccessHandler) {}

```

You are essentilly writing:

```swift
func example(_ handler: (NSURLSessionDataTask, AnyObject?) -> Void) {}

```



## typealias for empty closures


```swift
typealias Handler = () -> Void
typealias Handler = () -> ()

```

This block creates a type alias that works as a Void to Void function (takes in no parameters and returns nothing).

Here is a usage example:

```swift
var func: Handler?

func = {}

```



## typealias for other types


```swift
typealias Number = NSNumber

```

You can also use a type alias to give a type another name to make it easier to remember, or make your code more elegant.

**typealias for Tuples**

```swift
typealias PersonTuple = (name: String, age: Int, address: String)

```

And this can be used as:

```swift
func getPerson(for name: String) -> PersonTuple {
    //fetch from db, etc
    return ("name", 45, "address")
}

```

