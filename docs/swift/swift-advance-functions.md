---
metaTitle: "Swift - Swift Advance functions"
description: "Introduction with advance functions, Flatten multidimensional array"
---

# Swift Advance functions


Advance functions like `map`, `flatMap`, `filter`, and `reduce` are used to operate on various collection types like Array and Dictionary. Advance functions typically require little code and can be chained together in order to build up complex logic in a concise way.



## Introduction with advance functions


Let's take an scenario to understand advance function in better way,

```swift
struct User {
    var name: String
    var age: Int
    var country: String?
}

//User's information 
let user1 = User(name: "John", age: 24, country: "USA")
let user2 = User(name: "Chan", age: 20, country: nil)
let user3 = User(name: "Morgan", age: 30, country: nil)
let user4 = User(name: "Rachel", age: 20, country: "UK")
let user5 = User(name: "Katie", age: 23, country: "USA")
let user6 = User(name: "David", age: 35, country: "USA")
let user7 = User(name: "Bob",age: 22, country: nil)

//User's array list
let arrUser = [user1, user2, user3, user4, user5, user6, user7]

```

**Map Function:**

Use map to loop over a collection and apply the same operation to each element in the collection. The map function returns an array containing the results of applying a mapping or transform function to each item.

```swift
//Fetch all the user's name from array 
let arrUserName = arrUser.map({ $0.name }) // ["John", "Chan", "Morgan", "Rachel", "Katie", "David", "Bob"]

```

**Flat-Map Function:**

The simplest use is as the name suggests to flatten a collection of collections.

```swift
// Fetch all user country name & ignore nil value.
let arrCountry = arrUser.flatMap({ $0.country }) // ["USA", "UK", "USA", "USA"]

```

**Filter Function:**

Use filter to loop over a collection and return an Array containing only those elements that match an include condition.

```swift
// Filtering USA user from the array user list.
let arrUSAUsers = arrUser.filter({ $0.country == "USA" }) // [user1, user5, user6]

// User chaining methods to fetch user's name who live in USA 
let arrUserList = arrUser.filter({ $0.country == "USA" }).map({ $0.name }) // ["John", "Katie", "David"]

```

**Reduce:**

Use reduce to combine all items in a collection to create a single new value.

Swift 2.3:-

```swift
//Fetch user's total age.
let arrUserAge = arrUser.map({ $0.age }).reduce(0, combine: { $0 + $1 }) //174

//Prepare all user name string with seperated by comma 
let strUserName = arrUserName.reduce("", combine: { $0 == "" ? $1 : $0 + ", " + $1 }) // John, Chan, Morgan, Rachel, Katie, David, Bob

```

Swift 3:-

```swift
//Fetch user's total age.
let arrUserAge = arrUser.map({ $0.age }).reduce(0, { $0 + $1 }) //174

//Prepare all user name string with seperated by comma 
let strUserName = arrUserName.reduce("", { $0 == "" ? $1 : $0 + ", " + $1 }) // John, Chan, Morgan, Rachel, Katie, David, Bob

```



## Flatten multidimensional array


To flatten multidimensional array into single dimension, flatMap advance functions is used. Other use case is to neglect nil value from array & mapping values. Let's check with example:-

Suppose We have an multidimensional array of cities & we want to sorted city name list in ascending order. In that case we can use flatMap function like:-

```swift
let arrStateName = [["Alaska", "Iowa", "Missouri", "New Mexico"], ["New York", "Texas", "Washington", "Maryland"], ["New Jersey", "Virginia", "Florida", "Colorado"]]

```

Preparing a single dimensional list from multidimensional array,

```swift
let arrFlatStateList = arrStateName.flatMap({ $0 }) // ["Alaska", "Iowa", "Missouri", "New Mexico", "New York", "Texas", "Washington", "Maryland", "New Jersey", "Virginia", "Florida", "Colorado"]

```

For sorting array values, we can use chaining operation or sort flatten array. Here below example showing chaining operation,

```swift
// Swift 2.3 syntax 
let arrSortedStateList = arrStateName.flatMap({ $0 }).sort(<) // ["Alaska",     "Colorado", "Florida", "Iowa", "Maryland", "Missouri", "New Jersey", "New Mexico", "New York", "Texas", "Virginia", "Washington"]

// Swift 3 syntax
let arrSortedStateList = arrStateName.flatMap({ $0 }).sorted(by: <) // ["Alaska", "Colorado", "Florida", "Iowa", "Maryland", "Missouri", "New Jersey", "New Mexico", "New York", "Texas", "Virginia", "Washington"]

```

