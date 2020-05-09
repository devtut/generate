---
metaTitle: "Swift - Function as first class citizens in Swift"
description: "Assigning function to a variable, Passing function as an argument to another function, thus creating a Higher-Order Function, Function as return type from another function"
---

# Function as first class citizens in Swift


Functions as First-class members means, it can enjoy privileges just like Objects does. It can be assigned to a variable, passed on to a function as parameter or can be used as return type.



## Assigning function to a variable


```swift
struct Mathematics
{
    internal func performOperation(inputArray: [Int], operation: (Int)-> Int)-> [Int]
    {
        var processedArray = [Int]()
        
        for item in inputArray
        {
            processedArray.append(operation(item))
        }
        
        return processedArray
    }
    
    
    internal func performComplexOperation(valueOne: Int)-> ((Int)-> Int)
    {
        return
            ({
                 return valueOne + $0   
            })
    }
    
}


let arrayToBeProcessed = [1,3,5,7,9,11,8,6,4,2,100]

let math = Mathematics()

func add2(item: Int)-> Int
{
    return (item + 2)
}

// assigning the function to a variable and then passing it to a function as param
let add2ToMe = add2
print(math.performOperation(inputArray: arrayToBeProcessed, operation: add2ToMe))

```

**Output:**

```swift
[3, 5, 7, 9, 11, 13, 10, 8, 6, 4, 102]

```

**Similarly the above could be achieved using a** `closure`

```swift
// assigning the closure to a variable and then passing it to a function as param
let add2 = {(item: Int)-> Int in return item + 2}
print(math.performOperation(inputArray: arrayToBeProcessed, operation: add2))

```



## Passing function as an argument to another function, thus creating a Higher-Order Function


```swift
func multiply2(item: Int)-> Int
{
    return (item + 2)
}


let multiply2ToMe = multiply2

// passing the function directly to the function as param
print(math.performOperation(inputArray: arrayToBeProcessed, operation: multiply2ToMe))

```

**Output:**

```swift
[3, 5, 7, 9, 11, 13, 10, 8, 6, 4, 102]

```

**Similarly the above could be achieved using a** `closure`

```swift
// passing the closure directly to the function as param
print(math.performOperation(inputArray: arrayToBeProcessed, operation: { $0 * 2 }))

```



## Function as return type from another function


```swift
// function as return type
print(math.performComplexOperation(valueOne: 4)(5))

```

**Output:**

`9`

