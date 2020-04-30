---
metaTitle: "Null-Coalescing Operator"
description: "Basic usage, Null fall-through and chaining, Null coalescing operator with method calls, Use existing or create new, Lazy properties initialization with null coalescing operator"
---

# Null-Coalescing Operator



## Basic usage


Using the [`null-coalescing operator (??)`](https://msdn.microsoft.com/en-us/library/ms173224.aspx) allows you to specify a default value for a nullable type if the left-hand operand is `null`.

```cs
string testString = null;
Console.WriteLine("The specified string is - " + (testString ?? "not provided"));

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/GNosPU)

This is logically equivalent to:

```cs
string testString = null;
if (testString == null)
{
    Console.WriteLine("The specified string is - not provided");
}
else
{
    Console.WriteLine("The specified string is - " + testString);
}

```

or using the [ternary operator (?:)](http://stackoverflow.com/documentation/c%23/18/operators/6029/ternary-operator#t=201610101110242934481) operator:

```cs
string testString = null;
Console.WriteLine("The specified string is - " + (testString == null ? "not provided" : testString));

```



## Null fall-through and chaining


The left-hand operand must be nullable, while the right-hand operand may or may not be. The result will be typed accordingly.

**Non-nullable**

```cs
int? a = null;
int b = 3;
var output = a ?? b;
var type = output.GetType();  

Console.WriteLine($"Output Type :{type}");
Console.WriteLine($"Output value :{output}");

```

**Output:**

> 
<p>Type :System.Int32<br />
value :3</p>


[View Demo](https://dotnetfiddle.net/hKHOcN)

**Nullable**

```cs
int? a = null;
int? b = null;
var output = a ?? b;

```

`output` will be of type `int?` and equal to `b`, or `null`.

**Multiple Coalescing**

Coalescing can also be done in chains:

```cs
int? a = null;
int? b = null;
int c = 3;
var output = a ?? b ?? c;

var type = output.GetType();    
Console.WriteLine($"Type :{type}");
Console.WriteLine($"value :{output}");

```

**Output:**

> 
<p>Type :System.Int32<br />
value :3</p>


[View Demo](https://dotnetfiddle.net/xC8Bmc)

**Null Conditional Chaining**

The null coalescing operator can be used in tandem with the [null propagation operator](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607280322338995462) to provide safer access to properties of objects.

```cs
object o = null;
var output = o?.ToString() ?? "Default Value";

```

**Output:**

> 
<p>Type :System.String<br />
value :Default Value</p>


[View Demo](https://dotnetfiddle.net/nk1QRn)



## Null coalescing operator with method calls


The null coalescing operator makes it easy to ensure that a method that may return `null` will fall back to a default value.

Without the null coalescing operator:

```cs
string name = GetName();

if (name == null)
    name = "Unknown!";

```

With the null coalescing operator:

```cs
string name = GetName() ?? "Unknown!";

```



## Use existing or create new


A common usage scenario that this feature really helps with is when you are looking for an object in a collection and need to create a new one if it does not already exist.

```cs
IEnumerable<MyClass> myList = GetMyList();
var item = myList.SingleOrDefault(x => x.Id == 2) ?? new MyClass { Id = 2 };

```



## Lazy properties initialization with null coalescing operator


```cs
private List<FooBar> _fooBars;

public List<FooBar> FooBars
{
    get { return _fooBars ?? (_fooBars = new List<FooBar>()); }
}

```

The first time the property `.FooBars` is accessed the `_fooBars` variable will evaluate as `null`, thus falling through to the assignment statement assigns and evaluates to the resulting value.

### Thread safety

This is **not thread-safe** way of implementing lazy properties. For thread-safe laziness, use the [`Lazy<T>`](http://stackoverflow.com/documentation/c%23/1192/singleton-implementation/6795/lazy-thread-safe-singleton-using-lazyt) class built into the .NET Framework.

### C# 6 Syntactic Sugar using expression bodies

Note that since C# 6, this syntax can be simplified using expression body for the property:

```cs
private List<FooBar> _fooBars;

public List<FooBar> FooBars => _fooBars ?? ( _fooBars = new List<FooBar>() );

```

Subsequent accesses to the property will yield the value stored in the `_fooBars` variable.

### Example in the MVVM pattern

This is often used when implementing commands in the MVVM pattern. Instead of initializing the commands eagerly with the construction of a viewmodel, commands are lazily initialized using this pattern as follows:

```cs
private ICommand _actionCommand = null;
public ICommand ActionCommand =>
   _actionCommand ?? ( _actionCommand = new DelegateCommand( DoAction ) );

```



#### Syntax


- var result = possibleNullObject ?? defaultValue;



#### Parameters


|Parameter|Details
|------
|`possibleNullObject`|The value to test for null value. If non null, this value is returned. Must be a nullable type.
|`defaultValue`|The value returned if `possibleNullObject` is null. Must be the same type as `possibleNullObject`.



#### Remarks


The null coalescing operator itself is two consecutive question mark characters: `??`

It is a shorthand for the conditional expression:

```cs
possibleNullObject != null ? possibleNullObject : defaultValue

```

The left-side operand (object being tested) must be a nullable value type or reference type, or a compile error will occur.

The ?? operator works for both reference types and value types.

