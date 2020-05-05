---
metaTitle: "C# | Extension Methods"
description: "Extension methods - overview, Null checking, Explicitly using an extension method, Extension methods can only see public (or internal) members of the extended class, Generic Extension Methods, Extension methods for chaining, Extension methods with Enumeration, Extension methods dispatch based on static type, Extension methods on Interfaces, Extension methods aren't supported by dynamic code., Extension methods in combination with interfaces, Extensions and interfaces together enable DRY code and mixin-like functionality, IList<T> Extension Method Example: Comparing 2 Lists, Extension methods as strongly typed wrappers, Using Extension methods to create beautiful mapper classes, Using Extension methods to build new collection types (e.g. DictList), Extension methods for handling special cases, Using Extension methods with Static methods and Callbacks"
---

# Extension Methods




## Extension methods - overview


Extension methods were introduced in C# 3.0. Extension methods extend and add behavior to existing types without creating a new derived type, recompiling, or otherwise modifying the original type. **They are especially helpful when you cannot modify the source of a type you are looking to enhance.**  Extension methods may be created for system types, types defined by third parties, and types that you have defined yourself. The extension method can be invoked as though it were a member method of the original type. This allows for **Method Chaining** used to implement a **Fluent Interface**.

An extension method is created by adding a **static method** to a **static class** which is distinct from the original type being extended.  The static class holding the extension method is often created for the sole purpose of holding extension methods.

Extension methods take a special first parameter that designates the original type being extended.  This first parameter is decorated with the keyword `this` (which constitutes a special and distinct use of `this` in C#—it should be understood as different from the use of `this` which allows referring to members of the current object instance).

In the following example, the original type being extended is the class `string`.  `String` has been extended by a method `Shorten()`, which provides the additional functionality of shortening.  The static class `StringExtensions` has been created to hold the extension method.  The extension method `Shorten()` shows that it is an extension of `string` via the specially marked first parameter.  To show that the `Shorten()` method extends `string`, the first parameter is marked with `this`.  Therefore, the full signature of the first parameter is `this string text`, where `string` is the original type being extended and `text` is the chosen parameter name.

```cs
static class StringExtensions
{
    public static string Shorten(this string text, int length) 
    {
        return text.Substring(0, length);
    }
}

class Program
{
    static void Main()
    {
        // This calls method String.ToUpper()
        var myString = "Hello World!".ToUpper();

        // This calls the extension method StringExtensions.Shorten()
        var newString = myString.Shorten(5); 

        // It is worth noting that the above call is purely syntactic sugar
        // and the assignment below is functionally equivalent
        var newString2 = StringExtensions.Shorten(myString, 5);
    }
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/uiPhpP)

The object passed as the **first argument of an extension method** (which is accompanied by the `this` keyword) is the instance the extension method is called upon.

For example, when this code is executed:

```cs
"some string".Shorten(5);

```

The values of the arguments are as below:

```cs
text: "some string"
length: 5

```

**Note that extension methods are only usable if they are in the same namespace as their definition, if the namespace is imported explicitly by the code using the extension method, or if the extension class is namespace-less.** The .NET framework guidelines recommend putting extension classes in their own namespace. However, this may lead to discovery issues.

This results in no conflicts between the extension methods and the libraries being used, unless namespaces which might conflict are explicitly pulled in. For example [LINQ Extensions](http://stackoverflow.com/documentation/c%23/68/linq-queries):

```cs
using System.Linq; // Allows use of extension methods from the System.Linq namespace

class Program
{
    static void Main()
    {
        var ints = new int[] {1, 2, 3, 4};

        // Call Where() extension method from the System.Linq namespace
        var even = ints.Where(x => x % 2 == 0); 
    }
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/IF223c)

Since C# 6.0, it is also possible to put a `using static` directive to the **class** containing the extension methods. For example, `using static System.Linq.Enumerable;`. This makes extension methods from that particular class available without bringing other types from the same namespace into scope.

When a class method with the same signature is available, the compiler prioritizes it over the extension method call. For example:

```cs
class Test
{
   public void Hello()
   {
       Console.WriteLine("From Test");
   }
}

static class TestExtensions
{
    public static void Hello(this Test test)
    {
        Console.WriteLine("From extension method");
    }
}

class Program
{
    static void Main()
    {
        Test t = new Test();
        t.Hello(); // Prints "From Test"
    }
}

```

[Live demo on .NET Fiddle](https://dotnetfiddle.net/fI3sCJ)

Note that if there are two extension functions with the same signature, and one of them is in the same namespace, then that one will be prioritized. On the other hand, if both of them are accessed by `using`, then a compile time error will ensue with the message:

> 
**The call is ambiguous between the following methods or properties**


Note that the syntactic convenience of calling an extension method via `originalTypeInstance.ExtensionMethod()` is an optional convenience.  The method can also be called in the traditional manner, so that the special first parameter is used as a parameter to the method.

I.e., both of the following work:

```cs
//Calling as though method belongs to string--it seamlessly extends string
String s = "Hello World";
s.Shorten(5);  

//Calling as a traditional static method with two parameters
StringExtensions.Shorten(s, 5);

```



## Null checking


Extension methods are static methods which behave like instance methods. However, unlike what happens when calling an instance method on a `null` reference, when an extension method is called with a `null` reference, it does not throw a [`NullReferenceException`](https://msdn.microsoft.com/en-us/library/system.nullreferenceexception(v=vs.110).aspx). This can be quite useful in some scenarios.

For example, consider the following static class:

```cs
public static class StringExtensions
{
    public static string EmptyIfNull(this string text)
    {
        return text ?? String.Empty;
    }

    public static string NullIfEmpty(this string text)
    {
        return String.Empty == text ? null : text;
    }
}

```

```cs
string nullString = null;
string emptyString = nullString.EmptyIfNull();// will return ""
string anotherNullString = emptyString.NullIfEmpty(); // will return null

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/jNQWqg)



## Explicitly using an extension method


Extension methods can also be used like ordinary static class methods. This way of calling an extension method is more verbose, but is necessary in some cases.

```cs
static class StringExtensions
{
    public static string Shorten(this string text, int length) 
    {
        return text.Substring(0, length);
    }
}

```

Usage:

```cs
var newString = StringExtensions.Shorten("Hello World", 5);

```

### When to call extension methods as static methods

There are still scenarios where you would need to use an extension method as a static method:

- Resolving conflict with a member method. This can happen if a new version of a library introduces a new member method with the same signature. In this case, the member method will be preferred by the compiler.
- Resolving conflicts with another extension method with the same signature. This can happen if two libraries include similar extension methods and namespaces of both classes with extension methods are used in the same file.
- Passing extension method as a method group into delegate parameter.
- Doing your own binding through `Reflection`.
- Using the extension method in the Immediate window in Visual Studio.

### Using static

If a `using static` directive is used to bring static members of a static class into global scope, extension methods are skipped. Example:

```cs
using static OurNamespace.StringExtensions; // refers to class in previous example

// OK: extension method syntax still works.
"Hello World".Shorten(5);
// OK: static method syntax still works.
OurNamespace.StringExtensions.Shorten("Hello World", 5);
// Compile time error: extension methods can't be called as static without specifying class.
Shorten("Hello World", 5);

```

If you remove the `this` modifier from the first argument of the `Shorten` method, the last line will compile.



## Extension methods can only see public (or internal) members of the extended class


```cs
public class SomeClass
{
    public void DoStuff()
    {
        
    }

    protected void DoMagic()
    {
        
    }
}

public static class SomeClassExtensions
{
    public static void DoStuffWrapper(this SomeClass someInstance)
    {
        someInstance.DoStuff(); // ok
    }

    public static void DoMagicWrapper(this SomeClass someInstance)
    {
        someInstance.DoMagic(); // compilation error
    }
}

```

Extension methods are just a syntactic sugar, and are not actually members of the class they extend. This means that they cannot break encapsulation—they only have access to `public` (or when implemented in the same assembly, `internal`) fields, properties and methods.



## Generic Extension Methods


Just like other methods, extension methods can use generics.  For example:

```cs
static class Extensions
{
    public static bool HasMoreThanThreeElements<T>(this IEnumerable<T> enumerable)
    {
        return enumerable.Take(4).Count() > 3;
    }
}

```

and calling it would be like:

```cs
IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
var hasMoreThanThreeElements = numbers.HasMoreThanThreeElements();

```

[View Demo](https://dotnetfiddle.net/UlCa3i)

Likewise for multiple Type Arguments:

```cs
public static TU GenericExt<T, TU>(this T obj)
{
     TU ret = default(TU);
     // do some stuff with obj
     return ret;
}

```

Calling it would be like:

```cs
IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
var result = numbers.GenericExt<IEnumerable<int>,String>();

```

[View Demo](https://dotnetfiddle.net/aMNO0X)

You can also create extension methods for partially bound types in multi generic types:

```cs
class MyType<T1, T2>
{
}

static class Extensions
{
    public static void Example<T>(this MyType<int, T> test)
    {        
    }
}

```

Calling it would be like:

```cs
MyType<int, string> t = new MyType<int, string>();
t.Example();

```

[View Demo](https://dotnetfiddle.net/1FjUOH)

You can also specify type constraints with [`where`](https://stackoverflow.com/documentation/c%23/26/keywords/8137/where-type-constraints#t=201607221442171394675) :

```cs
public static bool IsDefault<T>(this T obj) where T : struct, IEquatable<T>
{
     return EqualityComparer<T>.Default.Equals(obj, default(T));
}

```

Calling code:

```cs
int number = 5;
var IsDefault = number.IsDefault();

```

[View Demo](https://dotnetfiddle.net/Jom3cS)



## Extension methods for chaining


When an extension method returns a value that has the same type as its `this` argument, it can be used to "chain" one or more method calls with a compatible signature. This can be useful for sealed and/or primitive types, and allows the creation of so-called "fluent" APIs if the method names read like natural human language.

```cs
void Main()
{
    int result = 5.Increment().Decrement().Increment(); 
    // result is now 6
}

public static class IntExtensions 
{
    public static int Increment(this int number) {
        return ++number;
    }

    public static int Decrement(this int number) {
        return --number;
    }
}

```

Or like this

```cs
void Main()
{
    int[] ints = new[] { 1, 2, 3, 4, 5, 6};
    int[] a = ints.WhereEven();
    //a is { 2, 4, 6 };
    int[] b = ints.WhereEven().WhereGreaterThan(2);
    //b is { 4, 6 };
}

public static class IntArrayExtensions
{
    public static int[] WhereEven(this int[] array)
    {
        //Enumerable.* extension methods use a fluent approach
        return array.Where(i => (i%2) == 0).ToArray();
    }

    public static int[] WhereGreaterThan(this int[] array, int value)
    {
        return array.Where(i => i > value).ToArray();
    }
}

```



## Extension methods with Enumeration


Extension methods are useful for adding functionality to enumerations.

One common use is to implement a conversion method.

```cs
public enum YesNo
{
    Yes,
    No,
}

public static class EnumExtentions
{
    public static bool ToBool(this YesNo yn)
    {
        return yn == YesNo.Yes;
    }
    public static YesNo ToYesNo(this bool yn)
    {
        return yn ? YesNo.Yes : YesNo.No;
    }
}

```

Now you can quickly convert your enum value to a different type. In this case a bool.

```cs
bool yesNoBool = YesNo.Yes.ToBool(); // yesNoBool == true
YesNo yesNoEnum = false.ToYesNo();   // yesNoEnum == YesNo.No

```

Alternatively extension methods can be used to add property like methods.

```cs
public enum Element
{
    Hydrogen,
    Helium,
    Lithium,
    Beryllium,
    Boron,
    Carbon,
    Nitrogen,
    Oxygen
    //Etc
}

public static class ElementExtensions
{
    public static double AtomicMass(this Element element)
    {
        switch(element)
        {
            case Element.Hydrogen:  return 1.00794;
            case Element.Helium:    return 4.002602;
            case Element.Lithium:   return 6.941;
            case Element.Beryllium: return 9.012182;
            case Element.Boron:     return 10.811;
            case Element.Carbon:    return 12.0107;
            case Element.Nitrogen:  return 14.0067;
            case Element.Oxygen:    return 15.9994;
            //Etc
        }
        return double.Nan;
    }
}

var massWater = 2*Element.Hydrogen.AtomicMass() + Element.Oxygen.AtomicMass();

```



## Extension methods dispatch based on static type


The static (compile-time) type is used rather than the dynamic (run-time type) to match parameters.

```cs
public class Base 
{ 
    public virtual string GetName()
    {
        return "Base";
    }
}

public class Derived : Base
{ 
    public override string GetName()
    {
        return "Derived";
    }
}

public static class Extensions
{
    public static string GetNameByExtension(this Base item)
    {
        return "Base";
    }

    public static string GetNameByExtension(this Derived item)
    {
        return "Derived";
    }
}

public static class Program   
{
    public static void Main()
    {
        Derived derived = new Derived();
        Base @base = derived;

        // Use the instance method "GetName"
        Console.WriteLine(derived.GetName()); // Prints "Derived"
        Console.WriteLine(@base.GetName()); // Prints "Derived"

        // Use the static extension method "GetNameByExtension"
        Console.WriteLine(derived.GetNameByExtension()); // Prints "Derived"
        Console.WriteLine(@base.GetNameByExtension()); // Prints "Base"
    }
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/7BGp8o)

Also the dispatch based on static type does not allow an extension method to be called on a `dynamic` object:

```cs
public class Person
{
    public string Name { get; set; }
}

public static class ExtenionPerson
{
    public static string GetPersonName(this Person person)
    {
        return person.Name;
    }
}

dynamic person = new Person { Name = "Jon" };
var name = person.GetPersonName(); // RuntimeBinderException is thrown

```



## Extension methods on Interfaces


One useful feature of extension methods is that you can create common methods for an interface. Normally an interface cannot have shared implementations, but with extension methods they can.

```cs
public interface IVehicle
{
    int MilesDriven { get; set; }
}

public static class Extensions
{
    public static int FeetDriven(this IVehicle vehicle)
    {
        return vehicle.MilesDriven * 5028;
    }
}

```

In this example, the method `FeetDriven` can be used on any `IVehicle`. This logic in this method would apply to all `IVehicle`s, so it can be done this way so that there doesn't have to be a `FeetDriven` in the `IVehicle` definition which would be implemented the same way for all children.



## Extension methods aren't supported by dynamic code.


```cs
static class Program
{
    static void Main()
    {
        dynamic dynamicObject = new ExpandoObject();

        string awesomeString = "Awesome";

        // Prints True
        Console.WriteLine(awesomeString.IsThisAwesome());

        dynamicObject.StringValue = awesomeString;

        // Prints True
        Console.WriteLine(StringExtensions.IsThisAwesome(dynamicObject.StringValue)); 
        
        // No compile time error or warning, but on runtime throws RuntimeBinderException
        Console.WriteLine(dynamicObject.StringValue.IsThisAwesome());
    }
}

static class StringExtensions
{
    public static bool IsThisAwesome(this string value)
    {
        return value.Equals("Awesome");
    }
}

```

> 
The reason [calling extension methods from dynamic code] doesn't work is because in regular, non-dynamic code extension methods work by doing a full search of all the classes known to the compiler for a static class that has an extension method that matches. The search goes in order based on the namespace nesting and available `using` directives in each namespace.
That means that in order to get a dynamic extension method invocation resolved correctly, somehow the DLR has to know **at runtime** what all the namespace nestings and `using` directives were **in your source code**.  We do not have a mechanism handy for encoding all that information into the call site. We considered inventing such a mechanism, but decided that it was too high cost and produced too much schedule risk to be worth it.


[Source](http://stackoverflow.com/a/5313149/1610754)



## Extension methods in combination with interfaces


It is very convenient to use extension methods with interfaces as implementation can be stored outside of class and all it takes to add some functionality to class is to decorate class with interface.

```cs
public interface IInterface
{
   string Do()
}

public static class ExtensionMethods{
    public static string DoWith(this IInterface obj){
      //does something with IInterface instance
    }
}

public class Classy : IInterface
{
   // this is a wrapper method; you could also call DoWith() on a Classy instance directly,
   // provided you import the namespace containing the extension method
   public Do(){
       return this.DoWith();
   }
}

```

use like:

```

var classy = new Classy();
 classy.Do(); // will call the extension
 classy.DoWith(); // Classy implements IInterface so it can also be called this way

```



## Extensions and interfaces together enable DRY code and mixin-like functionality


Extension methods enable you to simplify your interface definitions by only including core required functionality in the interface itself and allowing you to define convenience methods and overloads as extension methods. Interfaces with fewer methods are easier to implement in new classes. Keeping overloads as extensions rather than including them in the interface directly saves you from copying boilerplate code into every implementation, helping you keep your code DRY. This in fact is similar to the mixin pattern which C# does not support.

`System.Linq.Enumerable`’s extensions to `IEnumerable<T>` is a great example of this. `IEnumerable<T>` only requires the implementing class to implement two methods: generic and non-generic `GetEnumerator()`. But `System.Linq.Enumerable` provides countless useful utilities as extensions enabling concise and clear consumption of `IEnumerable<T>`.

The following is a very simple interface with convenience overloads provided as extensions.

```cs
public interface ITimeFormatter
{
   string Format(TimeSpan span);
}

public static class TimeFormatter
{
    // Provide an overload to *all* implementers of ITimeFormatter.
    public static string Format(
        this ITimeFormatter formatter,
        int millisecondsSpan)
        => formatter.Format(TimeSpan.FromMilliseconds(millisecondsSpan));
}

// Implementations only need to provide one method. Very easy to
// write additional implementations.
public class SecondsTimeFormatter : ITimeFormatter
{
   public string Format(TimeSpan span)
   {
       return $"{(int)span.TotalSeconds}s";
   }
}

class Program
{
    static void Main(string[] args)
    {
        var formatter = new SecondsTimeFormatter();
        // Callers get two method overloads!
        Console.WriteLine($"4500ms is rougly {formatter.Format(4500)}");
        var span = TimeSpan.FromSeconds(5);
        Console.WriteLine($"{span} is formatted as {formatter.Format(span)}");
    }
}

```



## IList<T> Extension Method Example: Comparing 2 Lists


You can use the following extension method for comparing the contents of two IList< T > instances of the same type.

By default the items are compared based on their order within the list and the items themselves, passing false to the `isOrdered` parameter will compare only the items themselves regardless of their order.

For this method to work, the generic type (`T`) must override both `Equals` and `GetHashCode` methods.

**Usage:**

```cs
List<string> list1 = new List<string> {"a1", "a2", null, "a3"};
List<string> list2 = new List<string> {"a1", "a2", "a3", null};

list1.Compare(list2);//this gives false
list1.Compare(list2, false);//this gives true. they are equal when the order is disregarded

```

**Method:**

```cs
public static bool Compare<T>(this IList<T> list1, IList<T> list2, bool isOrdered = true) 
{
    if (list1 == null && list2 == null)
        return true;
    if (list1 == null || list2 == null || list1.Count != list2.Count)
        return false;

    if (isOrdered)
    {
        for (int i = 0; i < list2.Count; i++)
        {
            var l1 = list1[i]; 
            var l2 = list2[i];
            if (
                 (l1 == null && l2 != null) || 
                 (l1 != null && l2 == null) || 
                 (!l1.Equals(l2)))
            {
                    return false;
            }
        }
        return true;
    }
    else
    {
        List<T> list2Copy = new List<T>(list2);
        //Can be done with Dictionary without O(n^2)
        for (int i = 0; i < list1.Count; i++)
        {
            if (!list2Copy.Remove(list1[i]))
                return false;
        }
        return true;
    }
}

```



## Extension methods as strongly typed wrappers


Extension methods can be used for writing strongly typed wrappers for dictionary-like objects. For example a cache, `HttpContext.Items` at cetera...

```cs
public static class CacheExtensions
{
    public static void SetUserInfo(this Cache cache, UserInfo data) => 
        cache["UserInfo"] = data;

    public static UserInfo GetUserInfo(this Cache cache) => 
        cache["UserInfo"] as UserInfo;
}

```

This approach removes the need of using string literals as keys all over the codebase as well as the need of casting to the required type during the read operation. Overall it creates a more secure, strongly typed way of interacting with such loosely typed objects as Dictionaries.



## Using Extension methods to create beautiful mapper classes


We can create a better mapper classes with extension methods,
Suppose if i have some DTO classes like

```cs

public class UserDTO
 {
        public AddressDTO Address { get; set; }
 }

 public class AddressDTO
 {
        public string Name { get; set; }
 }

```

and i need to map to corresponding view model classes

```cs
public class UserViewModel
{
    public AddressViewModel Address { get; set; }
}

public class AddressViewModel
{
    public string Name { get; set; }
}

```

then I can create my mapper class like below

```cs
public static class ViewModelMapper
{
      public static UserViewModel ToViewModel(this UserDTO user)
      {
            return user == null ?
                null :
                new UserViewModel()
                {
                    Address = user.Address.ToViewModel()
                    // Job = user.Job.ToViewModel(),
                    // Contact = user.Contact.ToViewModel() .. and so on
                };
      }

      public static AddressViewModel ToViewModel(this AddressDTO userAddr)
      {
            return userAddr == null ?
                null :
                new AddressViewModel()
                {
                    Name = userAddr.Name
                };
      }
}

```

Then finally i can invoke my mapper like below

```cs

   UserDTO userDTOObj = new UserDTO() {
            Address = new AddressDTO() {
                Name = "Address of the user"
            }
        };

    UserViewModel user = userDTOObj.ToViewModel(); // My DTO mapped to Viewmodel

```

The beauty here is all the mapping method have a common name (ToViewModel) and we can reuse it several ways



## Using Extension methods to build new collection types (e.g. DictList)


You can create extension methods to improve usability for nested collections like a `Dictionary` with a `List<T>` value.

Consider the following extension methods:

```cs
public static class DictListExtensions
{
    public static void Add<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
            where TCollection : ICollection<TValue>, new()
    {
        TCollection list;
        if (!dict.TryGetValue(key, out list))
        {
            list = new TCollection();
            dict.Add(key, list);
        }

        list.Add(value);
    }

    public static bool Remove<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
        where TCollection : ICollection<TValue>
    {
        TCollection list;
        if (!dict.TryGetValue(key, out list))
        {
            return false;
        }

        var ret = list.Remove(value);
        if (list.Count == 0)
        {
            dict.Remove(key);
        }
        return ret;
    }
}

```

you can use the extension methods as follows:

```cs
var dictList = new Dictionary<string, List<int>>();

dictList.Add("example", 5);
dictList.Add("example", 10);
dictList.Add("example", 15);

Console.WriteLine(String.Join(", ", dictList["example"])); // 5, 10, 15

dictList.Remove("example", 5);
dictList.Remove("example", 10);

Console.WriteLine(String.Join(", ", dictList["example"])); // 15

dictList.Remove("example", 15);

Console.WriteLine(dictList.ContainsKey("example")); // False

```

[View Demo](https://dotnetfiddle.net/UbdQuC)



## Extension methods for handling special cases


Extension methods can be used to "hide" processing of inelegant business rules that would otherwise require cluttering up a calling function with if/then statements. This is similar to and analogous to handling nulls with extension methods. For example,

```cs
public static class CakeExtensions
{
    public static Cake EnsureTrueCake(this Cake cake)
    {
        //If the cake is a lie, substitute a cake from grandma, whose cakes aren't as tasty but are known never to be lies. If the cake isn't a lie, don't do anything and return it.
        return CakeVerificationService.IsCakeLie(cake) ? GrandmasKitchen.Get1950sCake() : cake;
    }
}

```

```cs
Cake myCake = Bakery.GetNextCake().EnsureTrueCake();
myMouth.Eat(myCake);//Eat the cake, confident that it is not a lie.

```



## Using Extension methods with Static methods and Callbacks


Consider using Extension Methods as Functions which wrap other code, here's a great example that uses both a static method and and extension method to wrap the Try Catch construct. Make your code Bullet Proof...

```cs
using System;
using System.Diagnostics;

namespace Samples
{
    /// <summary>
    /// Wraps a try catch statement as a static helper which uses 
    /// Extension methods for the exception
    /// </summary>
    public static class Bullet
    {
        /// <summary>
        /// Wrapper for Try Catch Statement
        /// </summary>
        /// <param name="code">Call back for code</param>
        /// <param name="error">Already handled and logged exception</param>
        public static void Proof(Action code, Action<Exception> error)
        {
            try
            {
                code();
            }
            catch (Exception iox)
            {
                //extension method used here
                iox.Log("BP2200-ERR-Unexpected Error");
                //callback, exception already handled and logged
                error(iox);
            }
        }
        /// <summary>
        /// Example of a logging method helper, this is the extension method
        /// </summary>
        /// <param name="error">The Exception to log</param>
        /// <param name="messageID">A unique error ID header</param>
        public static void Log(this Exception error, string messageID)
        {
            Trace.WriteLine(messageID);
            Trace.WriteLine(error.Message);
            Trace.WriteLine(error.StackTrace);
            Trace.WriteLine("");
        }
    }
    /// <summary>
    /// Shows how to use both the wrapper and extension methods.
    /// </summary>
    public class UseBulletProofing
    {
        public UseBulletProofing()
        {
            var ok = false;
            var result = DoSomething();
            if (!result.Contains("ERR"))
            {
                ok = true;
                DoSomethingElse();
            }
        }

        /// <summary>
        /// How to use Bullet Proofing in your code.
        /// </summary>
        /// <returns>A string</returns>
        public string DoSomething()
        {
            string result = string.Empty;
            //Note that the Bullet.Proof method forces this construct.
            Bullet.Proof(() =>
            {
                //this is the code callback
                result = "DST5900-INF-No Exceptions in this code";
            }, error =>
            {
                //error is the already logged and handled exception
                //determine the base result
                result = "DTS6200-ERR-An exception happened look at console log";
                if (error.Message.Contains("SomeMarker"))
                {
                    //filter the result for Something within the exception message
                    result = "DST6500-ERR-Some marker was found in the exception";
                }
            });
            return result;
        }

        /// <summary>
        /// Next step in workflow
        /// </summary>
        public void DoSomethingElse()
        {
            //Only called if no exception was thrown before
        }
    }
}

```



#### Syntax


- public static ReturnType MyExtensionMethod(this TargetType target)
- public static ReturnType MyExtensionMethod(this TargetType target, TArg1 arg1, ...)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|this|The first parameter of an extension method should always be preceded by the `this` keyword, followed by the identifier with which to refer to the "current" instance of the object you are extending



#### Remarks


Extension methods are syntactic sugar that allow static methods to be invoked on object instances as if they were a member of the type itself.

Extension methods require an explicit target object. You will need to use the `this` keyword to access the method from within the extended type itself.

Extensions methods must be declared static, and must live in a static class.

**Which namespace?**

The choice of namespace for your extension method class is a trade-off between visibility and discoverability.

The most commonly mentioned [option](http://stackoverflow.com/q/1226189) is to have a custom namespace for your extension methods. However this will involve a communication effort so that users of your code know that the extension methods exist, and where to find them.

An alternative is to choose a namespace such that developers will discover your extension methods via Intellisense. So if you want to extend the `Foo` class, it is logical to put the extension methods in the same namespace as `Foo`.

It is important to realise that **nothing prevents you using "someone else's" namespace**: Thus if you want to extend `IEnumerable`, you can add your extension method in the `System.Linq` namespace.

This is not **always** a good idea. For example, in one specific case, you may want to extend a common type (`bool IsApproxEqualTo(this double value, double other)` for example), but not have that 'pollute' the whole of `System`. In this case it is preferable to chose a local, specific, namespace.

Finally, it is also possible to put the extension methods in **no namespace at all**!

A good reference question: [How do you manage the namespaces of your extension methods?](http://stackoverflow.com/questions/2520446/how-do-you-manage-the-namespaces-of-your-extension-methods)

**Applicability**

Care should be taken when creating extension methods to ensure that they are appropriate for all possible inputs and are not only relevant to specific situations. For example, it is possible to extend system classes such as `string`, which makes your new code available to **any** string. If your code needs to perform domain specific logic on a domain specific string format, an extension method would not be appropriate as its presence would confuse callers working with other strings in the system.

**The following list contains basic features and properties of extension methods**

1. It must be a static method.
1. It must be located in a static class.
1. It uses the "this" keyword as the first parameter with a type in .NET and this method will be called by a given type instance on the client side.
1. It also shown by VS intellisense. When we press the dot `.` after a type instance, then it comes in VS intellisense.
1. An extension method should be in the same namespace as it is used or you need to import the namespace of the class by a using statement.
1. You can give any name for the class that has an extension method but the class should be static.
1. If you want to add new methods to a type and you don't have the source code for it, then the solution is to use and implement extension methods of that type.
1. If you create extension methods that have the same signature methods as the type you are extending, then the extension methods will never be called.

