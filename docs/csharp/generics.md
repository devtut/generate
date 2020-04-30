---
metaTitle: "Generics"
description: "Implicit type inference (methods), Type inference (classes), Using generic method with an interface as a constraint type., Type constraints (new-keyword), Type constraints (classes and interfaces), Reflecting on type parameters, Covariance, Contravariance, Invariance, Variant interfaces, Checking equality of generic values., Type Parameters (Interfaces), Variant delegates, Variant types as parameters and return values, Type Parameters (Classes), Type Parameters (Methods), Type constraints (class and struct), Explicit type parameters, Generic type casting, Configuration reader with generic type casting"
---

# Generics




## Implicit type inference (methods)


When passing formal arguments to a generic method, relevant generic type arguments can usually be inferred implicitly. If all generic type can be inferred, then specifying them in the syntax is optional.

Consider the following generic method. It has one formal parameter and one generic type parameter. There is a very obvious relationship between them -- the type passed as an argument to the generic type parameter must be the same as the compile-time type of the argument passed to the formal parameter.

```cs
void M<T>(T obj)
{
}

```

These two calls are equivalent:

```cs
M<object>(new object());
M(new object());

```

These two calls are also equivalent:

```cs
M<string>("");
M("");

```

And so are these three calls:

```cs
M<object>("");
M((object) "");
M("" as object);

```

Notice that if at least one type argument cannot be inferred, then all of them have to be specified.

Consider the following generic method. The first generic type argument is the same as the type of the formal argument. But there is no such relationship for the second generic type argument. Therefore, the compiler has no way of inferring the second generic type argument in any call to this method.

```cs
void X<T1, T2>(T1 obj)
{
}

```

This doesn't work anymore:

```cs
X("");

```

This doesn't work either, because the compiler isn't sure if we are specifying the first or the second generic parameter (both would be valid as `object`):

```cs
X<object>("");

```

We are required to type out both of them, like this:

```cs
X<string, object>("");

```



## Type inference (classes)


Developers can be caught out by the fact that type inference **doesn't work** for constructors:

```cs
class Tuple<T1,T2>
{
   public Tuple(T1 value1, T2 value2)
   {
   }
}

var x = new Tuple(2, "two");              // This WON'T work...
var y = new Tuple<int, string>(2, "two"); // even though the explicit form will.

```

The first way of creating instance without explicitly specifying type parameters will cause compile time error which would say:

> 
Using the generic type 'Tuple<T1, T2>' requires 2 type arguments


A common workaround is to add a helper method in a static class:

```cs
static class Tuple
{
    public static Tuple<T1, T2> Create<T1, T2>(T1 value1, T2 value2)
    {
         return new Tuple<T1, T2>(value1, value2);
    }
}

var x = Tuple.Create(2, "two");  // This WILL work...

```



## Using generic method with an interface as a constraint type.


This is an example of how to use the generic type TFood inside Eat method on the class Animal

```cs
public interface IFood
{
    void EatenBy(Animal animal);
}

public class Grass: IFood
{
    public void EatenBy(Animal animal)
    {
        Console.WriteLine("Grass was eaten by: {0}", animal.Name);
    }
}

public class Animal
{
    public string Name { get; set; }

    public void Eat<TFood>(TFood food)
        where TFood : IFood
    {
        food.EatenBy(this);
    }
}

public class Carnivore : Animal
{
    public Carnivore()
    {
        Name = "Carnivore";
    }
}

public class Herbivore : Animal, IFood
{
    public Herbivore()
    {
        Name = "Herbivore";
    }
    
    public void EatenBy(Animal animal)
    {
        Console.WriteLine("Herbivore was eaten by: {0}", animal.Name);
    }
}

```

You can call the Eat method like this:

```cs
var grass = new Grass();        
var sheep = new Herbivore();
var lion = new Carnivore();
    
sheep.Eat(grass);
//Output: Grass was eaten by: Herbivore

lion.Eat(sheep);
//Output: Herbivore was eaten by: Carnivore

```

In this case if you try to call:

```cs
sheep.Eat(lion);

```

It won't be possible because the object lion does not implement the interface IFood.  Attempting to make the above call will generate a compiler error: "The type 'Carnivore' cannot be used as type parameter 'TFood' in the generic type or method 'Animal.Eat(TFood)'. There is no implicit reference conversion from 'Carnivore' to 'IFood'."



## Type constraints (new-keyword)


By using the `new()` constraint, it is possible to enforce type parameters to define an empty (default) constructor.

```cs
class Foo
{
    public Foo () { }
}

class Bar
{
    public Bar (string s) { ... }
}

class Factory<T>
    where T : new()
{
    public T Create()
    {
        return new T();
    }
}

Foo f = new Factory<Foo>().Create(); // Valid.
Bar b = new Factory<Bar>().Create(); // Invalid, Bar does not define a default/empty constructor.

```

The second call to to `Create()` will give compile time error with following message:

> 
'Bar' must be a non-abstract type with a public parameterless constructor in order to use it as parameter 'T' in the generic type or method 'Factory'


There is no constraint for a constructor with parameters, only parameterless constructors are supported.



## Type constraints (classes and interfaces)


Type constraints are able to force a type parameter to implement a certain interface or class.

```cs
interface IType;
interface IAnotherType;

// T must be a subtype of IType
interface IGeneric<T>
    where T : IType
{
}

// T must be a subtype of IType
class Generic<T>
    where T : IType
{
}

class NonGeneric
{
    // T must be a subtype of IType
    public void DoSomething<T>(T arg)
        where T : IType
    {
    }
}

// Valid definitions and expressions:
class Type : IType { }
class Sub : IGeneric<Type> { }
class Sub : Generic<Type> { }
new NonGeneric().DoSomething(new Type());

// Invalid definitions and expressions:
class AnotherType : IAnotherType { }
class Sub : IGeneric<AnotherType> { }
class Sub : Generic<AnotherType> { }
new NonGeneric().DoSomething(new AnotherType());

```

Syntax for multiple constraints:

```cs
class Generic<T, T1>
    where T : IType 
    where T1 : Base, new()
{
}

```

Type constraints works in the same way as inheritance, in that it is possible to specify multiple interfaces as constraints on the generic type, but only one class:

```cs
class A { /* ... */ }
class B { /* ... */ }

interface I1 { }
interface I2 { }

class Generic<T>
    where T : A, I1, I2
{
}

class Generic2<T>
    where T : A, B //Compilation error
{
}

```

Another rule is that the class must be added as the first constraint and then the interfaces:

```cs
class Generic<T>
    where T : A, I1
{
}

class Generic2<T>
    where T : I1, A //Compilation error
{
}

```

All declared constraints must be satisfied simultaneously for a particular generic instantiation to work. There is no way to specify two or more alternative sets of constraints.



## Reflecting on type parameters


The `typeof` operator works on type parameters.

```cs
class NameGetter<T>
{
    public string GetTypeName()
    {
        return typeof(T).Name;
    }
}

```



## Covariance


When is an `IEnumerable<T>` a subtype of a different `IEnumerable<T1>`? When `T` is a subtype of `T1`. `IEnumerable` is **covariant** in its `T` parameter, which means that `IEnumerable`'s subtype relationship goes in **the same direction** as `T`'s.

```cs
class Animal { /* ... */ }
class Dog : Animal { /* ... */ }

IEnumerable<Dog> dogs = Enumerable.Empty<Dog>();
IEnumerable<Animal> animals = dogs;  // IEnumerable<Dog> is a subtype of IEnumerable<Animal>
// dogs = animals;  // Compilation error - IEnumerable<Animal> is not a subtype of IEnumerable<Dog>

```

An instance of a covariant generic type with a given type parameter is implicitly convertible to the same generic type with a less derived type parameter.

This relationship holds because `IEnumerable` **produces** `T`s but doesn't consume them. An object that produces `Dog`s can be used as if it produces `Animal`s.

Covariant type parameters are declared using the `out` keyword, because the parameter must be used only as an **output**.

```cs
interface IEnumerable<out T> { /* ... */ }

```

A type parameter declared as covariant may not appear as an input.

```cs
interface Bad<out T>
{
    void SetT(T t);  // type error
}

```

Here's a complete example:

```cs
using NUnit.Framework;

namespace ToyStore
{
   enum Taste { Bitter, Sweet };

   interface IWidget
   {
      int Weight { get; }
   }

   interface IFactory<out TWidget>
       where TWidget : IWidget
   {
      TWidget Create();
   }

   class Toy : IWidget
   {
      public int Weight { get; set; }
      public Taste Taste { get; set; }
   }

   class ToyFactory : IFactory<Toy>
   {
      public const int StandardWeight = 100;
      public const Taste StandardTaste = Taste.Sweet;

      public Toy Create() { return new Toy { Weight = StandardWeight, Taste = StandardTaste }; }
   }

   [TestFixture]
   public class GivenAToyFactory
   {
      [Test]
      public static void WhenUsingToyFactoryToMakeWidgets()
      {
         var toyFactory = new ToyFactory();

         //// Without out keyword, note the verbose explicit cast:
         // IFactory<IWidget> rustBeltFactory = (IFactory<IWidget>)toyFactory;

         // covariance: concrete being assigned to abstract (shiny and new)
         IFactory<IWidget> widgetFactory = toyFactory;
         IWidget anotherToy = widgetFactory.Create();
         Assert.That(anotherToy.Weight, Is.EqualTo(ToyFactory.StandardWeight)); // abstract contract
         Assert.That(((Toy)anotherToy).Taste, Is.EqualTo(ToyFactory.StandardTaste)); // concrete contract
      }
   }
}

```



## Contravariance


When is an `IComparer<T>` a subtype of a different `IComparer<T1>`? When `T1` is a subtype of `T`. `IComparer` is **contravariant** in its `T` parameter, which means that `IComparer`'s subtype relationship goes in the **opposite direction** as `T`'s.

```cs
class Animal { /* ... */ }
class Dog : Animal { /* ... */ }

IComparer<Animal> animalComparer = /* ... */;
IComparer<Dog> dogComparer = animalComparer;  // IComparer<Animal> is a subtype of IComparer<Dog>
// animalComparer = dogComparer;  // Compilation error - IComparer<Dog> is not a subtype of IComparer<Animal>

```

An instance of a contravariant generic type with a given type parameter is implicitly convertible to the same generic type with a more derived type parameter.

This relationship holds because `IComparer` **consumes** `T`s but doesn't produce them. An object which can compare any two `Animal`s can be used to compare two `Dog`s.

Contravariant type parameters are declared using the `in` keyword, because the parameter must be used only as an **input**.

```cs
interface IComparer<in T> { /* ... */ }

```

A type parameter declared as contravariant may not appear as an output.

```cs
interface Bad<in T>
{
    T GetT();  // type error
}

```



## Invariance


`IList<T>` is never a subtype of a different `IList<T1>`. `IList` is **invariant** in its type parameter.

```cs
class Animal { /* ... */ }
class Dog : Animal { /* ... */ }

IList<Dog> dogs = new List<Dog>();
IList<Animal> animals = dogs;  // type error

```

There is no subtype relationship for lists because you can put values into a list **and** take values out of a list.

If `IList` was covariant, you'd be able to add items of the **wrong subtype** to a given list.

```cs
IList<Animal> animals = new List<Dog>();  // supposing this were allowed...
animals.Add(new Giraffe());  // ... then this would also be allowed, which is bad!

```

If `IList` was contravariant, you'd be able to extract values of the wrong subtype from a given list.

```cs
IList<Dog> dogs = new List<Animal> { new Dog(), new Giraffe() };  // if this were allowed...
Dog dog = dogs[1];  // ... then this would be allowed, which is bad!

```

Invariant type parameters are declared by omitting both the `in` and `out` keywords.

```cs
interface IList<T> { /* ... */ }

```



## Variant interfaces


Interfaces may have variant type parameters.

```cs
interface IEnumerable<out T>
{
    // ...
}
interface IComparer<in T>
{
    // ...
}

```

but classes and structures may not

```cs
class BadClass<in T1, out T2>  // not allowed
{
}

struct BadStruct<in T1, out T2>  // not allowed
{
}

```

nor do generic method declarations

```cs
class MyClass
{
    public T Bad<out T, in T1>(T1 t1)  // not allowed
    {
        // ...
    }
}

```

The example below shows multiple variance declarations on the same interface

```cs
interface IFoo<in T1, out T2, T3>
//  T1 : Contravariant type
//  T2 : Covariant type 
//  T3 : Invariant type
{
    // ...
}

IFoo<Animal, Dog, int> foo1 = /* ... */;
IFoo<Dog, Animal, int> foo2 = foo1;  
// IFoo<Animal, Dog, int> is a subtype of IFoo<Dog, Animal, int>

```



## Checking equality of generic values.


If logic of generic class or method requires checking equality of values having generic type, use `EqualityComparer<TType>.Default` [property](https://msdn.microsoft.com/en-us/library/ms224763(v=vs.110).aspx):

```cs
public void Foo<TBar>(TBar arg1, TBar arg2)
{
    var comparer = EqualityComparer<TBar>.Default;
    if (comparer.Equals(arg1,arg2)
    {
        ...
    }
}

```

This approach is better than simply calling `Object.Equals()` method, because default comparer implementation checks, whether `TBar` type implements `IEquatale<TBar>` [interface](https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx) and if yes, calls `IEquatable<TBar>.Equals(TBar other)` method. This allows to avoid boxing/unboxing of value types.



## Type Parameters (Interfaces)


Declaration:

```cs
interface IMyGenericInterface<T1, T2, T3, ...> { ... }

```

Usage (in inheritance):

```cs
class ClassA<T1, T2, T3> : IMyGenericInterface<T1, T2, T3> { ... }

class ClassB<T1, T2> : IMyGenericInterface<T1, T2, int> { ... }

class ClassC<T1> : IMyGenericInterface<T1, char, int> { ... }

class ClassD : IMyGenericInterface<bool, char, int> { ... }

```

Usage (as the type of a parameter):

```cs
void SomeMethod(IMyGenericInterface<int, char, bool> arg) { ... }

```



## Variant delegates


Delegates may have variant type parameters.

```cs
delegate void Action<in T>(T t);    // T is an input
delegate T Func<out T>();           // T is an output
delegate T2 Func<in T1, out T2>();  // T1 is an input, T2 is an output

```

This follows from the [Liskov Substitution Principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle), which states (among other things) that a method D can be considered more derived than a method B if:

- D has an equal or more derived return type than B
- D has equal or more general corresponding parameter types than B

Therefore the following assignments are all type safe:

```cs
Func<object, string> original = SomeMethod;
Func<object, object> d1 = original;
Func<string, string> d2 = original;
Func<string, object> d3 = original;

```



## Variant types as parameters and return values


If a covariant type appears as an output, the containing type is covariant. Producing a producer of `T`s is like producing `T`s.

```cs
interface IReturnCovariant<out T>
{
    IEnumerable<T> GetTs();
}

```

If a contravariant type appears as an output, the containing type is contravariant. Producing a consumer of `T`s is like consuming `T`s.

```cs
interface IReturnContravariant<in T>
{
    IComparer<T> GetTComparer();
}

```

If a covariant type appears as an input, the containing type is contravariant. Consuming a producer of `T`s is like consuming `T`s.

```cs
interface IAcceptCovariant<in T>
{
    void ProcessTs(IEnumerable<T> ts);
}

```

If a contravariant type appears as an input, the containing type is covariant. Consuming a consumer of `T`s is like producing `T`s.

```cs
interface IAcceptContravariant<out T>
{
    void CompareTs(IComparer<T> tComparer);
}

```



## Type Parameters (Classes)


Declaration:

```cs
class MyGenericClass<T1, T2, T3, ...>
{
    // Do something with the type parameters.
}

```

Initialisation:

```cs
var x = new MyGenericClass<int, char, bool>();

```

Usage (as the type of a parameter):

```cs
void AnotherMethod(MyGenericClass<float, byte, char> arg) { ... }

```



## Type Parameters (Methods)


Declaration:

```cs
void MyGenericMethod<T1, T2, T3>(T1 a, T2 b, T3 c)
{
    // Do something with the type parameters.
}

```

Invocation:

There is no need to supply type arguements to a genric method, because the compiler can implicitly infer the type.

```cs
int x =10;
int y =20;
string z = "test";
MyGenericMethod(x,y,z);

```

However, if there is an ambiguity, generic methods need to be called with type arguemnts as

```cs
MyGenericMethod<int, int, string>(x,y,z);

```



## Type constraints (class and struct)


It is possible to specify whether or not the type argument should be a reference type or a value type by using the respective constraints `class` or `struct`. If these constraints are used, they **must** be defined **before all** other constraints (for example a parent type or `new()`) can be listed.

```cs
// TRef must be a reference type, the use of Int32, Single, etc. is invalid.
// Interfaces are valid, as they are reference types
class AcceptsRefType<TRef>
    where TRef : class
{
    // TStruct must be a value type.
    public void AcceptStruct<TStruct>()
        where TStruct : struct
    {
    }

    // If multiple constraints are used along with class/struct
    // then the class or struct constraint MUST be specified first
    public void Foo<TComparableClass>()
        where TComparableClass : class, IComparable
    {
    }
}

```



## Explicit type parameters


There are different cases where you must Explicitly specify the type parameters for a generic method. In both of the below cases, the compiler is not able to infer all of the type parameters from the specified method parameters.

One case is when there are no parameters:

```cs
public void SomeMethod<T, V>() 
{
   // No code for simplicity
}

SomeMethod(); // doesn't compile
SomeMethod<int, bool>(); // compiles

```

Second case is when one (or more) of the type parameters is not part of the method parameters:

```cs
public K SomeMethod<K, V>(V input)
{
    return default(K);
}

int num1 = SomeMethod(3); // doesn't compile
int num2 = SomeMethod<int>("3"); // doesn't compile
int num3 = SomeMethod<int, string>("3"); // compiles.

```



## Generic type casting


```cs

   /// <summary>
    /// Converts a data type to another data type.
    /// </summary>
    public static class Cast
    {
        /// <summary>
        /// Converts input to Type of default value or given as typeparam T
        /// </summary>
        /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
        /// <param name="input">Input that need to be converted to specified type</param>
        /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
        /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
        public static T To<T>(object input, T defaultValue)
        {
            var result = defaultValue;
            try
            {
                if (input == null || input == DBNull.Value) return result;
                if (typeof (T).IsEnum)
                {
                    result = (T) Enum.ToObject(typeof (T), To(input, Convert.ToInt32(defaultValue)));
                }
                else
                {
                    result = (T) Convert.ChangeType(input, typeof (T));
                }
            }
            catch (Exception ex)
            {
                Tracer.Current.LogException(ex);
            }

            return result;
        }
        
        /// <summary>
        /// Converts input to Type of typeparam T
        /// </summary>
        /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
        /// <param name="input">Input that need to be converted to specified type</param>
        /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
        public static T To<T>(object input)
        {
            return To(input, default(T));
        }

        

    }

```

Usages:

```cs
std.Name = Cast.To<string>(drConnection["Name"]);
std.Age = Cast.To<int>(drConnection["Age"]);
std.IsPassed = Cast.To<bool>(drConnection["IsPassed"]);


// Casting type using default value
//Following both ways are correct
// Way 1 (In following style input is converted into type of default value)
std.Name = Cast.To(drConnection["Name"], "");
std.Marks = Cast.To(drConnection["Marks"], 0);
// Way 2    
std.Name = Cast.To<string>(drConnection["Name"], "");
std.Marks = Cast.To<int>(drConnection["Marks"], 0);

```



## Configuration reader with generic type casting


```cs

   /// <summary>
    /// Read configuration values from app.config and convert to specified types
    /// </summary>
    public static class ConfigurationReader
    {
        /// <summary>
        /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
        /// </summary>
        /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
        /// <param name="strKey">key to find value from AppSettings</param>
        /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
        /// <returns>AppSettings value against key is returned in Type of default value or given as typeparam T</returns>
        public static T GetConfigKeyValue<T>(string strKey, T defaultValue)
        {
            var result = defaultValue;
            try
            {
                if (ConfigurationManager.AppSettings[strKey] != null)
                    result = (T)Convert.ChangeType(ConfigurationManager.AppSettings[strKey], typeof(T));
            }
            catch (Exception ex)
            {
                Tracer.Current.LogException(ex);
            }

            return result;
        }
        /// <summary>
        /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
        /// </summary>
        /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
        /// <param name="strKey">key to find value from AppSettings</param>
        /// <returns>AppSettings value against key is returned in Type given as typeparam T</returns>
        public static T GetConfigKeyValue<T>(string strKey)
        {
            return GetConfigKeyValue(strKey, default(T));
        }

    }

```

Usages:

```cs
var timeOut = ConfigurationReader.GetConfigKeyValue("RequestTimeout", 2000);
var url = ConfigurationReader.GetConfigKeyValue("URL", "www.someurl.com");
var enabled = ConfigurationReader.GetConfigKeyValue("IsEnabled", false);

```



#### Syntax


- `public void SomeMethod <T> () { }`
- `public void SomeMethod<T, V>() { }`
- `public T SomeMethod<T>(IEnumerable<T> sequence) { ... }`
- `public void SomeMethod<T>() where T : new() { }`
- `public void SomeMethod<T, V>() where T : new() where V : struct { }`
- `public void SomeMethod<T>() where T: IDisposable { }`
- `public void SomeMethod<T>() where T: Foo { }`
- `public class MyClass<T> { public T Data {get; set; } }`



#### Parameters


|Parameter(s)|Description
|------
|T, V|Type placeholders for generic declarations



#### Remarks


Generics in C# are supported all the way down to the runtime: generic types built with C# will have their generic semantics preserved even after compiled to [CIL](https://en.wikipedia.org/wiki/Common_Intermediate_Language).

This effectively means that, in C#, it is possible to reflect on generic types and see them as they were declared or check if an object is an instance of a generic type, for example. This is in contrast with [type erasure](https://en.wikipedia.org/wiki/Type_erasure), where generic type information is removed during compilation. It is also in contrast with the template approach to generics, where multiple concrete generic types become multiple non-generic types at runtime, and any metadata required to further instantiate the original generic type definitions is lost.

Be careful, however, when reflecting on generic types: generic types' names will be altered on compilation, substituting the angled brackets and the type parameters' names by a backtick followed by the number of generic type parameters. Thus, a `Dictionary<TKey, Tvalue>` will be translated to `Dictionary`2`.

