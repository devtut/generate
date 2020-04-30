---
metaTitle: "Constructors and Finalizers"
description: "Static constructor, Singleton constructor pattern, Default Constructor, Forcing a static constructor to be called, Calling a constructor from another constructor, Calling the base class constructor, Finalizers on derived classes, Exceptions in static constructors, Calling virtual methods in constructor, Generic Static Constructors, Constructor and Property Initialization"
---

# Constructors and Finalizers


Constructors are methods in a class that are invoked when an instance of that class is created. Their main responsibility is to leave the new object in a useful and consistent state.

Destructors/Finalizers are methods in a class that are invoked when an instance of that is destroyed. In C# they are rarely explicitely written/used.



## Static constructor


A static constructor is called the first time any member of a type is initialized, a static class member is called  or a static method.
The static constructor is thread safe.
A static constructor is commonly used to:

- Initialize static state, that is state which is shared across different instances of the same class.
- Create a singleton

**Example:**

```cs
class Animal
{
    // * A static constructor is executed only once,
    //   when a class is first accessed.
    // * A static constructor cannot have any access modifiers
    // * A static constructor cannot have any parameters
    static Animal()
    {
        Console.WriteLine("Animal initialized");
    }

    // Instance constructor, this is executed every time the class is created
    public Animal()
    {
        Console.WriteLine("Animal created");
    }

    public static void Yawn()
    {
        Console.WriteLine("Yawn!");
    }
}

var turtle = new Animal();
var giraffe = new Animal();

```

**Output:**

> 
<p>Animal initialized<br />
Animal created<br />
Animal created</p>


[View Demo](https://dotnetfiddle.net/XmExII)

If the first call is to a static method, the static constructor is invoked without the instance constructor. This is OK, because the static method can't access instance state anyways.

```cs
Animal.Yawn();

```

This will output:

> 
<p>Animal initialized<br />
Yawn!</p>


See also [Exceptions in static constructors](http://stackoverflow.com/documentation/c%23/25/constructors-finalizers/15007/exceptions-in-static-constructors) and [Generic Static Constructors](http://stackoverflow.com/documentation/c%23/25/constructors-finalizers/15003/generic-static-constructors) .

Singleton example:

```cs
public class SessionManager
{
    public static SessionManager Instance;

    static SessionManager()
    {
        Instance = new SessionManager();
    }
}

```



## Singleton constructor pattern


```cs
public class SingletonClass
{
    public static SingletonClass Instance { get; } = new SingletonClass();

    private SingletonClass()
    {
        // Put custom constructor code here
    }    
}

```

Because the constructor is private, no new instances of `SingletonClass` can be made by consuming code. The only way to access the single instance of `SingletonClass` is by using the static property `SingletonClass.Instance`.

The `Instance` property is assigned by a static constructor that the C# compiler generates. The .NET runtime guarantees that the static constructor is run at most once and is run before `Instance` is first read. Therefore, all synchronization and initialization concerns are carried out by the runtime.

Note, that if the static constructor fails the `Singleton` class becomes permanently unusable for the life of the AppDomain.

Also, the static constructor is not guaranteed to run at the time of the first access of `Instance`. Rather, it will run **at some point before that**. This makes the time at which initialization happens non-deterministic. In practical cases the JIT often calls the static constructor during **compilation** (not execution) of a method referencing `Instance`. This is a performance optimization.

See the [Singleton Implementations](http://stackoverflow.com/documentation/c%23/1192/singleton-implementation#t=201607231143190778053) page for other ways to implement the singleton pattern.



## Default Constructor


When a type is defined without a constructor:

```cs
public class Animal
{
}

```

then the compiler generates a default constructor equivalent to the following:

```cs
public class Animal
{
    public Animal() {}
}

```

The definition of any constructor for the type will suppress the default constructor generation. If the type were defined as follows:

```cs
public class Animal
{
    public Animal(string name) {}
}

```

then an `Animal` could only be created by calling the declared constructor.

```cs
// This is valid
var myAnimal = new Animal("Fluffy");
// This fails to compile
var unnamedAnimal = new Animal();

```

For the second example, the compiler will display an error message:

> 
'Animal' does not contain a constructor that takes 0 arguments


If you want a class to have both a parameterless constructor and a constructor that takes a parameter, you can do it by explicitly implementing both constructors.

```cs
public class Animal
{
    
    public Animal() {} //Equivalent to a default constructor.
    public Animal(string name) {}
}

```

The compiler will not be able to generate a default constructor if the class extends another class which doesn't have a parameterless constructor. For example, if we had a class `Creature`:

```cs
public class Creature
{
    public Creature(Genus genus) {}
}

```

then `Animal` defined as `class Animal : Creature {}` would not compile.



## Forcing a static constructor to be called


While static constructors are always called before the first usage of a type it's sometimes useful to be able to force them to be called and the `RuntimeHelpers` class provide an helper for it:

```cs
using System.Runtime.CompilerServices;    
// ...
RuntimeHelpers.RunClassConstructor(typeof(Foo).TypeHandle);

```

****Remark**:** All static initialization (fields initializers for example) will run, not only the constructor itself.

****Potential usages**:** Forcing initialization during the splash screen in an UI application or ensuring that a static constructor doesn't fail in an unit test.



## Calling a constructor from another constructor


```cs
public class Animal
{
    public string Name { get; set; }

    public Animal() : this("Dog")
    {
    }

    public Animal(string name)
    {
        Name = name;
    }
}

var dog = new Animal();      // dog.Name will be set to "Dog" by default.
var cat = new Animal("Cat"); // cat.Name is "Cat", the empty constructor is not called.

```



## Calling the base class constructor


A constructor of a base class is called before a constructor of a derived class is executed. For example, if `Mammal` extends `Animal`, then the code contained in the constructor of `Animal` is called first when creating an instance of a `Mammal`.

If a derived class doesn't explicitly specify which constructor of the base class should be called, the compiler assumes the parameterless constructor.

```cs
public class Animal
{
    public Animal() { Console.WriteLine("An unknown animal gets born."); }
    public Animal(string name) { Console.WriteLine(name + " gets born"); }
}

public class Mammal : Animal
{
    public Mammal(string name)
    {
        Console.WriteLine(name + " is a mammal.");
    }
}

```

In this case, instantiating a `Mammal` by calling `new Mammal("George the Cat")` will print

> 
<p>An unknown animal gets born.<br />
George the Cat is a mammal.</p>


[View Demo](https://dotnetfiddle.net/xb8Vqr)

Calling a different constructor of the base class is done by placing `: base(args)` between the constructor's signature and its body:

```cs
public class Mammal : Animal
{
    public Mammal(string name) : base(name)
    {
        Console.WriteLine(name + " is a mammal.");
    }
}

```

Calling `new Mammal("George the Cat")` will now print:

> 
<p>George the Cat gets born.<br />
George the Cat is a mammal.</p>


[View Demo](https://dotnetfiddle.net/gbdERq)



## Finalizers on derived classes


When an object graph is finalized, the order is the reverse of the construction.  E.g. the super-type is finalized before the base-type as the following code demonstrates:

```cs
class TheBaseClass
{
    ~TheBaseClass() 
    {
        Console.WriteLine("Base class finalized!");
    }
}

class TheDerivedClass : TheBaseClass
{
    ~TheDerivedClass() 
    {
        Console.WriteLine("Derived class finalized!");
    }
}

//Don't assign to a variable
//to make the object unreachable
new TheDerivedClass();

//Just to make the example work;
//this is otherwise NOT recommended!
GC.Collect();

//Derived class finalized!
//Base class finalized!

```



## Exceptions in static constructors


If a static constructor throws an exception, it is never retried. The type is unusable for the lifetime of the AppDomain. Any further usages of the type will raise a `TypeInitializationException` wrapped around the original exception.

```cs
public class Animal
{
    static Animal()
    {
        Console.WriteLine("Static ctor");
        throw new Exception();
    }

    public static void Yawn() {}
}

try
{
    Animal.Yawn();
}
catch (Exception e)
{
    Console.WriteLine(e.ToString());
}

try
{
    Animal.Yawn();
}
catch (Exception e)
{
    Console.WriteLine(e.ToString());
}

```

This will output:

> 
Static ctor
<p>System.TypeInitializationException: The type initializer
for 'Animal' threw an exception. ---> System.Exception: Exception of
type 'System.Exception' was thrown.</p>


[...]

> 
<p>System.TypeInitializationException: The type initializer for 'Animal'
threw an exception. ---> System.Exception: Exception of type
'System.Exception' was thrown.</p>


where you can see that the actual constructor is only executed once, and the exception is re-used.



## Calling virtual methods in constructor


Unlike C++ in C# you can call a virtual method from class constructor (OK, you can also in C++ but behavior at first is surprising). For example:

```cs
abstract class Base
{
    protected Base()
    {
        _obj = CreateAnother();
    }

    protected virtual AnotherBase CreateAnother()
    {
        return new AnotherBase();
    }

    private readonly AnotherBase _obj;
}

sealed class Derived : Base
{
    public Derived() { }

    protected override AnotherBase CreateAnother()
    {
        return new AnotherDerived();
    }
}

var test = new Derived();
// test._obj is AnotherDerived

```

If you come from a C++ background this is surprising, base class constructor already sees derived class virtual method table!

**Be careful**: derived class may not been fully initialized yet (its constructor will be executed after base class constructor) and this technique is dangerous (there is also a StyleCop warning for this). Usually this is regarded as bad practice.



## Generic Static Constructors


If the type on which the static constructor is declared is generic, the static constructor will be called once for each unique combination of generic arguments.

```cs
class Animal<T>
{
    static Animal()
    {
        Console.WriteLine(typeof(T).FullName);
    }

    public static void Yawn() { }
}

Animal<Object>.Yawn();
Animal<String>.Yawn();

```

This will output:

> 
<p>System.Object<br />
System.String</p>


See also [How do static constructors for generic types work ?](http://stackoverflow.com/q/5629388)



## Constructor and Property Initialization


Shall the property value's assignment be executed **before** or **after** the class' constructor?

```cs
public class TestClass 
{
    public int TestProperty { get; set; } = 2;
    
    public TestClass() 
    {
        if (TestProperty == 1) 
        {
            Console.WriteLine("Shall this be executed?");
        }

        if (TestProperty == 2) 
        {
            Console.WriteLine("Or shall this be executed");
        }
    }
}

var testInstance = new TestClass() { TestProperty = 1 };

```

In the example above, shall the `TestProperty` value be `1` in the class' constructor or after the class constructor?

Assigning property values in the instance creation like this:

```cs
var testInstance = new TestClass() {TestProperty = 1};

```

Will be executed ****after**** the constructor is run. However, initializing the property value in the class' property in C# 6.0 like this:

```cs
public class TestClass 
{
    public int TestProperty { get; set; } = 2;

    public TestClass() 
    {
    }
}

```

will be done ****before**** the constructor is run.

Combining the two concepts above in a single example:

```cs
public class TestClass 
{
    public int TestProperty { get; set; } = 2;
    
    public TestClass() 
    {
        if (TestProperty == 1) 
        {
            Console.WriteLine("Shall this be executed?");
        }

        if (TestProperty == 2) 
        {
            Console.WriteLine("Or shall this be executed");
        }
    }
}

static void Main(string[] args) 
{
    var testInstance = new TestClass() { TestProperty = 1 };
    Console.WriteLine(testInstance.TestProperty); //resulting in 1
}

```

Final result:

```cs
"Or shall this be executed"
"1"

```

**Explanation:**

The `TestProperty` value will first be assigned as `2`, then the `TestClass` constructor will be run, resulting in printing of

```cs
"Or shall this be executed"

```

And then the `TestProperty` will be assigned as `1` due to `new TestClass() { TestProperty = 1 }`, making the final value for the `TestProperty` printed by `Console.WriteLine(testInstance.TestProperty)` to be

```cs
"1"

```



#### Remarks


C# does not actually have destructors, but rather Finalizers which use C++ style destructor syntax. Specifying a destructor overrides the `Object.Finalize()` method which cannot be called directly.

Unlike other languages with similar syntax, these methods are **not** called when objects go out of scope, but are called when the Garbage Collector runs, which occurs [under certain conditions](https://msdn.microsoft.com/en-us/library/ee787088(v=vs.110).aspx#conditions_for_a_garbage_collection). As such, they are **not** guaranteed to run in any particular order.

Finalizers should be responsible for cleaning up unmanaged resources **only** (pointers acquired via the Marshal class, received through p/Invoke (system calls) or raw pointers used within unsafe blocks). To clean up managed resources, please review IDisposable, the Dispose pattern and the [`using`](http://stackoverflow.com/documentation/c%23/38/using-statement) statement.

(Further reading: [When should I create a destructor?](http://stackoverflow.com/a/4899622))

