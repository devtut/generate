---
metaTitle: "Inheritance"
description: "Inheritance. Constructors' calls sequence, Inheriting from a base class, Inheriting from a class and implementing an interface, Inheriting from a class and implementing multiple interfaces, Testing and navigating inheritance, Extending an abstract base class, Constructors In A Subclass, Inheritance Anti-patterns, Inheriting methods, Base class with recursive type specification"
---

# Inheritance




## Inheritance. Constructors' calls sequence


Consider we have a class `Animal` which has a child class `Dog`

```cs
class Animal
{
    public Animal()
    {
        Console.WriteLine("In Animal's constructor");
    }
}

class Dog : Animal
{
    public Dog()
    {
        Console.WriteLine("In Dog's constructor");
    }
}

```

By default every class implicitly inherits the `Object` class.

This is same as the above code.

```cs
class Animal : Object
{
    public Animal()
    {
        Console.WriteLine("In Animal's constructor");
    }
}

```

When creating an instance of `Dog` class, the **base classes's default constructor (without parameters) will be called if there is no explicit call to another constructor in the parent class**.  In our case, first will be called `Object's` constructor, then `Animal's` and at the end `Dog's` constructor.

```cs
public class Program
{
    public static void Main()
    {
        Dog dog = new Dog();
    }
}

```

Output will be

> 
<p>In Animal's constructor<br />
In Dog's constructor</p>


[View Demo](https://dotnetfiddle.net/uOL8cE)

**Call parent's constructor explicitly.**

In the above examples, our `Dog` class constructor calls the **default** constructor of the `Animal` class. If you want, you can specify which constructor should be called: it is possible to call any constructor which is defined in the parent class.

Consider we have these two classes.

```cs
class Animal
{
    protected string name;

    public Animal()
    {
        Console.WriteLine("Animal's default constructor");
    }    

    public Animal(string name)
    {
        this.name = name;
        Console.WriteLine("Animal's constructor with 1 parameter");
        Console.WriteLine(this.name);
    }
}

class Dog : Animal
{
    public Dog() : base()
    {
        Console.WriteLine("Dog's default constructor");
    }  

    public Dog(string name) : base(name)
    {
        Console.WriteLine("Dog's constructor with 1 parameter");
        Console.WriteLine(this.name);
    }
}

```

**What is going here?**

We have 2 constructors in each class.

**What does `base` mean?**

`base` is a reference to the parent class. In our case, when we create an instance of `Dog` class like this

```cs
Dog dog = new Dog();

```

The runtime first calls the `Dog()`, which is the parameterless constructor. But its body doesn't work immediately. After the parentheses of the constructor we have a such call: `base()`, which means that when we call the default `Dog` constructor, it will in turn call the parent's **default** constructor. After the parent's constructor runs, it will return and then, finally, run the `Dog()` constructor body.

So output will be like this:

> 
<p>Animal's default constructor<br />
Dog's default constructor</p>


[View Demo](https://dotnetfiddle.net/eRKEjT)

**Now what if we call the `Dog's` constructor with a parameter?**

```cs
Dog dog = new Dog("Rex");

```

You know that members in the parent class which are not private are inherited by the child class, meaning that `Dog` will also have the `name` field.<br />
In this case we passed an argument to our constructor. It in his turn passes the argument to the parent class' **constructor with a parameter**, which initializes the `name` field.

Output will be

```cs
Animal's constructor with 1 parameter
Rex
Dog's constructor with 1 parameter
Rex

```

**Summary:**

Every object creation starts from the base class. In the inheritance, the classes which are in the hierarchy are chained. As all classes derive from `Object`, the first constructor to be called when any object is created is the `Object` class constructor; Then the next constructor in the chain is called and only after all of them are called the object is created

**base keyword**

1. The base keyword is used to access members of the base class from within a derived class:
<li>Call a method on the base class that has been overridden by another method.
Specify which base-class constructor should be called when creating instances of the derived class.</li>



## Inheriting from a base class


To avoid duplicating code, define common methods and attributes in a general class as a base:

```cs
public class Animal 
{
    public string Name { get; set; }
    // Methods and attributes common to all animals
    public void Eat(Object dinner)
    {
        // ...
    }
    public void Stare()
    {
        // ...
    }
    public void Roll()
    {
        // ...
    }
}

```

Now that you have a class that represents `Animal` in general, you can define a class that describes the peculiarities of specific animals:

```cs
public class Cat : Animal
{
    public Cat() 
    {
        Name = "Cat";
    }
    // Methods for scratching furniture and ignoring owner
    public void Scratch(Object furniture)
    {
        // ...
    }
}

```

The Cat class gets access to not only the methods described in its definition explicitly, but also all the methods defined in the general `Animal` base class. Any Animal (whether or not it was a Cat) could Eat, Stare, or Roll. An Animal would not be able to Scratch, however, unless it was also a Cat. You could then define other classes describing other animals. (Such as Gopher with a method for destroying flower gardens and Sloth with no extra methods at all.)



## Inheriting from a class and implementing an interface


```cs
public class Animal 
{
    public string Name { get; set; }
}

public interface INoiseMaker
{
    string MakeNoise();
}

//Note that in C#, the base class name must come before the interface names
public class Cat : Animal, INoiseMaker
{
    public Cat() 
    {
        Name = "Cat";
    }

    public string MakeNoise()
    {
        return "Nyan";
    }
}

```



## Inheriting from a class and implementing multiple interfaces


```cs
public class LivingBeing
{
    string Name { get; set; }
}

public interface IAnimal 
{
    bool HasHair { get; set; }
}

public interface INoiseMaker
{
    string MakeNoise();
}

//Note that in C#, the base class name must come before the interface names
public class Cat : LivingBeing, IAnimal, INoiseMaker
{
    public Cat() 
    {
        Name = "Cat";
        HasHair = true;
    }

    public bool HasHair { get; set; }

    public string Name { get; set; }

    public string MakeNoise()
    {
        return "Nyan";
    }
}

```



## Testing and navigating inheritance


```cs
interface BaseInterface {}
class BaseClass : BaseInterface {}

interface DerivedInterface {}
class DerivedClass : BaseClass, DerivedInterface {}

var baseInterfaceType = typeof(BaseInterface);
var derivedInterfaceType = typeof(DerivedInterface);
var baseType = typeof(BaseClass);
var derivedType = typeof(DerivedClass);

var baseInstance = new BaseClass();
var derivedInstance = new DerivedClass();  

Console.WriteLine(derivedInstance is DerivedClass); //True
Console.WriteLine(derivedInstance is DerivedInterface); //True
Console.WriteLine(derivedInstance is BaseClass); //True
Console.WriteLine(derivedInstance is BaseInterface); //True
Console.WriteLine(derivedInstance is object); //True

Console.WriteLine(derivedType.BaseType.Name);  //BaseClass
Console.WriteLine(baseType.BaseType.Name);  //Object
Console.WriteLine(typeof(object).BaseType);  //null

Console.WriteLine(baseType.IsInstanceOfType(derivedInstance));  //True
Console.WriteLine(derivedType.IsInstanceOfType(baseInstance));  //False

Console.WriteLine(
    string.Join(",", 
    derivedType.GetInterfaces().Select(t => t.Name).ToArray()));
//BaseInterface,DerivedInterface
    
Console.WriteLine(baseInterfaceType.IsAssignableFrom(derivedType)); //True
Console.WriteLine(derivedInterfaceType.IsAssignableFrom(derivedType)); //True
Console.WriteLine(derivedInterfaceType.IsAssignableFrom(baseType)); //False

```



## Extending an abstract base class


Unlike interfaces, which can be described as contracts for implementation, abstract classes act as contracts for extension.

An abstract class cannot be instantiated, it must be extended and the resulting class (or derived class) can then be instantiated.

Abstract classes are used to provide generic implementations

```cs
public abstract class Car
{
    public void HonkHorn() {
        // Implementation of horn being honked
    }
}

public class Mustang : Car
{
    // Simply by extending the abstract class Car, the Mustang can HonkHorn()
    // If Car were an interface, the HonkHorn method would need to be included
    // in every class that implemented it.
}

```

The above example shows how any class extending Car will automatically receive the HonkHorn method with the implementation. This means that any developer creating a new Car will not need to worry about how it will honk it's horn.



## Constructors In A Subclass


When you make a subclass of a base class, you can construct the base class by using `: base` after the subclass constructor's parameters.

```cs
class Instrument
{
    string type;
    bool clean;

    public Instrument (string type, bool clean)
    {
        this.type = type;
        this.clean = clean;
    }
}

class Trumpet : Instrument
{
    bool oiled;

    public Trumpet(string type, bool clean, bool oiled) : base(type, clean)
    {
        this.oiled = oiled;
    }
}

```



## Inheritance Anti-patterns


### Improper Inheritance

Lets say there are 2 classes class `Foo` and `Bar`. `Foo` has two features `Do1` and `Do2`. `Bar` needs to use `Do1` from `Foo`, but it doesn't need `Do2` or needs feature that is equivalent to `Do2` but does something completely different.

**Bad way**: make `Do2()` on `Foo` virtual then override it in `Bar` or just `throw Exception` in `Bar` for `Do2()`

```cs
public class Bar : Foo
{
    public override void Do2()
    {
        //Does something completely different that you would expect Foo to do
        //or simply throws new Exception 
    }
}

```

**Good way**

Take out `Do1()` from `Foo` and put it into new class `Baz` then inherit both `Foo` and `Bar` from `Baz` and implement `Do2()` separately

```cs
public class Baz
{
    public void Do1()
    {
        // magic
    }
}

public class Foo : Baz
{
    public void Do2()
    {
        // foo way
    }
}

public class Bar : Baz
{
    public void Do2()
    {
        // bar way or not have Do2 at all
    }
}

```

Now why first example is bad and second is good: When developer nr2 has to do a change in `Foo`, chances are he will break implementation of `Bar` because `Bar` is now inseparable from `Foo`. When doing it by latter example `Foo` and `Bar` commonalty has been moved to `Baz` and they do not affect each other (like the shouldn't).



## Inheriting methods


There are several ways methods can be inherited

```cs
public abstract class Car
{
    public void HonkHorn() {
        // Implementation of horn being honked
    }

    // virtual methods CAN be overridden in derived classes
    public virtual void ChangeGear() {
        // Implementation of gears being changed
    }

    // abstract methods MUST be overridden in derived classes
    public abstract void Accelerate();
}

public class Mustang : Car
{
    // Before any code is added to the Mustang class, it already contains 
    // implementations of HonkHorn and ChangeGear.

    // In order to compile, it must be given an implementation of Accelerate,
    // this is done using the override keyword
    public override void Accelerate() {
        // Implementation of Mustang accelerating
    }

    // If the Mustang changes gears differently to the implementation in Car
    // this can be overridden using the same override keyword as above
    public override void ChangeGear() {
        // Implementation of Mustang changing gears
    }
}

```



## Base class with recursive type specification


One time definition of a generic base class with recursive type specifier. Each node has one parent and multiple children.

```cs
/// <summary>
/// Generic base class for a tree structure
/// </summary>
/// <typeparam name="T">The node type of the tree</typeparam>
public abstract class Tree<T> where T : Tree<T>
{
    /// <summary>
    /// Constructor sets the parent node and adds this node to the parent's child nodes
    /// </summary>
    /// <param name="parent">The parent node or null if a root</param>
    protected Tree(T parent)
    {
        this.Parent=parent;
        this.Children=new List<T>();
        if(parent!=null)
        {
            parent.Children.Add(this as T);
        }
    }
    public T Parent { get; private set; }
    public List<T> Children { get; private set; }
    public bool IsRoot { get { return Parent==null; } }
    public bool IsLeaf { get { return Children.Count==0; } }
    /// <summary>
    /// Returns the number of hops to the root object
    /// </summary>
    public int Level { get { return IsRoot ? 0 : Parent.Level+1; } }
}

```

The above can be re-used every time a tree hierarchy of objects needs to be defined. The node object in the tree has to inherit from the base class with

```cs
public class MyNode : Tree<MyNode>
{
    // stuff
}

```

each node class knows where it is in the hierarchy, what the parent object is as well as what the children objects are. Several built in types use a tree structure, like `Control` or `XmlElement` and the above `Tree<T>` can be used as a base class of **any** type in your code.

For example, to create a hierarchy of parts where the total weight is calculated from the weight of **all** the children, do the following:

```cs
public class Part : Tree<Part>
{
    public static readonly Part Empty = new Part(null) { Weight=0 };
    public Part(Part parent) : base(parent) { }
    public Part Add(float weight)
    {
        return new Part(this) { Weight=weight };
    }
    public float Weight { get; set; }

    public float TotalWeight { get { return Weight+Children.Sum((part) => part.TotalWeight); } }
}

```

to be used as

```cs
// [Q:2.5] -- [P:4.2] -- [R:0.4]
//    \
//      - [Z:0.8]
var Q = Part.Empty.Add(2.5f);
var P = Q.Add(4.2f);
var R = P.Add(0.4f);
var Z = Q.Add(0.9f);

// 2.5+(4.2+0.4)+0.9 = 8.0
float weight = Q.TotalWeight;

```

Another example would in the definition of relative coordinate frames. In this case the true position of the coordinate frame depends on the positions of **all** the parent coordinate frames.

```cs
public class RelativeCoordinate : Tree<RelativeCoordinate>
{
    public static readonly RelativeCoordinate Start = new RelativeCoordinate(null, PointF.Empty) { };
    public RelativeCoordinate(RelativeCoordinate parent, PointF local_position)
        : base(parent)
    {
        this.LocalPosition=local_position;
    }
    public PointF LocalPosition { get; set; }
    public PointF GlobalPosition
    {
        get
        {
            if(IsRoot) return LocalPosition;
            var parent_pos = Parent.GlobalPosition;
            return new PointF(parent_pos.X+LocalPosition.X, parent_pos.Y+LocalPosition.Y);
        }
    }
    public float TotalDistance
    {
        get
        {
            float dist = (float)Math.Sqrt(LocalPosition.X*LocalPosition.X+LocalPosition.Y*LocalPosition.Y);
            return IsRoot ? dist : Parent.TotalDistance+dist;
        }
    }
    public RelativeCoordinate Add(PointF local_position)
    {
        return new RelativeCoordinate(this, local_position);
    }
    public RelativeCoordinate Add(float x, float y)
    {
        return Add(new PointF(x, y));
    }
}

```

to be used as

```cs
// Define the following coordinate system hierarchy
//
// o--> [A1] --+--> [B1] -----> [C1]
//             |     
//             +--> [B2] --+--> [C2]
//                         |
//                         +--> [C3]

var A1 = RelativeCoordinate.Start;
var B1 = A1.Add(100, 20);
var B2 = A1.Add(160, 10);

var C1 = B1.Add(120, -40);
var C2 = B2.Add(80, -20);
var C3 = B2.Add(60, -30);

double dist1 = C1.TotalDistance;

```



#### Syntax


- class DerivedClass : BaseClass
- class DerivedClass : BaseClass, IExampleInterface
- class DerivedClass : BaseClass, IExampleInterface, IAnotherInterface



#### Remarks


Classes can inherit directly from only one class, but (instead or at the same time) can implement one or more interfaces.

Structs can implement interfaces but cannot explicitly inherit from any type. They implicitly inherit from `System.ValueType`, which in turn inherits directly from `System.Object`.

Static classes [cannot](http://stackoverflow.com/a/259079) implement interfaces.

