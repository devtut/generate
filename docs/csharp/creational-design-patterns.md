---
metaTitle: "C# | Creational Design Patterns"
description: "Singleton Pattern, Factory Method pattern, Abstract Factory Pattern, Builder Pattern, Prototype Pattern"
---

# Creational Design Patterns



## Singleton Pattern


The Singleton pattern is designed to restrict creation of a class to exactly one single instance.

This pattern is used in a scenario where it makes sense to have only one of something, such as:

- a single class that orchestrates other objects' interactions, ex. Manager class
- or one class that represents a unique, single resource, ex. Logging component

One of the most common ways to implement the Singleton pattern is via a static **factory method** such as a `CreateInstance()` or `GetInstance()` (or a static property in C#, `Instance`), which is then designed to always return the same instance.

The first call to the method or property creates and returns the Singleton instance. Thereafter, the method always returns the same instance. This way, there is only ever one instance of the singleton object.

Preventing creation of instances via `new` can be accomplished by making the class constructor(s) `private.`

Here is a typical code example for implementing a Singleton pattern in C#:

```cs
class Singleton
{
    // Because the _instance member is made private, the only way to get the single 
    // instance is via the static Instance property below. This can also be similarly 
    // achieved with a GetInstance() method instead of the property.
    private static Singleton _instance = null;

    // Making the constructor private prevents other instances from being 
    // created via something like Singleton s = new Singleton(), protecting
    // against unintentional misuse.
    private Singleton()
    {
    }

    public static Singleton Instance
    {
        get 
        {
            // The first call will create the one and only instance.
            if (_instance == null)
            {
                _instance = new Singleton();
            }

            // Every call afterwards will return the single instance created above.
            return _instance;
        }
    }
}

```

To illustrate this pattern further, the code below checks whether an identical instance of the Singleton is returned when the Instance property is called more than once.

```cs
class Program
{
    static void Main(string[] args)
    {
        Singleton s1 = Singleton.Instance;
        Singleton s2 = Singleton.Instance;
        
        // Both Singleton objects above should now reference the same Singleton instance.
        if (Object.ReferenceEquals(s1, s2))
        {
            Console.WriteLine("Singleton is working");
        }
        else
        {
            // Otherwise, the Singleton Instance property is returning something 
            // other than the unique, single instance when called.
            Console.WriteLine("Singleton is broken");
        }
    }
}

```

Note: this implementation is not thread safe.

To see more examples, including how to make this thread-safe, visit:
[Singleton Implementation](http://stackoverflow.com/documentation/c%23/1192/singleton-implementation#t=201608010510155689796)

Singletons are conceptually similar to a global value, and cause similar design flaws and concerns. Because of this, the Singleton pattern is widely regarded as an anti-pattern.

Visit ["What is so bad about Singletons?"](http://stackoverflow.com/questions/137975/what-is-so-bad-about-singletons) for more information on the problems that arise with their use.

In C#, you have the ability to make a class `static`, which makes all members static, and the class cannot be instantiated. Given this, it is common to see static classes used in place of the Singleton pattern.

For key differences between the two, visit [C# Singleton Pattern Versus Static Class](http://www.dotnetperls.com/singleton-static).



## Factory Method pattern


Factory Method is one of creational design patterns. It is used to deal with the problem of creating objects without specifying exact result type. This document will teach you how to use Factory Method DP properly.

Let me explain the idea of it to you on a simple example. Imagine you're working in a factory that produces three types of devices - Ammeter, Voltmeter and resistance meter. You are writing a program for a central computer that will create selected device, but you don't know final decision of your boss on what to produce.

Let's create an interface `IDevice` with some common functions that all devices have:

```cs
public interface IDevice
{
    int Measure();
    void TurnOff();
    void TurnOn();
}

```

Now, we can create classes that represent our devices. Those classes must implement `IDevice` interface:

```cs
public class AmMeter : IDevice
{
    private Random r = null;
    public AmMeter()
    {
        r = new Random();
    }
    public int Measure() { return r.Next(-25, 60); }
    public void TurnOff() { Console.WriteLine("AmMeter flashes lights saying good bye!"); }
    public void TurnOn() { Console.WriteLine("AmMeter turns on..."); }
}
public class OhmMeter : IDevice
{
    private Random r = null;
    public OhmMeter()
    {
        r = new Random();
    }
    public int Measure() { return r.Next(0, 1000000); }
    public void TurnOff() { Console.WriteLine("OhmMeter flashes lights saying good bye!"); }
    public void TurnOn() { Console.WriteLine("OhmMeter turns on..."); }
}
public class VoltMeter : IDevice
{
    private Random r = null;
    public VoltMeter()
    {
        r = new Random();
    }
    public int Measure() { return r.Next(-230, 230); }
    public void TurnOff() { Console.WriteLine("VoltMeter flashes lights saying good bye!"); }
    public void TurnOn() { Console.WriteLine("VoltMeter turns on..."); }
}

```

Now we have to define factory method. Let's create `DeviceFactory` class with static method inside:

```cs
public enum Device
{
    AM,
    VOLT,
    OHM
}
public class DeviceFactory
{
    public static IDevice CreateDevice(Device d)
    {
        switch(d)
        {
            case Device.AM: return new AmMeter();
            case Device.VOLT: return new VoltMeter();
            case Device.OHM: return new OhmMeter();
            default: return new AmMeter();
        }
    }
}

```

Great! Let's test our code:

```cs
public class Program
{
    static void Main(string[] args)
    {
        IDevice device = DeviceFactory.CreateDevice(Device.AM);
        device.TurnOn();
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        device.TurnOff();
        Console.WriteLine();

        device = DeviceFactory.CreateDevice(Device.VOLT);
        device.TurnOn();
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        device.TurnOff();
        Console.WriteLine();

        device = DeviceFactory.CreateDevice(Device.OHM);
        device.TurnOn();
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        Console.WriteLine(device.Measure());
        device.TurnOff();
        Console.WriteLine();
    }
}

```

This is the example output you might see after running this code:

> 
AmMeter turns on...
36
6
33
43
24
AmMeter flashes lights saying good bye!
VoltMeter turns on...
102
-61
85
138
36
VoltMeter flashes lights saying good bye!
OhmMeter turns on...
723828
368536
685412
800266
578595
OhmMeter flashes lights saying good bye!




## Abstract Factory Pattern


Provide an interface for creating families of related or dependent objects without specifying their concrete classes.

In this example demonstrates the creation of different animal worlds for a computer game using different factories. Although the animals created by the Continent factories are different, the interactions among the animals remain the same.

```cs
using System;
 
namespace GangOfFour.AbstractFactory
{
  /// <summary>
  /// MainApp startup class for Real-World
  /// Abstract Factory Design Pattern.
  /// </summary>
  class MainApp
  {
    /// <summary>
    /// Entry point into console application.
    /// </summary>
    public static void Main()
    {
      // Create and run the African animal world
      ContinentFactory africa = new AfricaFactory();
      AnimalWorld world = new AnimalWorld(africa);
      world.RunFoodChain();
 
      // Create and run the American animal world
      ContinentFactory america = new AmericaFactory();
      world = new AnimalWorld(america);
      world.RunFoodChain();
 
      // Wait for user input
      Console.ReadKey();
    }
  }
 
 
  /// <summary>
  /// The 'AbstractFactory' abstract class
  /// </summary>
  abstract class ContinentFactory
  {
    public abstract Herbivore CreateHerbivore();
    public abstract Carnivore CreateCarnivore();
  }
 
  /// <summary>
  /// The 'ConcreteFactory1' class
  /// </summary>
  class AfricaFactory : ContinentFactory
  {
    public override Herbivore CreateHerbivore()
    {
      return new Wildebeest();
    }
    public override Carnivore CreateCarnivore()
    {
      return new Lion();
    }
  }
 
  /// <summary>
  /// The 'ConcreteFactory2' class
  /// </summary>
  class AmericaFactory : ContinentFactory
  {
    public override Herbivore CreateHerbivore()
    {
      return new Bison();
    }
    public override Carnivore CreateCarnivore()
    {
      return new Wolf();
    }
  }
 
  /// <summary>
  /// The 'AbstractProductA' abstract class
  /// </summary>
  abstract class Herbivore
  {
  }
 
  /// <summary>
  /// The 'AbstractProductB' abstract class
  /// </summary>
  abstract class Carnivore
  {
    public abstract void Eat(Herbivore h);
  }
 
  /// <summary>
  /// The 'ProductA1' class
  /// </summary>
  class Wildebeest : Herbivore
  {
  }
 
  /// <summary>
  /// The 'ProductB1' class
  /// </summary>
  class Lion : Carnivore
  {
    public override void Eat(Herbivore h)
    {
      // Eat Wildebeest
      Console.WriteLine(this.GetType().Name +
        " eats " + h.GetType().Name);
    }
  }
 
  /// <summary>
  /// The 'ProductA2' class
  /// </summary>
  class Bison : Herbivore
  {
  }
 
  /// <summary>
  /// The 'ProductB2' class
  /// </summary>
  class Wolf : Carnivore
  {
    public override void Eat(Herbivore h)
    {
      // Eat Bison
      Console.WriteLine(this.GetType().Name +
        " eats " + h.GetType().Name);
    }
  }
 
  /// <summary>
  /// The 'Client' class 
  /// </summary>
  class AnimalWorld
  {
    private Herbivore _herbivore;
    private Carnivore _carnivore;
 
    // Constructor
    public AnimalWorld(ContinentFactory factory)
    {
      _carnivore = factory.CreateCarnivore();
      _herbivore = factory.CreateHerbivore();
    }
 
    public void RunFoodChain()
    {
      _carnivore.Eat(_herbivore);
    }
  }
}

```

Output:

> 
Lion eats Wildebeest
Wolf eats Bison




## Builder Pattern


Separate the construction of a complex object from its representation so that the same construction process can create different representations and and provides a high level of control over the assembly of the objects.

In this example demonstrates the Builder pattern in which different vehicles are assembled in a step-by-step fashion. The Shop uses VehicleBuilders to construct a variety of Vehicles in a series of sequential steps.

```cs
using System;
using System.Collections.Generic;
 
namespace GangOfFour.Builder
{
  /// <summary>
  /// MainApp startup class for Real-World 
  /// Builder Design Pattern.
  /// </summary>
  public class MainApp
  {
    /// <summary>
    /// Entry point into console application.
    /// </summary>
    public static void Main()
    {
      VehicleBuilder builder;
 
      // Create shop with vehicle builders
      Shop shop = new Shop();
 
      // Construct and display vehicles
      builder = new ScooterBuilder();
      shop.Construct(builder);
      builder.Vehicle.Show();
 
      builder = new CarBuilder();
      shop.Construct(builder);
      builder.Vehicle.Show();
 
      builder = new MotorCycleBuilder();
      shop.Construct(builder);
      builder.Vehicle.Show();
 
      // Wait for user
      Console.ReadKey();
    }
  }
 
  /// <summary>
  /// The 'Director' class
  /// </summary>
  class Shop
  {
    // Builder uses a complex series of steps
    public void Construct(VehicleBuilder vehicleBuilder)
    {
      vehicleBuilder.BuildFrame();
      vehicleBuilder.BuildEngine();
      vehicleBuilder.BuildWheels();
      vehicleBuilder.BuildDoors();
    }
  }
 
  /// <summary>
  /// The 'Builder' abstract class
  /// </summary>
  abstract class VehicleBuilder
  {
    protected Vehicle vehicle;
 
    // Gets vehicle instance
    public Vehicle Vehicle
    {
      get { return vehicle; }
    }
 
    // Abstract build methods
    public abstract void BuildFrame();
    public abstract void BuildEngine();
    public abstract void BuildWheels();
    public abstract void BuildDoors();
  }
 
  /// <summary>
  /// The 'ConcreteBuilder1' class
  /// </summary>
  class MotorCycleBuilder : VehicleBuilder
  {
    public MotorCycleBuilder()
    {
      vehicle = new Vehicle("MotorCycle");
    }
 
    public override void BuildFrame()
    {
      vehicle["frame"] = "MotorCycle Frame";
    }
 
    public override void BuildEngine()
    {
      vehicle["engine"] = "500 cc";
    }
 
    public override void BuildWheels()
    {
      vehicle["wheels"] = "2";
    }
 
    public override void BuildDoors()
    {
      vehicle["doors"] = "0";
    }
  }
 
 
  /// <summary>
  /// The 'ConcreteBuilder2' class
  /// </summary>
  class CarBuilder : VehicleBuilder
  {
    public CarBuilder()
    {
      vehicle = new Vehicle("Car");
    }
 
    public override void BuildFrame()
    {
      vehicle["frame"] = "Car Frame";
    }
 
    public override void BuildEngine()
    {
      vehicle["engine"] = "2500 cc";
    }
 
    public override void BuildWheels()
    {
      vehicle["wheels"] = "4";
    }
 
    public override void BuildDoors()
    {
      vehicle["doors"] = "4";
    }
  }
 
  /// <summary>
  /// The 'ConcreteBuilder3' class
  /// </summary>
  class ScooterBuilder : VehicleBuilder
  {
    public ScooterBuilder()
    {
      vehicle = new Vehicle("Scooter");
    }
 
    public override void BuildFrame()
    {
      vehicle["frame"] = "Scooter Frame";
    }
 
    public override void BuildEngine()
    {
      vehicle["engine"] = "50 cc";
    }
 
    public override void BuildWheels()
    {
      vehicle["wheels"] = "2";
    }
 
    public override void BuildDoors()
    {
      vehicle["doors"] = "0";
    }
  }
 
  /// <summary>
  /// The 'Product' class
  /// </summary>
  class Vehicle
  {
    private string _vehicleType;
    private Dictionary<string,string> _parts = 
      new Dictionary<string,string>();
 
    // Constructor
    public Vehicle(string vehicleType)
    {
      this._vehicleType = vehicleType;
    }
 
    // Indexer
    public string this[string key]
    {
      get { return _parts[key]; }
      set { _parts[key] = value; }
    }
 
    public void Show()
    {
      Console.WriteLine("\n---------------------------");
      Console.WriteLine("Vehicle Type: {0}", _vehicleType);
      Console.WriteLine(" Frame : {0}", _parts["frame"]);
      Console.WriteLine(" Engine : {0}", _parts["engine"]);
      Console.WriteLine(" #Wheels: {0}", _parts["wheels"]);
      Console.WriteLine(" #Doors : {0}", _parts["doors"]);
    }
  }
}

```

Output

> 
<hr />
<p>Vehicle Type: Scooter  Frame  : Scooter Frame<br />
Engine : none<br />
#Wheels: 2<br />
#Doors : 0</p>
<hr />
<p>Vehicle Type: Car<br />
Frame  : Car Frame<br />
Engine : 2500 cc<br />
#Wheels: 4<br />
#Doors : 4</p>
<hr />
<p>Vehicle Type: MotorCycle<br />
Frame  : MotorCycle Frame<br />
Engine : 500 cc<br />
#Wheels: 2<br />
#Doors : 0</p>




## Prototype Pattern


Specify the kind of objects to create using a prototypical instance, and create new objects by copying this prototype.

In this example demonstrates the Prototype pattern in which new Color objects are created by copying pre-existing, user-defined Colors of the same type.

```cs
using System;
using System.Collections.Generic;
 
namespace GangOfFour.Prototype
{
  /// <summary>
  /// MainApp startup class for Real-World 
  /// Prototype Design Pattern.
  /// </summary>
  class MainApp
  {
    /// <summary>
    /// Entry point into console application.
    /// </summary>
    static void Main()
    {
      ColorManager colormanager = new ColorManager();
 
      // Initialize with standard colors
      colormanager["red"] = new Color(255, 0, 0);
      colormanager["green"] = new Color(0, 255, 0);
      colormanager["blue"] = new Color(0, 0, 255);
 
      // User adds personalized colors
      colormanager["angry"] = new Color(255, 54, 0);
      colormanager["peace"] = new Color(128, 211, 128);
      colormanager["flame"] = new Color(211, 34, 20);
 
      // User clones selected colors
      Color color1 = colormanager["red"].Clone() as Color;
      Color color2 = colormanager["peace"].Clone() as Color;
      Color color3 = colormanager["flame"].Clone() as Color;
 
      // Wait for user
      Console.ReadKey();
    }
  }
 
  /// <summary>
  /// The 'Prototype' abstract class
  /// </summary>
  abstract class ColorPrototype
  {
    public abstract ColorPrototype Clone();
  }
 
  /// <summary>
  /// The 'ConcretePrototype' class
  /// </summary>
  class Color : ColorPrototype
  {
    private int _red;
    private int _green;
    private int _blue;
 
    // Constructor
    public Color(int red, int green, int blue)
    {
      this._red = red;
      this._green = green;
      this._blue = blue;
    }
 
    // Create a shallow copy
    public override ColorPrototype Clone()
    {
      Console.WriteLine(
        "Cloning color RGB: {0,3},{1,3},{2,3}",
        _red, _green, _blue);
 
      return this.MemberwiseClone() as ColorPrototype;
    }
  }
 
  /// <summary>
  /// Prototype manager
  /// </summary>
  class ColorManager
  {
    private Dictionary<string, ColorPrototype> _colors =
      new Dictionary<string, ColorPrototype>();
 
    // Indexer
    public ColorPrototype this[string key]
    {
      get { return _colors[key]; }
      set { _colors.Add(key, value); }
    }
  }
}

```

Output:

> 
Cloning color RGB: 255,  0,  0
Cloning color RGB: 128,211,128
Cloning color RGB: 211, 34, 20




#### Remarks


The creational patterns aim to separate a system from how its objects are created, composed, and represented. They increase the system's flexibility in terms of the what, who, how, and when of object creation. Creational patterns encapsulate the knowledge about which classes a system uses, but they hide the details of how the instances of these classes are created and put together. Programmers have come to realize that composing systems with inheritance makes those systems too rigid. The creational patterns are designed to break this close coupling.

