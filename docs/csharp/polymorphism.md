---
metaTitle: "Polymorphism"
description: "Types of Polymorphism, Another Polymorphism Example"
---

# Polymorphism



## Types of Polymorphism


Polymorphism means that a operation can also be applied to values of some other types.

There are multiple types of Polymorphism:

<li>**Ad hoc polymorphism:**<br />
contains `function overloading`. The target is that a Method can be used with
different types without the need of being generic.</li>
<li>**Parametric polymorphism:**<br />
is the use of generic types. See [Generics](http://stackoverflow.com/documentation/c%23/27/generics)</li>
<li>**Subtyping:**<br />
has the target inherit of a class to generalize a similar functionality</li>

### Ad hoc polymorphism

The target of `Ad hoc polymorphism` is to create a method, that can be called by different datatypes without a need of type-conversion in the function call or generics. The following method(s) `sumInt(par1, par2)` can be called with different datatypes and has for each combination of types a own implementation:

```cs
public static int sumInt( int a, int b)
{
    return a + b;    
}

public static int sumInt( string a, string b)
{
    int _a, _b;
    
    if(!Int32.TryParse(a, out _a))
        _a = 0;
    
    if(!Int32.TryParse(b, out _b))
        _b = 0;
    
    return _a + _b;
}

public static int sumInt(string a, int b)
{
    int _a;
    
    if(!Int32.TryParse(a, out _a))
        _a = 0;    
    
    return _a + b;
}

public static int sumInt(int a, string b)
{        
    return sumInt(b,a);
}

```

Here's a example call:

```cs
public static void Main()
{
    Console.WriteLine(sumInt( 1 , 2 ));  //  3
    Console.WriteLine(sumInt("3","4"));  //  7
    Console.WriteLine(sumInt("5", 6 ));  // 11
    Console.WriteLine(sumInt( 7 ,"8"));  // 15
}

```

### Subtyping

Subtyping is the use of inherit from a base class to generalize a similar behavior:

```cs
public interface Car{
    void refuel();
}

public class NormalCar : Car
{
    public void refuel()
    {
        Console.WriteLine("Refueling with petrol");    
    }
}

public class ElectricCar : Car
{
    public void refuel()
    {
        Console.WriteLine("Charging battery");    
    }
}

```

Both classes `NormalCar` and `ElectricCar` now have a method to refuel, but their own implementation. Here's a Example:

```cs
public static void Main()
{
    List<Car> cars = new List<Car>(){
        new NormalCar(),
        new ElectricCar()
    };
    
    cars.ForEach(x => x.refuel());
}

```

The output will be was following:

> 
<p>Refueling with petrol<br />
Charging battery</p>




## Another Polymorphism Example


Polymorphism is one of the pillar of OOP. Poly derives from a Greek term which means 'multiple forms'.

Below is an example which exhibits Polymorphism. The class `Vehicle` takes multiple forms as a base class.

The Derived classes `Ducati` and `Lamborghini` inherits from `Vehicle` and overrides the base class's `Display()` method, to display its own `NumberOfWheels`.

```cs
public class Vehicle
{
    protected int NumberOfWheels { get; set; } = 0;
    public Vehicle()
    {
    }

    public virtual void Display()
    {
        Console.WriteLine($"The number of wheels for the {nameof(Vehicle)} is {NumberOfWheels}");
    }
}

public class Ducati : Vehicle
{
    public Ducati()
    {
        NoOfWheels = 2;
    }

    public override void Display()
    {
        Console.WriteLine($"The number of wheels for the {nameof(Ducati)} is {NumberOfWheels}");
    }
}

public class Lamborghini : Vehicle
{
    public Lamborghini()
    {
        NoOfWheels = 4;
    }

    public override void Display()
    {
        Console.WriteLine($"The number of wheels for the {nameof(Lamborghini)} is {NumberOfWheels}");
    }
}

```

Below is the code snippet where Polymorphism is exhibited. The object is created for the base type `Vehicle` using a variable `vehicle` at Line 1. It calls the base class method `Display()` at Line 2 and display the output as shown.

```cs

static void Main(string[] args)
 {
    Vehicle vehicle = new Vehicle();    //Line 1
    vehicle.Display();                  //Line 2  
    vehicle = new Ducati();             //Line 3
    vehicle.Display();                  //Line 4
    vehicle = new Lamborghini();        //Line 5
    vehicle.Display();                  //Line 6
 }

```

At Line 3, the `vehicle` object is pointed to the derived class `Ducati` and calls its `Display()` method, which displays the output as shown. Here comes the polymorphic behavior, even though the object `vehicle` is of type `Vehicle`, it calls the derived class method `Display()` as the type `Ducati` overrides the base class `Display()` method, since the `vehicle` object is pointed towards `Ducati`.

The same explanation is applicable when it invokes the `Lamborghini` type's `Display()` method.

The Output is shown below

```cs
The number of wheels for the Vehicle is 0        // Line 2 
The number of wheels for the Ducati is 2         // Line 4
The number of wheels for the Lamborghini is 4    // Line 6

```

