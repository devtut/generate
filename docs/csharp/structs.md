---
metaTitle: "C# | Structs"
description: "Declaring a struct, Struct usage, Struct implementing interface, Structs are copied on assignment"
---

# Structs



## Declaring a struct


```cs
public struct Vector 
{
    public int X;
    public int Y;
    public int Z;
}

public struct Point
{
    public decimal x, y;
    
    public Point(decimal pointX, decimal pointY)
    {
        x = pointX;
        y = pointY;
    }
}

```


<li>
`struct` instance fields can be set via a parametrized constructor or individually after `struct` construction.
</li>
<li>
Private members can only be initialized by the constructor.
</li>
<li>
`struct` defines a sealed type that implicitly inherits from System.ValueType.
</li>
<li>
Structs cannot inherit from any other type, but they can implement interfaces.
</li>
<li>
Structs are copied on assignment, meaning all data is copied to the new instance and changes to one of them are not reflected by the other.
</li>
<li>
A struct cannot be `null`, although it **can** used as a nullable type:

```cs
Vector v1 = null; //illegal
Vector? v2 = null; //OK
Nullable<Vector> v3 = null // OK

```


</li>
<li>
Structs can be instantiated with or without using the `new` operator.

```cs
//Both of these are acceptable
Vector v1 = new Vector();
v1.X = 1;
v1.Y = 2;
v1.Z = 3;

Vector v2;
v2.X = 1;
v2.Y = 2;
v2.Z = 3;

```


However, the `new` operator must be used in order to use an initializer:

```cs
Vector v1 = new MyStruct { X=1, Y=2, Z=3 }; // OK
Vector v2 { X=1, Y=2, Z=3 }; // illegal

```


</li>

A struct can declare everything a class can declare, with a few exceptions:

- A struct cannot declare a parameterless constructor. `struct` instance fields can be set via a parameterized constructor or individually after `struct` construction. Private members can only be initialized by the constructor.
- A struct cannot declare members as protected, since it is implicitly sealed.
- Struct fields can only be initialized if they are const or static.



## Struct usage


**With constructor:**

```cs
Vector v1 = new Vector();
v1.X = 1;
v1.Y = 2;
v1.Z = 3;

Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
// Output X=1,Y=2,Z=3

Vector v1 = new Vector();
//v1.X is not assigned
v1.Y = 2;
v1.Z = 3;

Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
// Output X=0,Y=2,Z=3

Point point1 = new Point();
point1.x = 0.5;
point1.y = 0.6;

Point point2 = new Point(0.5, 0.6);

```

**Without constructor:**

```cs
Vector v1;
v1.Y = 2;
v1.Z = 3;

Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
//Output ERROR "Use of possibly unassigned field 'X'

Vector v1;
v1.X = 1;
v1.Y = 2;
v1.Z = 3;

Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
// Output X=1,Y=2,Z=3

Point point3;
point3.x = 0.5;
point3.y = 0.6;

```

If we use a struct with its constructor, we aren't going to have problems with unassigned field (each unassigned field has null value).

Unlike classes, a struct doesn't have to be constructed, i.e. there is no need to use the new keyword, unless you need to call one of the constructors. A struct does not require the new keyword because is a value-type and thus cannot be null.



## Struct implementing interface


```cs
public interface IShape
{
    decimal Area();
}

public struct Rectangle : IShape
{
    public decimal Length { get; set; }
    public decimal Width { get; set; }

    public decimal Area()
    {
        return Length * Width;
    }
}

```



## Structs are copied on assignment


Sinse structs are value types all the data is **copied** on assignment, and any modification to the new copy does not change the data for the original copy. The code snippet below shows that `p1` is **copied** to `p2` and changes made on `p1` does not affect `p2` instance.

```cs
var p1 = new Point {
    x = 1,
    y = 2
};

Console.WriteLine($"{p1.x} {p1.y}"); // 1 2

var p2 = p1;
Console.WriteLine($"{p2.x} {p2.y}"); // Same output: 1 2

p1.x = 3;
Console.WriteLine($"{p1.x} {p1.y}"); // 3 2
Console.WriteLine($"{p2.x} {p2.y}"); // p2 remain the same: 1 2

```



#### Remarks


Unlike classes, a `struct` is a value type, and is created on the local stack and not on the managed heap, **by default**. This means that once the specific stack goes out of scope, the `struct` is de-allocated. Contained reference types of de-allocated `struct`s are also swept, once the GC determines they are not longer referenced to by the `struct`.

`struct`s cannot inherit and cannot be bases for inheritance, they are implicitly sealed, and also cannot include `protected` members. However, a `struct` can implement an interface, as classes do.

