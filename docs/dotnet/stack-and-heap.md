---
metaTitle: ".NET Framework - Stack and Heap"
description: "Value types in use, Reference types in use"
---

# Stack and Heap




## Value types in use


Value types simply contain a ****value****.

All value types are derived from the [System.ValueType](https://msdn.microsoft.com/en-us/library/system.valuetype.aspx) class, and this includes most of the built in types.

When creating a new value type, the an area of memory called ****the stack**** is used.<br />
The stack will grow accordingly, by the size the declared type. So for example, an int will always be allocated 32 bits of memory on the stack. When the value type is no longer in scope, the space on the stack will be deallocated.

The code below demonstrates a value type being assigned to a new variable. A struct is being used as a convenient way to create a custom value type (the System.ValueType class cannot be otherwise extended).

The important thing to understand is that when assigning a value type, the value itself ****copied**** to the new variable, meaning we have two distinct instances of the object, that cannot affect each other.

```dotnet
struct PersonAsValueType
{
    public string Name;
}

class Program
{
    static void Main()
    {
        PersonAsValueType personA;

        personA.Name = "Bob";

        var personB = personA;

        personA.Name = "Linda";

        Console.WriteLine(                // Outputs 'False' - because 
            object.ReferenceEquals(       // personA and personB are referencing 
                personA,                  // different areas of memory
                personB));                

        Console.WriteLine(personA.Name);  // Outputs 'Linda'
        Console.WriteLine(personB.Name);  // Outputs 'Bob'
    }
}

```



## Reference types in use


Reference types are comprised of both a ****reference**** to a memory area, and a ****value**** stored within that area.<br />
This is analogous to pointers in C/C++.

All reference types are stored on what is known as ****the heap****.<br />
The heap is simply a managed area of memory where objects are stored. When a new object is instantiated, a part of the heap will be allocated for use by that object, and a  reference to that location of the heap will be returned. The heap is managed and maintained by the **garbage collector**, and does not allow for manual intervention.

In addition to the memory space required for the instance itself, additional space is required to store the reference itself, along with additional temporary information required by the .NET CLR.

The code below demonstrates a reference type being assigned to a new variable. In this instance, we are using a class, all classes are reference types (even if static).

When a reference type is assigned to another variable, it is the ****reference**** to the object that is copied over, **not** the value itself. This is an important distinction between value types and reference types.

The implications of this are that we now have **two** references to the same object.<br />
Any changes to the values within that object will be reflected by both variables.

```dotnet
class PersonAsReferenceType
{
    public string Name;
}

class Program
{
    static void Main()
    {
        PersonAsReferenceType personA;

        personA = new PersonAsReferenceType { Name = "Bob" };

        var personB = personA;

        personA.Name = "Linda";

        Console.WriteLine(               // Outputs 'True' - because
            object.ReferenceEquals(      // personA and personB are referencing 
                personA,                 // the *same* memory location
                personB));

        Console.WriteLine(personA.Name); // Outputs 'Linda'
        Console.WriteLine(personB.Name); // Outputs 'Linda'
    }

```



#### Remarks


It's worth noting that on declaring a reference type, its initial value will be `null`. This is because it does not yet point to a location in memory, and is a perfectly valid state.<br />
However, with the exception of nullable types, value types must typically always have a value.

