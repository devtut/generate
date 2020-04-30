---
metaTitle: "ICloneable"
description: "Implementing ICloneable in a class, Implementing ICloneable in a struct"
---

# ICloneable




## Implementing ICloneable in a class


Implement `ICloneable` in a class with a twist. Expose a public type safe `Clone()` and implement `object Clone()` privately.

```cs
public class Person : ICloneable
{
    // Contents of class
    public string Name { get; set; }
    public int Age { get; set; }
    // Constructor
    public Person(string name, int age)
    {
        this.Name=name;
        this.Age=age;
    }
    // Copy Constructor
    public Person(Person other)
    {
        this.Name=other.Name;
        this.Age=other.Age;
    }

    #region ICloneable Members
    // Type safe Clone
    public Person Clone() { return new Person(this); }
    // ICloneable implementation
    object ICloneable.Clone()
    {
        return Clone();
    }
    #endregion
}

```

Later to be used as follows:

```cs
{
    Person bob=new Person("Bob", 25);
    Person bob_clone=bob.Clone();
    Debug.Assert(bob_clone.Name==bob.Name);

    bob.Age=56;
    Debug.Assert(bob.Age!=bob.Age);
}

```

Notice that changing the age of `bob` does not change the age of `bob_clone`. This is because the design uses cloning instead of assigning of (reference) variables.



## Implementing ICloneable in a struct


The implementation of ICloneable for a struct is not generally needed because structs do a memberwise copy with the assignment operator `=`. But the design might require the implementation of another interface that inherits from `ICloneable`.

Another reason would be if the struct contains a reference type (or an array) which would need copying also.

```cs
// Structs are recommended to be immutable objects
[ImmutableObject(true)]
public struct Person : ICloneable
{
    // Contents of class
    public string Name { get; private set; }
    public int Age { get; private set; }
    // Constructor
    public Person(string name, int age)
    {
        this.Name=name;
        this.Age=age;
    }
    // Copy Constructor
    public Person(Person other)
    {
        // The assignment operator copies all members
        this=other;
    }

    #region ICloneable Members
    // Type safe Clone
    public Person Clone() { return new Person(this); }
    // ICloneable implementation
    object ICloneable.Clone()
    {
        return Clone();
    }
    #endregion
}

```

Later to be used as follows:

```cs
static void Main(string[] args)
{
    Person bob=new Person("Bob", 25);
    Person bob_clone=bob.Clone();
    Debug.Assert(bob_clone.Name==bob.Name);
}

```



#### Syntax


- object ICloneable.Clone() { return Clone(); }  // Private implementation of interface method which uses our custom public Clone() function.
- public Foo Clone() { return new Foo(this); } // Public clone method should utilize the copy constructor logic.



#### Remarks


The `CLR` requires a method definition `object Clone()` which is not type safe. It is common practice to override this behavior and define a type safe method that returns a copy of the containing class.

It is up to the author to decide if cloning means only shallow copy, or deep copy. For immutable structures containing references it is recommended to do a deep copy. For classes being references themselves it is probably fine to implement a shallow copy.

<sub>NOTE: In `C#` an interface method can be implemented privately with the syntax shown above.</sub>

