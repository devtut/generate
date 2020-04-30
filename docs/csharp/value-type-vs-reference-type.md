---
metaTitle: "Value type vs Reference type"
description: "Passing by reference using ref keyword., Changing values elsewhere, Passing by reference, Assignment, Difference with method parameters ref and out, ref vs out parameters"
---

# Value type vs Reference type



## Passing by reference using ref keyword.


From the [documentation](https://msdn.microsoft.com/en-IN/library/0f66670z.aspx) :

> 
<p>In C#, arguments can be passed to parameters either by value or by
reference.  Passing by reference enables function members, methods,
properties, indexers, operators, and constructors to change the value
of the parameters and have that change persist in the calling
environment.  To pass a parameter by reference, use the `ref` or `out`
keyword.</p>


The difference between `ref` and `out` is that `out` means that the passed parameter has to be assigned before the function ends.in contrast parameters passed with `ref` can be changed or left unchanged.

```cs
using System;

class Program
{
    static void Main(string[] args)
    {
        int a = 20;
        Console.WriteLine("Inside Main - Before Callee: a = {0}", a);
        Callee(a);
        Console.WriteLine("Inside Main - After Callee: a = {0}", a);
        
        Console.WriteLine("Inside Main - Before CalleeRef: a = {0}", a);
        CalleeRef(ref a);
        Console.WriteLine("Inside Main - After CalleeRef: a = {0}", a);
     
        Console.WriteLine("Inside Main - Before CalleeOut: a = {0}", a);
        CalleeOut(out a);
        Console.WriteLine("Inside Main - After CalleeOut: a = {0}", a);
        
        Console.ReadLine();
    }

    static void Callee(int a)
    {
        a = 5;
        Console.WriteLine("Inside Callee a : {0}", a);
    }

    static void CalleeRef(ref int a)
    {
        a = 6;
        Console.WriteLine("Inside CalleeRef a : {0}", a);
    }
    
    static void CalleeOut(out int a)
    {
        a = 7;
        Console.WriteLine("Inside CalleeOut a : {0}", a);
    }
}

```

**Output** :

```cs
Inside Main - Before Callee: a = 20
Inside Callee a : 5
Inside Main - After Callee: a = 20
Inside Main - Before CalleeRef: a = 20
Inside CalleeRef a : 6
Inside Main - After CalleeRef: a = 6
Inside Main - Before CalleeOut: a = 6
Inside CalleeOut a : 7
Inside Main - After CalleeOut: a = 7

```



## Changing values elsewhere


```cs
public static void Main(string[] args)
{
    var studentList = new List<Student>();
    studentList.Add(new Student("Scott", "Nuke"));
    studentList.Add(new Student("Vincent", "King"));
    studentList.Add(new Student("Craig", "Bertt"));

    // make a separate list to print out later
    var printingList = studentList; // this is a new list object, but holding the same student objects inside it

    // oops, we've noticed typos in the names, so we fix those
    studentList[0].LastName = "Duke";
    studentList[1].LastName = "Kong";
    studentList[2].LastName = "Brett";

    // okay, we now print the list
    PrintPrintingList(printingList);
}

private static void PrintPrintingList(List<Student> students)
{
    foreach (Student student in students)
    {
        Console.WriteLine(string.Format("{0} {1}", student.FirstName, student.LastName));
    }
}

```

You'll notice that even though the printingList list was made before the corrections to student names after the typos, the PrintPrintingList method still prints out the corrected names:

```cs
Scott Duke
Vincent Kong
Craig Brett

```

This is because both lists hold a list of references to the same students. SO changing the underlying student object propogates to usages by either list.

Here's what the student class would look like.

```cs
public class Student
{
    public string FirstName { get; set; }
    public string LastName { get; set; }

    public Student(string firstName, string lastName)
    {
        this.FirstName = firstName;
        this.LastName = lastName;
    }
}

```



## Passing by reference


If you want the Value Types vs Reference Types in methods example to work properly, use the ref keyword in your method signature for the parameter you want to pass by reference, as well as when you call the method.

```cs
public static void Main(string[] args)
{
    ...
    DoubleNumber(ref number); // calling code
    Console.WriteLine(number); // outputs 8
    ...
}

```

```cs
public void DoubleNumber(ref int number)
{
    number += number;
}

```

Making these changes would make the number update as expected, meaning the console output for number would be 8.



## Assignment


```cs
var a = new List<int>();
var b = a;
a.Add(5);
Console.WriteLine(a.Count); // prints 1 
Console.WriteLine(b.Count); // prints 1 as well

```

Assigning to a variable of a `List<int>` does not create a copy of the `List<int>`. Instead, it copies the reference to the `List<int>`. We call types that behave this way **reference types**.



## Difference with method parameters ref and out


There are two possible ways to pass a value type by reference: `ref` and `out`. The difference is that by passing it with `ref` the value must be initialized but not when passing it with `out`. Using `out` ensures that the variable has a value after the method call:

```cs
public void ByRef(ref int value)
{
    Console.WriteLine(nameof(ByRef) + value);
    value += 4;
    Console.WriteLine(nameof(ByRef) + value);
}

public void ByOut(out int value)
{
    value += 4 // CS0269: Use of unassigned out parameter `value'  
    Console.WriteLine(nameof(ByOut) + value); // CS0269: Use of unassigned out parameter `value'  

    value = 4;
    Console.WriteLine(nameof(ByOut) + value);
}

public void TestOut()
{
    int outValue1;
    ByOut(out outValue1); // prints 4

    int outValue2 = 10;   // does not make any sense for out
    ByOut(out outValue2); // prints 4
}

public void TestRef()
{
    int refValue1;
    ByRef(ref refValue1); // S0165  Use of unassigned local variable 'refValue'

    int refValue2 = 0;
    ByRef(ref refValue2); // prints 0 and 4

    int refValue3 = 10;
    ByRef(ref refValue3); // prints 10 and 14
}

```

The catch is that by using `out` the parameter `must` be initialized before leaving the method, therefore the following method is possible with `ref` but not with `out`:

```cs
public void EmtyRef(bool condition, ref int value)
{
    if (condition)
    {
        value += 10;
    }
}

public void EmtyOut(bool condition, out int value)
{
    if (condition)
    {
        value = 10;
    }
} //CS0177: The out parameter 'value' must be assigned before control leaves the current method

```

This is because if `condition` does not hold, `value` goes unassigned.



## ref vs out parameters


**Code**

```cs
class Program
{
    static void Main(string[] args)
    {
        int a = 20;
        Console.WriteLine("Inside Main - Before Callee: a = {0}", a);
        Callee(a);
        Console.WriteLine("Inside Main - After Callee: a = {0}", a);
        Console.WriteLine();

        Console.WriteLine("Inside Main - Before CalleeRef: a = {0}", a);
        CalleeRef(ref a);
        Console.WriteLine("Inside Main - After CalleeRef: a = {0}", a);
        Console.WriteLine();

        Console.WriteLine("Inside Main - Before CalleeOut: a = {0}", a);
        CalleeOut(out a);
        Console.WriteLine("Inside Main - After CalleeOut: a = {0}", a);
        Console.ReadLine();
    }

    static void Callee(int a)
    {
        a += 5;
        Console.WriteLine("Inside Callee a : {0}", a);
    }

    static void CalleeRef(ref int a)
    {
        a += 10;
        Console.WriteLine("Inside CalleeRef a : {0}", a);
    }

    static void CalleeOut(out int a)
    {
        // can't use a+=15 since for this method 'a' is not intialized only declared in the method declaration
        a = 25; //has to be initialized
        Console.WriteLine("Inside CalleeOut a : {0}", a);
    }
}

```

**Output**

```cs
Inside Main - Before Callee: a = 20
Inside Callee a : 25
Inside Main - After Callee: a = 20

Inside Main - Before CalleeRef: a = 20
Inside CalleeRef a : 30
Inside Main - After CalleeRef: a = 30

Inside Main - Before CalleeOut: a = 30
Inside CalleeOut a : 25
Inside Main - After CalleeOut: a = 25

```



#### Syntax


- Passing by reference: public void Double(ref int numberToDouble) { }



#### Remarks


### Introduction

### Value types

Value types are the simpler of the two. Value types are often used to represent data itself. An integer, a Boolean or a point in 3D space are all examples of good value types.

Value types (structs) are declared by using the struct keyword. See the syntax section for an example of how to declare a new struct.

Generally speaking, We have 2 keywords that are used to declare value types:

- Structs
- Enumerations

### Reference types

Reference types are slightly more complex. Reference types are traditional objects in the sense of Object Oriented Programming. So, they support inheritance (and the benefits there of) and also support finalizers.

In C# generally we have this reference types:

- Classes
- Delegates
- Interfaces

New reference types (classes) are declared using the class keyword. For an example, see the syntax section for how to declare a new reference type.

### Major Differences

The major differences between reference types and value types can be seen below.

### Value types exist on the stack, reference types exist on the heap

This is the often mentioned difference between the two, but really, what it boils down to is that when you use a value type in C#, such as an int, the program will use that variable to refer directly to that value. If you say int mine = 0, then the variable mine refers directly to 0, which is efficient. However, reference types actually hold (as the name suggests) a reference to the underlying object, this is akin to pointers in other languages such as C++.

You might not notice the effects of this immediately, but the effects are there, are powerful and are subtle. See the example on changing reference types elsewhere for an example.

This difference is the primary reason for the following other differences, and is worth knowing.

### Value types don't change when you change them in a method, reference types do

When a value type is passed into a method as a parameter, if the method changes the value in any way, the value is not changed In contrast, passing a reference type into that same method and changing it will change the underlying object, so that other things that use that same object will have the newly changed object rather than their original value.

See the example of value types vs reference types in methods for more info.

Simply pass them into your method using the "ref" keyword, and you are then passing this object by reference. Meaning, it's the same object in memory. So modifications you make will be respected. See the example on passing by reference for an example.

### Value types cannot be null, reference types can

Pretty much as it says, you can assign null to a reference type, meaning the variable you've assigned can have no actual object assigned to it. In the case of value types, however, this is not possible. You can, however, use Nullable, to allow your value type to be nullable, if this is a requirement, though if this is something you are considering, think strongly whether a class might not be the best approach here, if it is your own type.

