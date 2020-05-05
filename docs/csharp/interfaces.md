---
metaTitle: "C# | Interfaces"
description: "Implementing an interface, Explicit interface implementation, Implementing multiple interfaces, Interface Basics, IComparable<T> as an Example of Implementing an Interface, Why we use interfaces, Hiding members with Explicit Implementation"
---

# Interfaces



## Implementing an interface


An interface is used to enforce the presence of a method in any class that 'implements' it.  The interface is defined with the keyword `interface` and a class can 'implement' it by adding `: InterfaceName` after the class name. A class can implement multiple interfaces by separating each interface with a comma.<br />
`: InterfaceName, ISecondInterface`

```cs
public interface INoiseMaker
{
    string MakeNoise();
}

public class Cat : INoiseMaker
{
    public string MakeNoise()
    {
        return "Nyan";
    }
}

public class Dog : INoiseMaker
{
    public string MakeNoise()
    {
        return "Woof";
    }
}

```

Because they implement `INoiseMaker`, both `cat` and `dog` are required to include the `string MakeNoise()` method and will fail to compile without it.



## Explicit interface implementation


Explicit interface implementation is necessary when you implement multiple interfaces who define a common method, but different implementations are required depending on which interface is being used to call the method (note that you don't need explicit implementations if multiple interfaces share the same method and a common implementation is possible).

```cs
interface IChauffeur 
{
    string Drive();
}

interface IGolfPlayer
{
    string Drive();
}

class GolfingChauffeur : IChauffeur, IGolfPlayer 
{
    public string Drive()
    {
        return "Vroom!";
    }

    string IGolfPlayer.Drive()
    {
        return "Took a swing...";
    }
}


GolfingChauffeur obj = new GolfingChauffeur();
IChauffeur chauffeur = obj;
IGolfPlayer golfer = obj;

Console.WriteLine(obj.Drive()); // Vroom!
Console.WriteLine(chauffeur.Drive()); // Vroom!
Console.WriteLine(golfer.Drive()); // Took a swing...

```

The implementation cannot be called from anywhere else except by using the interface:

```cs
public class Golfer : IGolfPlayer
{
    string IGolfPlayer.Drive()
    {
        return "Swinging hard...";
    }
    public void Swing()
    {
        Drive(); // Compiler error: No such method
    }
}

```

Due to this, it may be advantageous to put complex implementation code of an explicitly implemented interface in a separate, private method.

An explicit interface implementation can of course only be used for methods that actually exist for that interface:

```cs
public class ProGolfer : IGolfPlayer
{
    string IGolfPlayer.Swear() // Error
    {
        return "The ball is in the pit";
    }
}

```

Similarly, using an explicit interface implementation without declaring that interface on the class causes an error, too.

### Hint:

Implementing interfaces explicitly can also be used to avoid dead code. When a method is no longer needed and gets removed from the interface, the compiler will complain about each still existing implementation.

### Note:

Programmers expect the contract to be the same regardless of the context of the type and explicit implementation should not expose different behavior when called.
So unlike the example above, `IGolfPlayer.Drive` and `Drive` should do the same thing when possible.



## Implementing multiple interfaces


```cs
public interface IAnimal 
{
    string Name { get; set; }
}

public interface INoiseMaker
{
    string MakeNoise();
}

public class Cat : IAnimal, INoiseMaker
{
    public Cat() 
    {
        Name = "Cat";
    }

    public string Name { get; set; }

    public string MakeNoise()
    {
        return "Nyan";
    }
}

```



## Interface Basics


An Interface's function known as a "contract" of functionality. It means that it declares properties and methods but it doesn't implement them.

So unlike classes Interfaces:

- Can't be instantiated
- Can't have any functionality
- Can only contain methods * **(Properties and Events are methods internally)**
- Inheriting an interface is called "Implementing"
- You can inherit from 1 class, but you can "Implement" multiple Interfaces

```cs
public interface ICanDoThis{
    void TheThingICanDo();
    int SomeValueProperty { get; set; }
}

```

Things to notice:

- The "I" prefix is a naming convention used for interfaces.
- The function body is replaced with a semicolon ";".
- Properties are also allowed because internally they are also methods

```cs
public class MyClass : ICanDoThis {
    public void TheThingICanDo(){
        // do the thing
    }

    public int SomeValueProperty { get; set; }
    public int SomeValueNotImplemtingAnything { get; set; }
}

```

.

```cs
ICanDoThis obj = new MyClass();

// ok
obj.TheThingICanDo();

// ok
obj.SomeValueProperty = 5;

// Error, this member doesn't exist in the interface
obj.SomeValueNotImplemtingAnything = 5;

// in order to access the property in the class you must "down cast" it
((MyClass)obj).SomeValueNotImplemtingAnything = 5; // ok

```

This is especially useful when you're working with UI frameworks such as WinForms or WPF because it's mandatory to inherit from a base class to create user control and you loose the ability to create abstraction over different control types. An example? Coming up:

```cs
public class MyTextBlock : TextBlock {
    public void SetText(string str){
        this.Text = str;
    }
}

public class MyButton : Button {
    public void SetText(string str){
        this.Content = str;
    }
}

```

The problem proposed is that both contain some concept of "Text" but the property names differ. And you can't create create a **abstract base class** because they have a mandatory inheritance to 2 different classes. An interface can alleviate that

```cs
public interface ITextControl{
    void SetText(string str);
}

public class MyTextBlock : TextBlock, ITextControl {
    public void SetText(string str){
        this.Text = str;
    }
}

public class MyButton : Button, ITextControl {
    public void SetText(string str){
        this.Content = str;
    }

    public int Clicks { get; set; }
}

```

Now MyButton and MyTextBlock is interchangeable.

```cs
var controls = new List<ITextControls>{
    new MyTextBlock(),
    new MyButton()
};

foreach(var ctrl in controls){
    ctrl.SetText("This text will be applied to both controls despite them being different");


    // Compiler Error, no such member in interface
    ctrl.Clicks = 0;

    // Runtime Error because 1 class is in fact not a button which makes this cast invalid
    ((MyButton)ctrl).Clicks = 0;


    /* the solution is to check the type first.
    This is usually considered bad practice since
    it's a symptom of poor abstraction */
    var button = ctrl as MyButton;
    if(button != null)
        button.Clicks = 0; // no errors

   
}

```



## IComparable<T> as an Example of Implementing an Interface


Interfaces can seem abstract until you seem them in practice. The `IComparable` and `IComparable<T>` are great examples of why interfaces can be helpful to us.

Let's say that in a program for a online store, we have a variety of items you can buy. Each item has a name, an ID number, and a price.

```cs
public class Item {
    
    public string name; // though public variables are generally bad practice,
    public int idNumber; // to keep this example simple we will use them instead
    public decimal price; // of a property.

    // body omitted for brevity        

}

```

We have our `Item`s stored inside of a `List<Item>`, and in our program somewhere, we want to sort our list by ID number from smallest to largest. Instead of writing our own sorting algorithm, we can instead use the `Sort()` method that `List<T>` already has. However, as our `Item` class is right now, there is no way for the `List<T>` to understand what order to sort the list. Here is where the `IComparable` interface comes in.

To correctly implement the `CompareTo` method, `CompareTo` should return a positive number if the parameter is "less than" the current one, zero if they are equal, and a negative number if the parameter is "greater than".

```cs
Item apple = new Item();
apple.idNumber = 15;
Item banana = new Item();
banana.idNumber = 4;
Item cow = new Item();
cow.idNumber = 15;
Item diamond = new Item();
diamond.idNumber = 18;

Console.WriteLine(apple.CompareTo(banana)); // 11
Console.WriteLine(apple.CompareTo(cow)); // 0
Console.WriteLine(apple.CompareTo(diamond)); // -3

```

Here's the example `Item`'s implementation of the interface:

```cs
public class Item : IComparable<Item> {
    
    private string name;
    private int idNumber;
    private decimal price;

    public int CompareTo(Item otherItem) {

        return (this.idNumber - otherItem.idNumber);

    }

    // rest of code omitted for brevity    

}

```

On a surface level, the `CompareTo` method in our item simply returns the difference in their ID numbers, but what does the above do in practice?

Now, when we call `Sort()` on a `List<Item>` object, the `List` will automatically call the `Item`'s `CompareTo` method when it needs to determine what order to put objects in. Furthermore, besides `List<T>`, any other objects that need the ability to compare two objects will work with the `Item` because we have defined the ability for two different `Item`s to be compared with one another.



## Why we use interfaces


An interface is a definition of a contract between the user of the interface and the class that implement it. One way to think of an interface is as a declaration that an object can perform certain functions.

Let's say that we define an interface `IShape` to represent different type of shapes, we expect a shape to have an area, so we will define a method to force the interface implementations to return their area :

```cs
public interface IShape
{
    double ComputeArea();
}

```

Let's that we have the following two shapes : a `Rectangle` and a `Circle`

```cs
public class Rectangle : IShape
{
    private double length;
    private double width;

    public Rectangle(double length, double width)
    {
        this.length = length;
        this.width = width;
    }

    public double ComputeArea()
    {
        return length * width;
    }
}

public class Circle : IShape
{
    private double radius;

    public Circle(double radius)
    {
        this.radius = radius;
    }

    public double ComputeArea()
    {
        return Math.Pow(radius, 2.0) * Math.PI;
    }
}

```

Each one of them have its own definition of its area, but both of them are shapes. So it's only logical to see them as `IShape` in our program :

```cs
private static void Main(string[] args)
{
    var shapes = new List<IShape>() { new Rectangle(5, 10), new Circle(5) };
    ComputeArea(shapes);

    Console.ReadKey();
}

private static void ComputeArea(IEnumerable<IShape> shapes) 
{
    foreach (shape in shapes)
    {
        Console.WriteLine("Area: {0:N}, shape.ComputeArea());
    }
}

// Output:
// Area : 50.00
// Area : 78.54

```



## "Hiding" members with Explicit Implementation


Don't you hate it when interfaces pollute you class with too many members you don't even care about? Well I got a solution! Explicit Implementations

```cs
public interface IMessageService {
    void OnMessageRecieve();
    void SendMessage();
    string Result { get; set; }
    int Encoding { get; set; }
    // yadda yadda
}

```

Normally you'd implement the class like this.

```cs
public class MyObjectWithMessages : IMessageService {
     public void OnMessageRecieve(){

     }

     public void SendMessage(){

     }

     public string Result { get; set; }
     public int Encoding { get; set; }
}

```

Every member is public.

```cs
var obj = new MyObjectWithMessages();

// why would i want to call this function?
obj.OnMessageRecieve();

```

Answer: I don't. So neither should it be declared public
but simply declaring the members as private will make the compiler throw an error

The solution is to use explicit implementation:

```cs
public class MyObjectWithMessages : IMessageService{
    void IMessageService.OnMessageRecieve() {
        
    }

    void IMessageService.SendMessage() {
        
    }

    string IMessageService.Result { get; set; }
    int IMessageService.Encoding { get; set; }
}

```

So now you have implemented the members as required and they wont expose any members in as public.

```cs
var obj = new MyObjectWithMessages();

/* error member does not exist on type MyObjectWithMessages. 
 * We've succesfully made it "private" */
obj.OnMessageRecieve();

```

If you seriously still want to access the member even though is explicitly implement all you have to do is cast the object to the interface and you good to go.

```cs
((IMessageService)obj).OnMessageRecieve();

```

