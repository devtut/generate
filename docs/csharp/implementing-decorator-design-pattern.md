---
metaTitle: "C# | Implementing Decorator Design Pattern"
description: "Simulating cafeteria"
---

# Implementing Decorator Design Pattern



## Simulating cafeteria


Decorator is one of structural design patterns. It is used to add, remove or change behaviour of object. This document will teach you how to use Decorator DP properly.

Let me explain the idea of it to you on a simple example. Imagine you're now in Starbobs, famous coffee company. You can place an order for any coffee you want - with cream and sugar, with cream and topping and much more combinations! But, the base of all drinks is coffee - dark, bitter drink, you can modify. Let's write a simple program that simulates coffee machine.

First, we need to create and abstract class that describes our base drink:

```cs
public abstract class AbstractCoffee
{
    protected AbstractCoffee k = null;

    public AbstractCoffee(AbstractCoffee k)
    {
        this.k = k;
    }

    public abstract string ShowCoffee();
}

```

Now, let's create some extras, like sugar, milk and topping. Created classes must implement `AbstractCoffee` - they will decorate it:

```cs
public class Milk : AbstractCoffee
{
    public Milk(AbstractCoffee c) : base(c) { }
    public override string ShowCoffee()
    {
        if (k != null)
            return k.ShowCoffee() + " with Milk";
        else return "Milk";
    }
}
public class Sugar : AbstractCoffee
{
    public Sugar(AbstractCoffee c) : base(c) { }

    public override string ShowCoffee()
    {
        if (k != null) return k.ShowCoffee() + " with Sugar";
        else return "Sugar";
    }
}
public class Topping : AbstractCoffee
{
    public Topping(AbstractCoffee c) : base(c) { }

    public override string ShowCoffee()
    {
        if (k != null) return k.ShowCoffee() + " with Topping";
        else return "Topping";
    }
}

```

Now we can create our favourite coffee:

```cs
public class Program
{
    public static void Main(string[] args)
    {
        AbstractCoffee coffee = null; //we cant create instance of abstract class
        coffee = new Topping(coffee); //passing null
        coffee = new Sugar(coffee); //passing topping instance
        coffee = new Milk(coffee);  //passing sugar
        Console.WriteLine("Coffee with " + coffee.ShowCoffee());

    }
}

```

Running the code will produce the following output:

> 
Coffee with Topping with Sugar with Milk




#### Remarks


Pros of using Decorator:

- you can add new functionalities at runtime in different configurations
- good alternative for inheritance
- client can choose configuration he wants to use

