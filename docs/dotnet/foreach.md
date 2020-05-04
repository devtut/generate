---
metaTitle: "ForEach"
description: "Extension method for IEnumerable, Calling a method on an object in a list"
---

# ForEach



## Extension method for IEnumerable


`ForEach()` is defined on the `List<T>` class, but not on `IQueryable<T>` or `IEnumerable<T>`. You have two choices in those cases:

**ToList first**

The enumeration (or query) will be evaluated, copying the results into a new list or calling the database. The method is then called on each item.

```dotnet
IEnumerable<Customer> customers = new List<Customer>();

customers.ToList().ForEach(c => c.SendEmail());

```

This method has obvious memory usage overhead, as an intermediate list is created.

**Extension method**

Write an extension method:

```dotnet
public static void ForEach<T>(this IEnumerable<T> enumeration, Action<T> action)
{
    foreach(T item in enumeration)
    {
        action(item);
    }
}

```

Use:

```dotnet
IEnumerable<Customer> customers = new List<Customer>();

customers.ForEach(c => c.SendEmail());

```

Caution: The Framework's LINQ methods have been designed with the intention of being **pure**, which means they do not produce side effects. The `ForEach` method's only purpose is to produce side effects, and deviates from the other methods in this aspect. You may consider just using a plain `foreach` loop instead.



## Calling a method on an object in a list


```dotnet
public class Customer {
   public void SendEmail()
   {
       // Sending email code here
   }
}

List<Customer> customers = new List<Customer>();

customers.Add(new Customer());
customers.Add(new Customer());

customers.ForEach(c => c.SendEmail());

```



#### Remarks


**Use it at all?**

You might argue that the intention of the .NET framework is that queries do not have any side effects and the `ForEach` method is by definition causing a side effect. You might find your code more maintainable and easier to test if you use a plain `foreach` instead.

