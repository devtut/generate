---
metaTitle: "ReadOnlyCollections"
description: "Creating a ReadOnlyCollection, Updating a ReadOnlyCollection, Warning: Elements in a ReadOnlyCollection are not inherently read-only"
---

# ReadOnlyCollections



## Creating a ReadOnlyCollection


### Using the Constructor

A `ReadOnlyCollection` is created by passing an existing `IList` object into the constructor:

```dotnet
var groceryList = new List<string> { "Apple", "Banana" };
var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);

```

### Using LINQ

Additionaly, LINQ provides an `AsReadOnly()` extension method for `IList` objects:

```dotnet
var readOnlyVersion = groceryList.AsReadOnly();

```

### Note

Typically, you want to maintain the source collection privately and allow public access to the `ReadOnlyCollection`. While you could create a `ReadOnlyCollection` from an in-line list, you would be unable to modify the collection after you created it.

```dotnet
var readOnlyGroceryList = new List<string> {"Apple", "Banana"}.AsReadOnly();
// Great, but you will not be able to update the grocery list because 
//  you do not have a reference to the source list anymore!

```

If you find yourself doing this, you may want to consider using another data structure, such as an `ImmutableCollection`.



## Updating a ReadOnlyCollection


A `ReadOnlyCollection` cannot be edited directly. Instead, the source collection is updated and the `ReadOnlyCollection` will reflect these changes. This is the key feature of the `ReadOnlyCollection`.

```dotnet
var groceryList = new List<string> { "Apple", "Banana" };

var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);

var itemCount = readOnlyGroceryList.Count;  // There are currently 2 items

//readOnlyGroceryList.Add("Candy");         // Compiler Error - Items cannot be added to a ReadOnlyCollection object
groceryList.Add("Vitamins");                // ..but they can be added to the original collection

itemCount = readOnlyGroceryList.Count;      // Now there are 3 items
var lastItem = readOnlyGroceryList.Last();  // The last item on the read only list is now "Vitamins"

```

[View Demo](https://dotnetfiddle.net/C8qQrS)



## Warning: Elements in a ReadOnlyCollection are not inherently read-only


If the source collection is of a type that is not immutable, elements accessed through a `ReadOnlyCollection` can be modified.

```dotnet
public class Item
{
    public string Name { get; set; }
    public decimal Price { get; set; }
}

public static void FillOrder()
{
    // An order is generated
    var order = new List<Item>
    {
        new Item { Name = "Apple", Price = 0.50m },
        new Item { Name = "Banana", Price = 0.75m },
        new Item { Name = "Vitamins", Price = 5.50m }
    };

    // The current sub total is $6.75
    var subTotal = order.Sum(item => item.Price);

    // Let the customer preview their order
    var customerPreview = new ReadOnlyCollection<Item>(order);

    // The customer can't add or remove items, but they can change 
    //   the price of an item, even though it is a ReadOnlyCollection
    customerPreview.Last().Price = 0.25m;

    // The sub total is now only $1.50!
    subTotal = order.Sum(item => item.Price);
}

```

[View Demo](https://dotnetfiddle.net/fXE66F)



#### Remarks


A `ReadOnlyCollection` provides a read-only view to an existing collection (the 'source collection').

Items are not directly added to or removed from a `ReadOnlyCollection`. Instead, they are added and removed from the source collection and the `ReadOnlyCollection` will reflect these changes to the source.

The number and order of elements inside a `ReadOnlyCollection` cannot be modified, but the properties of the elements can be and the methods can be called, assuming they are in scope.

Use a `ReadOnlyCollection` when you want to allow external code to view your collection without being able to modify it, but still be able to modify the collection yourself.

See Also

- `ObservableCollection<T>`
- `ReadOnlyObservableCollection<T>`

### ReadOnlyCollections vs ImmutableCollection

A `ReadOnlyCollection` differs from an `ImmutableCollection` in that you cannot edit an `ImmutableCollection` once you created it - it will always contain `n` elements, and they cannot be replaced or reordered. A `ReadOnlyCollection`, on the other hand, cannot be edited directly, but elements can still be added/removed/reordered using the source collection.

