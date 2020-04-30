---
metaTitle: "Collection Initializers"
description: "Collection initializers, C# 6 Index Initializers, Collection initializers in custom classes, Using collection initializer inside object initializer, Collection Initializers with Parameter Arrays"
---

# Collection Initializers



## Collection initializers


Initialize a collection type with values:

```cs
var stringList = new List<string>
{
    "foo",
    "bar",
};

```

Collection initializers are syntactic sugar for `Add()` calls. Above code is equivalent to:

```cs
var temp = new List<string>();
temp.Add("foo");
temp.Add("bar");
var stringList = temp;

```

Note that the intialization is done atomically using a temporary variable, to avoid race conditions.

For types that offer multiple parameters in their `Add()` method, enclose the comma-separated arguments in curly braces:

```cs
var numberDictionary = new Dictionary<int, string>
{
    { 1, "One" },
    { 2, "Two" },
};

```

This is equivalent to:

```cs
var temp = new Dictionary<int, string>();
temp.Add(1, "One");
temp.Add(2, "Two");
var numberDictionarynumberDictionary = temp;

```



## C# 6 Index Initializers


Starting with C# 6, collections with indexers can be initialized by specifying the index to assign in square brackets, followed by an equals sign, followed by the value to assign.

### Dictionary Initialization

An example of this syntax using a Dictionary:

```cs
var dict = new Dictionary<string, int>
{
    ["key1"] = 1,
    ["key2"] = 50
};

```

This is equivalent to:

```cs
var dict = new Dictionary<string, int>();
dict["key1"] = 1;
dict["key2"] = 50

```

The collection initializer syntax to do this before C# 6 was:

```cs
var dict = new Dictionary<string, int>
{
    { "key1", 1 },
    { "key2", 50 }
};

```

Which would correspond to:

```cs
var dict = new Dictionary<string, int>();
dict.Add("key1", 1);
dict.Add("key2", 50);

```

So there is a significant difference in functionality, as the new syntax uses the **indexer** of the initialized object to assign values instead of using its `Add()` method. This means the new syntax only requires a publicly available indexer, and works for any object that has one.

```cs
public class IndexableClass
{
    public int this[int index]
    {
        set 
        { 
            Console.WriteLine("{0} was assigned to index {1}", value, index);
        }
    }
}

var foo = new IndexableClass
{
    [0] = 10,
    [1] = 20
}

```

This would output:

> 
<p>`10 was assigned to index 0`<br/>
`20 was assigned to index 1`</p>




## Collection initializers in custom classes


To make a class support collection initializers, it must implement `IEnumerable` interface and have at least one `Add` method. Since C# 6, any collection implementing `IEnumerable` can be extended with custom `Add` methods using extension methods.

```cs
class Program
{
    static void Main()
    {
        var col = new MyCollection {
            "foo",
            { "bar", 3 },
            "baz",
            123.45d,
        };
    }
}

class MyCollection : IEnumerable
{
    private IList list = new ArrayList();

    public void Add(string item)
    {
        list.Add(item)
    }

    public void Add(string item, int count)
    {
        for(int i=0;i< count;i++) {
            list.Add(item);
        }
    }

    public IEnumerator GetEnumerator()
    {
        return list.GetEnumerator();
    }
}

static class MyCollectionExtensions
{
    public static void Add(this MyCollection @this, double value) => 
        @this.Add(value.ToString());
}

```



## Using collection initializer inside object initializer


```cs
public class Tag
{
    public IList<string> Synonyms { get; set; }
}

```

`Synonyms` is a collection-type property. When the `Tag` object is created using object initializer syntax, `Synonyms` can also be initialized with collection initializer syntax:

```cs
Tag t = new Tag 
{
    Synonyms = new List<string> {"c#", "c-sharp"}
};

```

The collection property can be readonly and still support collection initializer syntax. Consider this modified example (`Synonyms` property now has a private setter):

```cs
public class Tag
{
    public Tag()
    {
        Synonyms = new List<string>();
    }
    
    public IList<string> Synonyms { get; private set; }
}

```

A new `Tag` object can be created like this:

```cs
Tag t = new Tag 
{
    Synonyms = {"c#", "c-sharp"}
};

```

This works because collection initializers are just syntatic sugar over calls to `Add()`. There's no new list being created here, the compiler is just generating calls to `Add()` on the exiting object.



## Collection Initializers with Parameter Arrays


You can mix normal parameters and parameter arrays:

```cs
public class LotteryTicket : IEnumerable{
    public int[] LuckyNumbers;
    public string UserName;

    public void Add(string userName, params int[] luckyNumbers){
        UserName = userName;
        Lottery = luckyNumbers;
    }
}

```

This syntax is now possible:

```cs
var Tickets = new List<LotteryTicket>{
    {"Mr Cool"  , 35663, 35732, 12312, 75685},
    {"Bruce"    , 26874, 66677, 24546, 36483, 46768, 24632, 24527},
    {"John Cena", 25446, 83356, 65536, 23783, 24567, 89337}
}

```



#### Remarks


The only requirement for an object to be initialized using this syntactic sugar is that the type implements `System.Collections.IEnumerable` and the `Add` method. Although we call it a collection initializer, the object does **not** have to be an collection.

