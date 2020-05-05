---
metaTitle: ".NET Framework - Dictionaries"
description: "Initializing a Dictionary with a Collection Initializer, Adding to a Dictionary, Getting a value from a dictionary, Make a Dictionary<string, T> with Case-Insensivitve keys., Enumerating a Dictionary, ConcurrentDictionary<TKey, TValue> (from .NET 4.0), IEnumerable to Dictionary (≥ .NET 3.5), Removing from a Dictionary, ContainsKey(TKey), Dictionary to List, ConcurrentDictionary augmented with Lazy'1 reduces duplicated computation"
---

# Dictionaries



## Initializing a Dictionary with a Collection Initializer


```dotnet
// Translates to `dict.Add(1, "First")` etc.
var dict = new Dictionary<int, string>()
{
    { 1, "First" },
    { 2, "Second" },
    { 3, "Third" }
};

// Translates to `dict[1] = "First"` etc.
// Works in C# 6.0.
var dict = new Dictionary<int, string>()
{
    [1] = "First",
    [2] = "Second",
    [3] = "Third"
};

```



## Adding to a Dictionary


```dotnet
Dictionary<int, string> dict = new Dictionary<int, string>();
dict.Add(1, "First");
dict.Add(2, "Second");

// To safely add items (check to ensure item does not already exist - would throw)
if(!dict.ContainsKey(3))
{
   dict.Add(3, "Third");
}

```

Alternatively they can be added/set via the an indexer.  (An indexer internally looks like a property, having a get and set, but takes a parameter of any type which is specified between the brackets) :

```dotnet
Dictionary<int, string> dict = new Dictionary<int, string>();
dict[1] = "First";
dict[2] = "Second";
dict[3] = "Third";

```

Unlike the `Add` method which throws an exception, if a key is already contained in the dictionary, the indexer just replaces the existing value.

For thread-safe dictionary use `ConcurrentDictionary<TKey, TValue>`:

```dotnet
var dict = new ConcurrentDictionary<int, string>();
dict.AddOrUpdate(1, "First", (oldKey, oldValue) => "First");

```



## Getting a value from a dictionary


Given this setup code:

```dotnet
var dict = new Dictionary<int, string>()
{
    { 1, "First" },
    { 2, "Second" },
    { 3, "Third" }
};

```

You may want to read the value for the entry with key 1. If key doesn't exist getting a value will throw `KeyNotFoundException`, so you may want to first check for that with `ContainsKey`:

```dotnet
if (dict.ContainsKey(1))
    Console.WriteLine(dict[1]);

```

This has one disadvantage: you will search through your dictionary twice (once to check for existence and one to read the value). For a large dictionary this can impact performance. Fortunately both operations can be performed together:

```dotnet
string value;
if (dict.TryGetValue(1, out value))
    Console.WriteLine(value);

```



## Make a Dictionary<string, T> with Case-Insensivitve keys.


```dotnet
var MyDict = new Dictionary<string,T>(StringComparison.InvariantCultureIgnoreCase)

```



## Enumerating a Dictionary


You can enumerate through a Dictionary in one of 3 ways:

**Using KeyValue pairs**

```dotnet
Dictionary<int, string> dict = new Dictionary<int, string>();
foreach(KeyValuePair<int, string> kvp in dict) 
{
   Console.WriteLine("Key : " + kvp.Key.ToString() + ", Value : " + kvp.Value);
}

```

**Using Keys**

```dotnet
Dictionary<int, string> dict = new Dictionary<int, string>();
foreach(int key in dict.Keys)
{
    Console.WriteLine("Key : " + key.ToString() + ", Value : " + dict[key]);
}

```

**Using Values**

```dotnet
Dictionary<int, string> dict = new Dictionary<int, string>();
foreach(string s in dict.Values)
{
    Console.WriteLine("Value : " + s);
}

```



## ConcurrentDictionary<TKey, TValue> (from .NET 4.0)


> 
<p>Represents a thread-safe collection of key/value pairs that can be
accessed by multiple threads concurrently.</p>


### Creating an instance

Creating an instance works pretty much the same way as with `Dictionary<TKey, TValue>`, e.g.:

```dotnet
var dict = new ConcurrentDictionary<int, string>();

```

### Adding or Updating

You might be surprised, that there is no `Add` method, but instead there is `AddOrUpdate` with 2 overloads:

(1) `AddOrUpdate(TKey key, TValue, Func<TKey, TValue, TValue> addValue)` - **Adds a key/value pair if the key does not already exist, or updates a key/value pair by using the specified function if the key already exists.**

(2) `AddOrUpdate(TKey key, Func<TKey, TValue> addValue, Func<TKey, TValue, TValue> updateValueFactory)` - **Uses the specified functions to add a key/value pair to the if the key does not already exist, or to update a key/value pair if the key already exists.**

Adding or updating a value, no matter what was the value if it was already present for given key (1):

```dotnet
string addedValue = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => "First");

```

Adding or updating a value, but now altering the value in update, based on the previous value (1):

```dotnet
string addedValue2 = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => $"{valueOld} Updated");

```

Using the overload (2) we can also add new value using a factory:

```dotnet
string addedValue3 = dict.AddOrUpdate(1, (key) => key == 1 ? "First" : "Not First", (updateKey, valueOld) => $"{valueOld} Updated");

```

### Getting value

Getting a value is the same as with the `Dictionary<TKey,TValue>`:

```dotnet
string value = null;
bool success = dict.TryGetValue(1, out value);

```

### Getting or Adding a value

There are two mehod overloads, that will **get or add** a value in a thread-safe manner.

Get value with key 2, or add value "Second" if the key is not present:

```dotnet
string theValue = dict.GetOrAdd(2, "Second");

```

Using a factory for adding a value, if value is not present:

```dotnet
string theValue2 = dict.GetOrAdd(2, (key) => key == 2 ? "Second" : "Not Second." );

```



## IEnumerable to Dictionary (≥ .NET 3.5)


Create a [Dictionary<TKey, TValue>](https://msdn.microsoft.com/en-us/library/xfhwa508(v=vs.100).aspx) from an [IEnumerable<T>](https://msdn.microsoft.com/en-us/library/9eekhta0(v=vs.100).aspx):

```dotnet
using System;
using System.Collections.Generic;
using System.Linq;

```

****

```dotnet
public class Fruits
{
    public int Id { get; set; }
    public string Name { get; set; }
}

```

****

```dotnet
var fruits = new[]
{ 
    new Fruits { Id = 8 , Name = "Apple" },
    new Fruits { Id = 3 , Name = "Banana" },
    new Fruits { Id = 7 , Name = "Mango" },
};


// Dictionary<int, string>                  key      value
var dictionary = fruits.ToDictionary(x => x.Id, x => x.Name);

```



## Removing from a Dictionary


Given this setup code:

```dotnet
var dict = new Dictionary<int, string>()
{
    { 1, "First" },
    { 2, "Second" },
    { 3, "Third" }
};

```

Use the `Remove` method to remove a key and its associated value.

```dotnet
bool wasRemoved = dict.Remove(2);

```

Executing this code removes the key `2` and it's value from the dictionary. `Remove` returns a boolean value indicating whether the specified key was found and removed from the dictionary. If the key does not exist in the dictionary, nothing is removed from the dictionary, and false is returned (no exception is thrown).

It's **incorrect** to try and remove a key by setting the value for the key to `null`.

```dotnet
dict[2] = null; // WRONG WAY TO REMOVE!

```

This will not remove the key. It will just replace the previous value with a value of `null`.

To remove all keys and values from a dictionary, use the `Clear` method.

```dotnet
dict.Clear();

```

After executing `Clear` the dictionary's `Count` will be 0, but the internal capacity remains unchanged.



## ContainsKey(TKey)


To check if a `Dictionary` has an specifique key, you can call the method [`ContainsKey(TKey)`](https://msdn.microsoft.com/library/htszx2dy(v=vs.110).aspx) and provide the key of `TKey` type. The method returns a `bool` value when the key exists on the dictionary. For sample:

```dotnet
var dictionary = new Dictionary<string, Customer>()
{
   {"F1", new Customer() { FirstName = "Felipe", ... } },
   {"C2", new Customer() { FirstName = "Carl", ... } },
   {"J7", new Customer() { FirstName = "John", ... } },
   {"M5", new Customer() { FirstName = "Mary", ... } },
};

```

And check if a `C2` exists on the Dictionary:

```dotnet
if (dictionary.ContainsKey("C2")) 
{
   // exists
}

```

The ContainsKey method is available on the generic version [`Dictionary<TKey, TValue>`](https://msdn.microsoft.com/library/htszx2dy(v=vs.110).aspx).



## Dictionary to List


Creating a list of KeyValuePair:

```dotnet
Dictionary<int, int> dictionary = new Dictionary<int, int>();
List<KeyValuePair<int, int>> list = new List<KeyValuePair<int, int>>();
list.AddRange(dictionary);

```

Creating a list of keys:

```dotnet
Dictionary<int, int> dictionary = new Dictionary<int, int>();
List<int> list = new List<int>();
list.AddRange(dictionary.Keys);

```

Creating a list of values:

```dotnet
Dictionary<int, int> dictionary = new Dictionary<int, int>();
List<int> list = new List<int>();
list.AddRange(dictionary.Values);

```



## ConcurrentDictionary augmented with Lazy'1 reduces duplicated computation


### Problem

ConcurrentDictionary shines when it comes to instantly returning of existing keys from cache, mostly lock free, and contending on a granular level.
But what if the object creation is really expensive, outweighing the cost of context switching, and some cache misses occur?

If the same key is requested from multiple threads, one of the objects resulting from colliding operations will be eventually added to the collection, and the others will be thrown away, wasting the CPU resource to create the object and memory resource to store the object temporarily. Other resources could be wasted as well. This is really bad.

### Solution

We can combine `ConcurrentDictionary<TKey, TValue>` with `Lazy<TValue>`. The idea is that ConcurrentDictionary GetOrAdd method can only return the value which was actually added to the collection. The loosing Lazy objects could be wasted in this case too, but that's not much problem, as the Lazy object itself is relatively unexpensive. The Value property of the losing Lazy is never requested, because we are smart to only request the Value property of the one actually added to the collection - the one returned from the GetOrAdd method:

```dotnet
public static class ConcurrentDictionaryExtensions
{
    public static TValue GetOrCreateLazy<TKey, TValue>(
        this ConcurrentDictionary<TKey, Lazy<TValue>> d,
        TKey key,
        Func<TKey, TValue> factory)
    {
        return
            d.GetOrAdd(
                key,
                key1 =>
                    new Lazy<TValue>(() => factory(key1),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value;
    }
}

```

Caching of XmlSerializer objects can be particularly expensive, and there is a lot of contention at the application startup too. And there is more to this: if those are custom serializers, there will be a memory leak too for the rest of the process lifecycle. The only benefit of the ConcurrentDictionary in this case is that for the rest of the process lifecycle there will be no locks, but application startup and memory usage would be inacceptable. This is a job for our ConcurrentDictionary, augmented with Lazy:

```dotnet
private ConcurrentDictionary<Type, Lazy<XmlSerializer>> _serializers =
    new ConcurrentDictionary<Type, Lazy<XmlSerializer>>();

public XmlSerializer GetSerialier(Type t)
{
    return _serializers.GetOrCreateLazy(t, BuildSerializer);
}

private XmlSerializer BuildSerializer(Type t)
{
    throw new NotImplementedException("and this is a homework");
}

```

