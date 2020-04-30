---
metaTitle: "Indexer"
description: "A simple indexer, Overloading the indexer to create a SparseArray, Indexer with 2 arguments and interface"
---

# Indexer



## A simple indexer


```cs
class Foo
{
    private string[] cities = new[] { "Paris", "London", "Berlin" };

    public string this[int index]
    {
        get {
            return cities[index];
        }
        set {
            cities[index] = value;
        }
    }
}

```

**Usage:**

```cs

   var foo = new Foo();

    // access a value    
    string berlin = foo[2];

    // assign a value
    foo[0] = "Rome";

```

[View Demo](https://dotnetfiddle.net/I1usLs)



## Overloading the indexer to create a SparseArray


By overloading the indexer you can create a class that looks and feels like an array but isn't. It will have O(1) get and set methods, can access an element at index 100, and yet still have the size of the elements inside of it. The SparseArray class

```cs
class SparseArray
    {
        Dictionary<int, string> array = new Dictionary<int, string>();

        public string this[int i]
        {
            get
            {
                if(!array.ContainsKey(i))
                {
                    return null;
                }
                return array[i];
            }
            set
            {
                if(!array.ContainsKey(i))
                    array.Add(i, value);
            }
        }
    }

```



## Indexer with 2 arguments and interface


```cs
interface ITable { 
    // an indexer can be declared in an interface
    object this[int x, int y] { get; set; }
}

class DataTable : ITable
{
    private object[,] cells = new object[10, 10];

    /// <summary>
    /// implementation of the indexer declared in the interface
    /// </summary>
    /// <param name="x">X-Index</param>
    /// <param name="y">Y-Index</param>
    /// <returns>Content of this cell</returns>
    public object this[int x, int y]
    {
        get
        {
            return cells[x, y];
        }
        set
        {
            cells[x, y] = value;
        }
    }
}

```



#### Syntax


- public ReturnType this[IndexType index] { get { ...  } set {  ... }}



#### Remarks


Indexer allows array-like syntax to access a property of an object with an index.

- Can be used on a class, struct or interface.
- Can be overloaded.
- Can use multiple parameters.
- Can be used to access and set values.
- Can use any type for it's index.

