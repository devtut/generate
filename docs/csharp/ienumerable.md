---
metaTitle: "IEnumerable"
description: "IEnumerable with custom Enumerator, IEnumerable<int>"
---

# IEnumerable


`IEnumerable` is the base interface for all non-generic collections like ArrayList that can be enumerated. `IEnumerator<T>` is the base interface for all generic enumerators like List<>.

`IEnumerable` is an interface which implements the method `GetEnumerator`. The `GetEnumerator` method returns an `IEnumerator` which provides options to iterate through the collection like foreach.



## IEnumerable with custom Enumerator


Implementing the IEnumerable interface allows classes to be enumerated in the same way as BCL collections. This requires extending the Enumerator class which tracks the state of the enumeration.

Other than iterating over a standard collection, examples include:

- Using ranges of numbers based on a function rather than a collection of objects
- Implementing different iteration algorithms over collections, like DFS or BFS on a graph collection

```cs
public static void Main(string[] args) {

    foreach (var coffee in new CoffeeCollection()) {
        Console.WriteLine(coffee);
    }
}

public class CoffeeCollection : IEnumerable {
    private CoffeeEnumerator enumerator;

    public CoffeeCollection() {
        enumerator = new CoffeeEnumerator();
    }

    public IEnumerator GetEnumerator() {
        return enumerator;
    }

    public class CoffeeEnumerator : IEnumerator {
        string[] beverages = new string[3] { "espresso", "macchiato", "latte" };
        int currentIndex = -1;

        public object Current {
            get {
                return beverages[currentIndex];
            }
        }

        public bool MoveNext() {
            currentIndex++;

            if (currentIndex < beverages.Length) {
                return true;
            }

            return false;
        }

        public void Reset() {
            currentIndex = 0;
        }
    }
}

```



## IEnumerable<int>


In its most basic form, an object that implements IEnumerable represents a series of objects. The objects in question can be iterated using the c# `foreach` keyword.

In the example below, the object `sequenceOfNumbers` implements IEnumerable. It represents a series of integers. The `foreach` loop iterates through each in turn.

```cs
int AddNumbers(IEnumerable<int> sequenceOfNumbers) {
    int returnValue = 0;
    foreach(int i in sequenceOfNumbers) {
        returnValue += i;
    }
    return returnValue;
}

```



#### Remarks


IEnumerable is the base interface for all non-generic collections that can be enumerated

