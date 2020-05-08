---
metaTitle: "Android - How to use SparseArray"
description: "Basic example using SparseArray"
---

# How to use SparseArray


A [`SparseArray`](https://developer.android.com/reference/android/util/SparseArray.html) is an alternative for a `Map`. A `Map` requires its keys to be objects. The phenomenon of autoboxing occurs when we want to use a primitive `int` value as key. The compiler automatically converts primitive values to their boxed types (e.g. `int` to `Integer`). The difference in memory footprint is noticeable: `int` uses 4 bytes, `Integer` uses 16 bytes. A `SparseArray` uses `int` as key value.



## Basic example using SparseArray


```java
class Person {
    String name;

    public Person(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Person person = (Person) o;

        return name != null ? name.equals(person.name) : person.name == null;
    }

    @Override
    public int hashCode() {
        return name != null ? name.hashCode() : 0;
    }

    @Override
    public String toString() {
        return "Person{" +
                "name='" + name + '\'' +
                '}';
    }
}

final Person steve = new Person("Steve");
Person[] persons = new Person[] { new Person("John"), new Person("Gwen"), steve, new Person("Rob") };
int[] identifiers = new int[] {1234, 2345, 3456, 4567};

final SparseArray<Person> demo = new SparseArray<>();

// Mapping persons to identifiers.
for (int i = 0; i < persons.length; i++) {
    demo.put(identifiers[i], persons[i]);
}

// Find the person with identifier 1234.
Person id1234 = demo.get(1234); // Returns John.

// Find the person with identifier 6410.
Person id6410 = demo.get(6410); // Returns null.

// Find the 3rd person.
Person third = demo.valueAt(3); // Returns Rob.

// Find the 42th person.
//Person fortysecond = demo.valueAt(42); // Throws ArrayIndexOutOfBoundsException.

// Remove the last person.
demo.removeAt(demo.size() - 1); // Rob removed.

// Remove the person with identifier 1234.
demo.delete(1234); // John removed.

// Find the index of Steve.
int indexOfSteve = demo.indexOfValue(steve);

// Find the identifier of Steve.
int identifierOfSteve = demo.keyAt(indexOfSteve);

```

[Tutorial on YouTube](https://www.youtube.com/watch?v=I16lz26WyzQ)



#### Remarks


Advantage :

- Less memory usage(because of the primitive keys).
- No auto-boxing.

Disadvantage :

- SparseArray uses binary search for find value ( O(log n) ), so its may not be the best solution if have to work with large number of element(use HashMap).

There are several variants of the family like :
-SparseArray          <Integer, Object>
-SparseBooleanArray   <Integer, Boolean>
-SparseIntArray       <Integer, Integer>
-SparseLongArray      <Integer, Long>
-LongSparseArray      <Long, Object>
-LongSparseLongArray  <Long, Long>

SparseArray operations

<li>
<p>adding element
-- put(int, x): Adds a mapping from the specified key to the specified value, replacing the previous mapping from the specified key if there was one.
-- append(int, x): Puts a key/value pair into the array, optimizing for the case where the key is greater than all existing keys in the array.
You should use append() in case of sequential keys to optimize performance. Otherwise put() is fine.</p>
</li>
<li>
<p>removing element
-- delete(int): Removes the mapping from the specified key, if there was any.
-- removeAt(int): Removes the mapping at the given index.
-- removeAtRange(int, int): Remove a range of mappings as a batch.</p>
</li>
<li>
<p>accessing element
-- get(int): Gets the int mapped from the specified key, or 0 if no such mapping has been made.
-- get(int, E): Gets the int mapped from the specified key, or the specified value if no such mapping has been made.
-- valueAt(int): Given an index in the range 0...size()-1, returns the value from the indexth key-value mapping that this SparseIntArray stores. Indices are ordered in ascending order.</p>
</li>
<li>
<p>index/key search
-- keyAt(int): Given an index in the range 0...size()-1, returns the key from the indexth key-value mapping that this SparseIntArray stores. Indices are ordered in ascending order.
-- valueAt(int): Given an index in the range 0...size()-1, returns the value from the indexth key-value mapping that this SparseIntArray stores. Indices are ordered in ascending order.
-- indexOfKey(int): Returns the index for which keyAt(int) would return the specified key, or a negative number if the specified key is not mapped.
-- indexOfValue(E): Returns an index for which valueAt(int) would return the specified key, or a negative number if no keys map to the specified value. Beware that this is a linear search, unlike lookups by key, and that multiple keys can map to the same value and this will find only one of them.
The difference in their memory footprint is noticeable: the int uses 4 bytes, the Integer uses 16 bytes.SparseArray uses int as key value.</p>
</li>

