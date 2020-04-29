---
metaTitle: "Sets"
description: "Initialization, Basics of Set, Types and Usage of Sets, Declaring a HashSet with values, Create a list from an existing Set, Eliminating duplicates using Set"
---

# Sets



## Initialization


A Set is a Collection that cannot contain duplicate elements. It models the mathematical set abstraction.

`Set` have its implementation in various classes like `HashSet`, `TreeSet`, `LinkedHashSet`.

For example:

**HashSet:**

```java
Set<T> set = new HashSet<T>();

```

Here `T` can be `String`, `Integer` or any other **object**. **HashSet** allows for quick lookup of O(1) but does not sort the data added to it and loses the insertion order of items.

**TreeSet:**

It stores data in a sorted manner sacrificing some speed for basic operations which take O(lg(n)).  It does not maintain the insertion order of items.

```java
TreeSet<T> sortedSet = new TreeSet<T>();

```

**LinkedHashSet:**

It is a linked list implementation of `HashSet`  Once can iterate over the items in the order they were added.  Sorting is not provided for its contents. O(1) basic operations are provided, however there is higher cost than `HashSet` in maintaining the backing linked list.

```java
LinkedHashSet<T> linkedhashset = new LinkedHashSet<T>();

```



## Basics of Set


**What is a Set?**

A set is a data structure which contains a set of elements with an important property that no two elements in the set are equal.

**Types of Set:**

1. **HashSet:** A set backed by a hash table (actually a HashMap instance)
1. **Linked HashSet:** A Set backed by Hash table and linked list, with predictable iteration order
1. **TreeSet:** A NavigableSet implementation based on a TreeMap.

**Creating a set**

```java
Set<Integer> set = new HashSet<Integer>(); // Creates an empty Set of Integers

Set<Integer> linkedHashSet = new LinkedHashSet<Integer>(); //Creates a empty Set of Integers, with predictable iteration order

```

**Adding elements to a Set**

Elements can be added to a set using the `add()` method

```

set.add(12); //  - Adds element 12 to the set
 set.add(13); //  - Adds element 13 to the set

```

Our set after executing this method:

```java
set = [12,13]

```

**Delete all the elements of a Set**

```java
set.clear();  //Removes all objects from the collection.

```

After this set will be:

```java
set = []

```

**Check whether an element is part of the Set**

Existence of an element in the set can be checked using the `contains()` method

```java
set.contains(0);  //Returns true if a specified object is an element within the set.

```

**Output:** `False`

**Check whether a Set is empty**

`isEmpty()` method can be used to check whether a Set is empty.

```java
set.isEmpty();  //Returns true if the set has no elements

```

**Output:** True

**Remove an element from the Set**

```

set.remove(0); // Removes first occurrence of a specified object from the collection

```

**Check the Size of the Set**

```java
set.size(); //Returns the number of elements in the collection

```

**Output:** 0



## Types and Usage of Sets


Generally, sets are a type of collection which stores unique values. Uniqueness is determined by the `equals()` and `hashCode()` methods.

Sorting is determined by the type of set.

### `HashSet` - Random Sorting

```java
Set<String> set = new HashSet<> ();
set.add("Banana");
set.add("Banana");
set.add("Apple");
set.add("Strawberry");

// Set Elements: ["Strawberry", "Banana", "Apple"]

```

### `LinkedHashSet` - Insertion Order

```java
Set<String> set = new LinkedHashSet<> ();
set.add("Banana");
set.add("Banana");
set.add("Apple");
set.add("Strawberry");

// Set Elements: ["Banana", "Apple", "Strawberry"]

```

### `TreeSet` - By `compareTo()` or `Comparator`

```java
Set<String> set = new TreeSet<> ();
set.add("Banana");
set.add("Banana");
set.add("Apple");
set.add("Strawberry");

// Set Elements: ["Apple", "Banana", "Strawberry"]

```

```java
Set<String> set = new TreeSet<> ((string1, string2) -> string2.compareTo(string1));
set.add("Banana");
set.add("Banana");
set.add("Apple");
set.add("Strawberry");

// Set Elements: ["Strawberry", "Banana", "Apple"]

```



## Declaring a HashSet with values


You can create a new class that inherits from HashSet:

```java
Set<String> h = new HashSet<String>() {{
    add("a");
    add("b");
}};

```

One line solution:

```java
Set<String> h = new HashSet<String>(Arrays.asList("a", "b"));

```

Using guava:

```java
Sets.newHashSet("a", "b", "c")

```

Using Streams:

```java
Set<String> set3 = Stream.of("a", "b", "c").collect(toSet());

```



## Create a list from an existing Set


**Using a new List**

```java
List<String> list = new ArrayList<String>(listOfElements);

```

**Using List.addAll() method**

```

   Set<String> set = new HashSet<String>();
    set.add("foo");
    set.add("boo");
    
    List<String> list = new ArrayList<String>();
    list.addAll(set);

```

**Using Java 8 Steam API**

```java
List<String> list = set.stream().collect(Collectors.toList());

```



## Eliminating duplicates using Set


Suppose you have a collection `elements`, and you want to create another collection containing the same elements but with all **duplicates eliminated**:

```java
Collection<Type> noDuplicates = new HashSet<Type>(elements);

```

**Example**:

```java
List<String> names = new ArrayList<>(
        Arrays.asList("John", "Marco", "Jenny", "Emily", "Jenny", "Emily", "John"));
Set<String> noDuplicates = new HashSet<>(names);
System.out.println("noDuplicates = " + noDuplicates);

```

**Output**:

```java
noDuplicates = [Marco, Emily, John, Jenny]

```

