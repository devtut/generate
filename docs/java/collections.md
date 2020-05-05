---
metaTitle: "Java - Collections"
description: "Removing items from a List within a loop, Constructing collections from existing data, Declaring an ArrayList and adding objects, Iterating over Collections, Immutable Empty Collections, Unmodifiable Collection, Pitfall: concurrent modification exceptions, Sub Collections, Join lists, Removing matching items from Lists using Iterator., Collections and Primitive Values, Creating your own Iterable structure for use with Iterator or for-each loop."
---

# Collections


The collections framework in `java.util` provides a number of generic classes for sets of data with functionality that can't be provided by regular arrays.

Collections framework contains interfaces for `Collection<O>`, with main sub-interfaces `List<O>` and `Set<O>`, and mapping collection `Map<K,V>`. Collections are the root interface and are being implemented by many other collection frameworks.



## Removing items from a List within a loop


It is tricky to remove items from a list while within a loop, this is due to the fact that the index and length of the list gets changed.

Given the following list, here are some examples that will give an unexpected result and some that will give the correct result.

```java
List<String> fruits = new ArrayList<String>();
fruits.add("Apple");
fruits.add("Banana");
fruits.add("Strawberry");

```

### INCORRECT

### Removing in iteration of `for` statement **Skips "Banana":**

The code sample will only print `Apple` and `Strawberry`. `Banana` is skipped because it moves to index `0` once `Apple` is deleted, but at the same time `i` gets incremented to `1`.

```java
for (int i = 0; i < fruits.size(); i++) {
    System.out.println (fruits.get(i)); 
    if ("Apple".equals(fruits.get(i))) {
         fruits.remove(i);
    }     
}

```

### Removing in the enhanced `for` statement **Throws Exception:**

Because of iterating over collection and modifying it at the same time.

> 
<p>Throws:
java.util.ConcurrentModificationException</p>


```java
for (String fruit : fruits) { 
    System.out.println(fruit);
    if ("Apple".equals(fruit)) {
        fruits.remove(fruit);
    }
}

```

### CORRECT

### Removing in while loop using an `Iterator`

```java
Iterator<String> fruitIterator = fruits.iterator();
while(fruitIterator.hasNext()) {     
    String fruit = fruitIterator.next();     
    System.out.println(fruit);
    if ("Apple".equals(fruit)) {
        fruitIterator.remove();
    } 
}

```

The `Iterator` interface has a `remove()` method built in just for this case. However, this method is [marked as "optional"](https://docs.oracle.com/javase/8/docs/api/java/util/Iterator.html#remove--) in the documentation, and it might throw an `UnsupportedOperationException`.

> 
<p>Throws:
UnsupportedOperationException - if the remove operation is not supported by this iterator</p>


Therefore, it is advisable to check the documentation to make sure this operation is supported (in practice, unless the collection is an immutable one obtained through a 3rd party library or the use of one of the `Collections.unmodifiable...()` method, the operation is almost always supported).

While using an `Iterator` a `ConcurrentModificationException` is thrown when the `modCount` of the `List` is changed from when the `Iterator` was created. This could have happened in the same thread or in a multi-threaded application sharing the same list.

A `modCount` is an `int` variable which counts the number of times this list has been structurally modified. A structural change essentially means an `add()` or `remove()` operation being invoked on `Collection` object (changes made by `Iterator` are not counted). When the `Iterator` is created, it stores this `modCount` and on every iteration of the `List` checks if the current `modCount` is same as and when the `Iterator` was created. If there is a change in the `modCount` value it throws a `ConcurrentModificationException`.

Hence for the above-declared list, an operation like below will not throw any exception:

```java
Iterator<String> fruitIterator = fruits.iterator();
fruits.set(0, "Watermelon");
while(fruitIterator.hasNext()){
    System.out.println(fruitIterator.next());
}

```

But adding a new element to the `List` after initializing an `Iterator` will throw a `ConcurrentModificationException`:

```java
Iterator<String> fruitIterator = fruits.iterator();
fruits.add("Watermelon");
while(fruitIterator.hasNext()){
    System.out.println(fruitIterator.next());    //ConcurrentModificationException here
}

```

### Iterating backwards

```java
for (int i = (fruits.size() - 1); i >=0; i--) {
    System.out.println (fruits.get(i));
    if ("Apple".equals(fruits.get(i))) {
         fruits.remove(i);
    }
}

```

This does not skip anything. The downside of this approach is that the output is reverse. However, in most cases where you remove items that will not matter. You should never do this with [`LinkedList`](https://docs.oracle.com/javase/8/docs/api/java/util/LinkedList.html).

### Iterating forward, adjusting the loop index

```java
for (int i = 0; i < fruits.size(); i++) {
    System.out.println (fruits.get(i)); 
    if ("Apple".equals(fruits.get(i))) {
         fruits.remove(i);
         i--;
    }     
}

```

This does not skip anything. When the `i`th element is removed from the `List`, the element originally positioned at index `i+1` becomes the new `i`th element. Therefore, the loop can decrement `i` in order for the next iteration to process the next element, without skipping.

### Using a "should-be-removed" list

```java
ArrayList shouldBeRemoved = new ArrayList();
for (String str : currentArrayList) {
    if (condition) {
        shouldBeRemoved.add(str);
    }
}
currentArrayList.removeAll(shouldBeRemoved);

```

This solution enables the developer to check if the correct elements are removed in a cleaner way.

**In Java 8 the following alternatives are possible. These are cleaner and more straight forward if the removing does not have to happen in a loop.**

### Filtering a Stream

A `List` can be streamed and filtered. A proper filter can be used to remove all undesired elements.

```java
List<String> filteredList = 
    fruits.stream().filter(p -> !"Apple".equals(p)).collect(Collectors.toList());

```

Note that unlike all the other examples here, this example produces a new `List` instance and keeps the original `List` unchanged.

### Using `removeIf`

Saves the overhead of constructing a stream if all that is needed is to remove a set of items.

```java
fruits.removeIf(p -> "Apple".equals(p));

```



## Constructing collections from existing data


### Standard Collections

### Java Collections framework

A simple way to construct a `List` from individual data values is to use `java.utils.Arrays` method `Arrays.asList`:

```java
List<String> data = Arrays.asList("ab", "bc", "cd", "ab", "bc", "cd");

```

All standard collection implementations provide constructors that take another collection as an argument adding all elements to the new collection at the time of construction:

```java
List<String> list = new ArrayList<>(data); // will add data as is
Set<String> set1 = new HashSet<>(data); // will add data keeping only unique values
SortedSet<String> set2 = new TreeSet<>(data); // will add data keeping unique values and sorting
Set<String> set3 = new LinkedHashSet<>(data); // will add data keeping only unique values and preserving the original order

```

### Google Guava Collections framework

Another great framework is `Google Guava` that is amazing utility class (providing convenience static methods) for construction of different types of standard collections `Lists` and `Sets`:

```

import com.google.common.collect.Lists;
 import com.google.common.collect.Sets;
 ...
 List<String> list1 = Lists.newArrayList("ab", "bc", "cd");
 List<String> list2 = Lists.newArrayList(data);
 Set<String> set4 = Sets.newHashSet(data);
 SortedSet<String> set5 = Sets.newTreeSet("bc", "cd", "ab", "bc", "cd");

```

### Mapping Collections

### Java Collections framework

Similarly for maps, given a `Map<String, Object> map` a new map can be constructed with all elements as follows:

```java
Map<String, Object> map1 = new HashMap<>(map);
SortedMap<String, Object> map2 = new TreeMap<>(map);

```

### Apache Commons Collections framework

Using `Apache Commons` you can create Map using array in `ArrayUtils.toMap`
as well as `MapUtils.toMap`:

```

import org.apache.commons.lang3.ArrayUtils;
 ...
 // Taken from org.apache.commons.lang.ArrayUtils#toMap JavaDoc

 // Create a Map mapping colors.
 Map colorMap = MapUtils.toMap(new String[][] {{
     {"RED", "#FF0000"},
     {"GREEN", "#00FF00"},
     {"BLUE", "#0000FF"}});

```

Each element of the array must be either a Map.Entry or an Array, containing at least two elements, where the first element is used as key and the second as value.

### Google Guava Collections framework

Utility class from `Google Guava` framework is named `Maps`:

```

import com.google.common.collect.Maps;
 ...
 void howToCreateMapsMethod(Function<? super K,V> valueFunction,
           Iterable<K> keys1, 
           Set<K> keys2, 
           SortedSet<K> keys3) {
     ImmutableMap<K, V> map1 = toMap(keys1, valueFunction); // Immutable copy
     Map<K, V> map2 = asMap(keys2, valueFunction); // Live Map view
     SortedMap<K, V> map3 = toMap(keys3, valueFunction); // Live Map view
 }

```

Using [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html),

```java
Stream.of("xyz", "abc").collect(Collectors.toList());

```

or

```java
Arrays.stream("xyz", "abc").collect(Collectors.toList());

```



## Declaring an ArrayList and adding objects


We can create an `ArrayList` (following the `List` interface):

```java
List aListOfFruits = new ArrayList();

```

```java
List<String> aListOfFruits = new ArrayList<String>();

```

```java
List<String> aListOfFruits = new ArrayList<>();

```

Now, use the method `add` to add a `String`:

```java
aListOfFruits.add("Melon");
aListOfFruits.add("Strawberry");

```

In the above example, the `ArrayList` will contain the `String` "Melon" at index 0 and the `String` "Strawberry" at index 1.

Also we can add multiple elements with `addAll(Collection<? extends E> c)` method

```java
List<String> aListOfFruitsAndVeggies = new ArrayList<String>();
aListOfFruitsAndVeggies.add("Onion");
aListOfFruitsAndVeggies.addAll(aListOfFruits);

```

Now "Onion" is placed at 0 index in `aListOfFruitsAndVeggies`, "Melon" is at index 1 and "Strawberry" is at index 2.



## Iterating over Collections


### Iterating over List

```java
List<String> names  = new ArrayList<>(Arrays.asList("Clementine", "Duran", "Mike"));

```

```java
names.forEach(System.out::println);

```

If we need parallelism use

```java
names.parallelStream().forEach(System.out::println);

```

```java
for (String name : names) {
    System.out.println(name);
}

```

```java
for (int i = 0; i < names.size(); i++) {
    System.out.println(names.get(i));
}

```

```java
//Creates ListIterator which supports both forward as well as backward traversel
ListIterator<String> listIterator = names.listIterator();

//Iterates list in forward direction
while(listIterator.hasNext()){
    System.out.println(listIterator.next());
}

//Iterates list in backward direction once reaches the last element from above iterator in forward direction
while(listIterator.hasPrevious()){
    System.out.println(listIterator.previous());
}

```

### Iterating over Set

```java
Set<String> names = new HashSet<>(Arrays.asList("Clementine", "Duran", "Mike"));

```

```java
names.forEach(System.out::println);

```

```java
for (Iterator<String> iterator = names.iterator(); iterator.hasNext(); ) {
    System.out.println(iterator.next());
}

for (String name : names) {
    System.out.println(name);
}

```

```java
Iterator iterator = names.iterator();
while (iterator.hasNext()) {
    System.out.println(iterator.next());
}

```

### Iterating over Map

```java
Map<Integer, String> names = new HashMap<>();
names.put(1, "Clementine");
names.put(2, "Duran");
names.put(3, "Mike");

```

```java
names.forEach((key, value) -> System.out.println("Key: " + key + " Value: " + value));

```

```java
for (Map.Entry<Integer, String> entry : names.entrySet()) {
    System.out.println(entry.getKey());
    System.out.println(entry.getValue());
}
    
// Iterating over only keys
for (Integer key : names.keySet()) {
    System.out.println(key);
}
// Iterating over only values
for (String value : names.values()) {
    System.out.println(value);
}

```

```java
Iterator entries = names.entrySet().iterator();
while (entries.hasNext()) {
    Map.Entry entry = (Map.Entry) entries.next();
    System.out.println(entry.getKey());
    System.out.println(entry.getValue());
}

```



## Immutable Empty Collections


Sometimes it is appropriate to use an immutable empty collection. The [`Collections`](https://docs.oracle.com/javase/7/docs/api/java/util/Collections.html) class provides methods to get such collections in an efficient way:

```java
List<String> anEmptyList = Collections.emptyList();
Map<Integer, Date> anEmptyMap = Collections.emptyMap();
Set<Number> anEmptySet = Collections.emptySet();

```

These methods are generic and will automatically convert the returned collection to the type it is assigned to. That is, an invocation of e.g. `emptyList()` can be assigned to any type of `List` and likewise for `emptySet()` and `emptyMap()`.

The collections returned by these methods are immutable in that they will throw `UnsupportedOperationException` if you attempt to call methods which would change their contents (`add`, `put`, etc.). These collections are primarily useful as substitutes for empty method results or other default values, instead of using `null` or creating objects with `new`.



## Unmodifiable Collection


Sometimes it's not a good practice expose an internal collection since it can lead to a malicious code vulnerability due to it's mutable characteristic. In order to provide "read-only" collections java provides its unmodifiable versions.

An unmodifiable collection is often a copy of a modifiable collection which guarantees that the collection itself cannot be altered. Attempts to modify it will result in an UnsupportedOperationException exception.

It is important to notice that objects which are present inside the collection can still be altered.

```java
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class MyPojoClass {
    private List<Integer> intList = new ArrayList<>();

    public void addValueToIntList(Integer value){
        intList.add(value);
    }
    
    public List<Integer> getIntList() {
        return Collections.unmodifiableList(intList);
    }
}

```

The following attempt to modify an unmodifiable collection will throw an exception:

```java
import java.util.List;

public class App {

    public static void main(String[] args) {
        MyPojoClass pojo = new MyPojoClass();
        pojo.addValueToIntList(42);
        
        List<Integer> list = pojo.getIntList();
        list.add(69);
    }
}

```

output:

```java
Exception in thread "main" java.lang.UnsupportedOperationException
    at java.util.Collections$UnmodifiableCollection.add(Collections.java:1055)
    at App.main(App.java:12)

```



## Pitfall: concurrent modification exceptions


This exception occurs when a collection is modified while iterating over it using methods other than those provided by the iterator object. For example, we have a list of hats and we want to remove all those that have ear flaps:

```java
List<IHat> hats = new ArrayList<>();
hats.add(new Ushanka()); // that one has ear flaps
hats.add(new Fedora());
hats.add(new Sombrero());
for (IHat hat : hats) {
    if (hat.hasEarFlaps()) {
        hats.remove(hat);
    }
}

```

If we run this code, ****ConcurrentModificationException**** will be raised since the code modifies the collection while iterating it. The same exception may occur if one of the multiple threads working with the same list is trying to modify the collection while others iterate over it. Concurrent modification of collections in multiple threads is a natural thing, but should be treated with usual tools from the concurrent programming toolbox such as synchronization locks, special collections adopted for concurrent modification, modifying the cloned collection from initial etc.



## Sub Collections


### List subList(int fromIndex, int toIndex)

Here fromIndex is inclusive and toIndex is exclusive.

```java
List list = new ArrayList(); 
List list1 = list.subList(fromIndex,toIndex); 

```


1. If the list doesn't exist in the give range, it throws IndexOutofBoundException.
1. What ever changes made on the list1 will impact the same changes in the list.This is called backed collections.
1. If the fromnIndex is greater than the toIndex  (fromIndex > toIndex) it throws IllegalArgumentException.

Example:

```java
List<String> list = new ArrayList<String>();
List<String> list = new ArrayList<String>();
list.add("Hello1"); 
list.add("Hello2"); 
System.out.println("Before Sublist "+list); 
List<String> list2 = list.subList(0, 1);
list2.add("Hello3"); 
System.out.println("After sublist changes "+list); 

```

> 
<p>Output:<br/>
Before Sublist [Hello1, Hello2]<br/>
After sublist changes [Hello1, Hello3, Hello2]</p>


### Set subSet(fromIndex,toIndex)

Here fromIndex is inclusive and toIndex is exclusive.

```java
Set set = new TreeSet(); 
Set set1 = set.subSet(fromIndex,toIndex);

```

The returned set will throw an IllegalArgumentException on an attempt to insert an element outside its range.

### Map subMap(fromKey,toKey)

fromKey is inclusive and toKey is exclusive

```java
Map map = new TreeMap();
Map map1 = map.get(fromKey,toKey);

```

If fromKey is greater than toKey or if this map itself has a restricted range, and fromKey or toKey lies outside the bounds of the range then it throws IllegalArgumentException.

All the collections support backed collections means changes made on the sub collection will have same change on the main collection.



## Join lists


Following ways can be used for joining lists without modifying source list(s).

First approach. Has more lines but easy to understand

```java
List<String> newList = new ArrayList<String>();
newList.addAll(listOne);
newList.addAll(listTwo);

```

Second approach. Has one less line but less readable.

```java
List<String> newList = new ArrayList<String>(listOne);
newList.addAll(listTwo);

```

Third approach. Requires third party [Apache commons-collections](https://commons.apache.org/proper/commons-collections/apidocs/org/apache/commons/collections4/ListUtils.html) library.

```java
ListUtils.union(listOne,listTwo);

```

Using Streams the same can be achieved by

```java
List<String> newList = Stream.concat(listOne.stream(), listTwo.stream()).collect(Collectors.toList());

```

References. [Interface List](https://docs.oracle.com/javase/8/docs/api/java/util/List.html#addAll-java.util.Collection-)



## Removing matching items from Lists using Iterator.


Above I noticed an example to remove items from a List within a Loop and I thought of another example that may come in handy this time using the `Iterator` interface.<br>
This is a demonstration of a trick that might come in handy when dealing with duplicate items in lists that you want to get rid of.

Note: This is only adding on to the **Removing items from a List within a loop** example:

So let's define our lists as usual

```

   String[] names = {"James","Smith","Sonny","Huckle","Berry","Finn","Allan"};
    List<String> nameList = new ArrayList<>();

    //Create a List from an Array
    nameList.addAll(Arrays.asList(names));
    
    String[] removeNames = {"Sonny","Huckle","Berry"};
    List<String> removeNameList = new ArrayList<>();

    //Create a List from an Array
    removeNameList.addAll(Arrays.asList(removeNames));

```

The following method takes in two Collection objects and performs the magic of removing the elements in our `removeNameList` that match with elements in `nameList`.

```java
private static void removeNames(Collection<String> collection1, Collection<String> collection2) {
      //get Iterator.
    Iterator<String> iterator = collection1.iterator();
    
    //Loop while collection has items
    while(iterator.hasNext()){
        if (collection2.contains(iterator.next()))
            iterator.remove(); //remove the current Name or Item
    }
}

```

Calling the method and passing in the `nameList` and the `removeNameList`as follows `removeNames(nameList,removeNameList);`<br>
Will produce the following output:

> 
<p>Array List before removing names:
**James Smith Sonny Huckle Berry Finn Allan**<br>
Array List after removing names:
**James Smith Finn Allan**</p>


A simple neat use for Collections that may come in handy to remove repeating elements within lists.



## Collections and Primitive Values


Collections in Java only work for objects. I.e. there is no `Map<int, int>` in Java. Instead, primitive values need to be **boxed** into objects, as in `Map<Integer, Integer>`. Java auto-boxing will enable transparent use of these collections:

```java
Map<Integer, Integer> map = new HashMap<>();
map.put(1, 17); // Automatic boxing of int to Integer objects
int a = map.get(1); // Automatic unboxing.

```

Unfortunately, the overhead of this is **substantial**. A `HashMap<Integer, Integer>` will require about 72 bytes per entry (e.g. on 64-bit JVM with compressed pointers, and assuming integers larger than 256, and assuming 50% load of the map). Because the actual data is only 8 bytes, this yields a massive overhead. Furthermore, it requires two level of indirection (Map -> Entry -> Value) it is unnecessarily slow.

There exist several libraries with optimized collections for primitive data types (that require only ~16 bytes per entry at 50% load, i.e. 4x less memory, and one level of indirection less), that can yield substantial performance benefits when using large collections of primitive values in Java.



## Creating your own Iterable structure for use with Iterator or for-each loop.


To ensure that our collection can be iterated using iterator or for-each loop, we have to take care of following steps:

<li>The stuff we want to iterate upon has to be `Iterable` and expose
`iterator()`.</li>
1. Design a `java.util.Iterator` by overriding `hasNext()`, `next()` and `remove()`.

I have added a simple generic linked list implementation below that uses above entities to make the linked list iterable.

```java
package org.algorithms.linkedlist;
 
import java.util.Iterator;
import java.util.NoSuchElementException;
 
 
public class LinkedList<T> implements Iterable<T> {
 
    Node<T> head, current;
 
    private static class Node<T> {
        T data;
        Node<T> next;
 
        Node(T data) {
            this.data = data;
        }
    }
 
    public LinkedList(T data) {
        head = new Node<>(data);
    }
 
    public Iterator<T> iterator() {
        return new LinkedListIterator();
    }
 
    private class LinkedListIterator implements Iterator<T> {
 
        Node<T> node = head;
 
        @Override
        public boolean hasNext() {
            return node != null;
        }
 
        @Override
        public T next() {
            if (!hasNext())
                throw new NoSuchElementException();
            Node<T> prevNode = node;
            node = node.next;
            return prevNode.data;
        }
 
        @Override
        public void remove() {
            throw new UnsupportedOperationException("Removal logic not implemented.");
        }
    }
 
    public void add(T data) {
        Node current = head;
        while (current.next != null)
            current = current.next;
        current.next = new Node<>(data);
    }
 
}
 
class App {
    public static void main(String[] args) {
 
        LinkedList<Integer> list = new LinkedList<>(1);
        list.add(2);
        list.add(4);
        list.add(3);
 
        //Test #1
        System.out.println("using Iterator:");
        Iterator<Integer> itr = list.iterator();
        while (itr.hasNext()) {
            Integer i = itr.next();
            System.out.print(i + " ");
        }
 
        //Test #2
        System.out.println("\n\nusing for-each:");
        for (Integer data : list) {
            System.out.print(data + " ");
        }
    }
}

```

**Output**

```java
using Iterator:
1 2 4 3
using for-each:
1 2 4 3 

```

This will run in Java 7+. You can make it run on Java 5 and Java 6 also by substituting:

```java
LinkedList<Integer> list = new LinkedList<>(1);

```

with

```java
LinkedList<Integer> list = new LinkedList<Integer>(1);

```

or just any other version by incorporating the compatible changes.



#### Remarks


Collections are objects that can store collections of other objects inside of them. You can specify the type of data stored in a collection using [Generics](http://stackoverflow.com/documentation/java/92/generics).

Collections generally use the `java.util` or `java.util.concurrent` namespaces.

Java 1.4.2 and below do not support generics.  As such, you can not specify the type parameters that a collection contains.  In addition to not having type safety, you must also use casts to get the correct type back from a collection.

In addition to `Collection<E>`, there are multiple major types of collections, some of which have subtypes.

- `List<E>` is an ordered collection of objects.  It is similar to an array, but does not define a size limit. Implementations will usually grow in size internally to accomodate new elements.
<li>`Set<E>` is a collection of objects that does not allow duplicates.
<ul>
- `SortedSet<E>` is a `Set<E>` that also specifies element ordering.

- `SortedMap<K,V>` is a `Map<K,V>` that also specifies element ordering.

Java 5 adds in a new collection type:

- `Queue<E>` is a collection of elements meant to be processed in a specific order.  The implementation specifies whether this is FIFO or LIFO.  This obsoletes the `Stack` class.

Java 6 adds in some new subtypes of collections.

- `NavigableSet<E>` is a `Set<E>` with special navigation methods built in.
- `NavigableMap<K,V>` is a `Map<K,V>` with special navigation methods built in.
- `Deque<E>` is a `Queue<E>` that can be read from either end.

Note that the above items are all interfaces.  In order to use them, you must find the appropriate implementing classes, such as `ArrayList`, `HashSet`, `HashMap`, or `PriorityQueue`.

Each type of collection has multiple implementations that have different performance metrics and use cases.

Note that the [Liskov Substitution Principle](http://stackoverflow.com/documentation/java/87/inheritance/3106/the-liskov-substitution-principle#t=201607211929168634383) applies to the collection subtypes.  That is, a `SortedSet<E>` can be passed to a function expecting a `Set<E>`.  It is also useful to read about [Bounded Parameters](http://stackoverflow.com/documentation/java/92/generics/1229/bounded-parameters#t=201607211933059600587) in the Generics section for more information on how to use collections with class inheritance.

If you want to create your own collections, it may be easier to inherit one of the abstract classes (such as `AbstractList`) instead of implementing the interface.

Prior to 1.2, you had to use the following classes/interfaces instead:

- `Vector` instead of `ArrayList`
- `Dictionary` instead of `Map`.  Note that Dictionary is also an abstract class rather than an interface.
- `Hashtable` instead of `HashMap`

These classes are obsolete and should not be used in modern code.

