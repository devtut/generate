---
metaTitle: "Iterator and Iterable"
description: "Creating your own Iterable., Removing elements using an iterator, Using Iterable in for loop, Using the raw iterator"
---

# Iterator and Iterable




## Creating your own Iterable.


To create your own Iterable as with any interface you just implement the abstract methods in the interface. For `Iterable` there is only one which is called `iterator()`. But its return type `Iterator` is itself an interface with three abstract methods. You can return an iterator associated with some collection or create your own custom implementation:

```java
public static class Alphabet implements Iterable<Character> {

    @Override
    public Iterator<Character> iterator() {
        return new Iterator<Character>() {
            char letter = 'a';

            @Override
            public boolean hasNext() {
                return letter <= 'z';
            }

            @Override
            public Character next() {
                return letter++;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException("Doesn't make sense to remove a letter");
            }
        };
    }
}

```

To use:

```java
public static void main(String[] args) {
    for(char c : new Alphabet()) {
        System.out.println("c = " + c);
    }
}

```

The new `Iterator` should come with a state pointing to the first item, each call to next updates its state to point to the next one. The `hasNext()` checks to see if the iterator is at the end. If the iterator were connected to a modifiable collection then the iterator's optional `remove()` method might be implemented to remove the item currently pointed to from the underlying collection.



## Removing elements using an iterator


The `Iterator.remove()` method is an optional method that removes the element returned by the previous call to `Iterator.next()`.  For example, the following code populates a list of strings and then removes all of the empty strings.

```java
List<String> names = new ArrayList<>();
names.add("name 1");
names.add("name 2");
names.add("");
names.add("name 3");
names.add("");
System.out.println("Old Size : " + names.size());
Iterator<String> it = names.iterator();
while (it.hasNext()) {
  String el = it.next();
  if (el.equals("")) {
    it.remove();
  }
}
System.out.println("New Size : " + names.size());

```

Output :

```java
Old Size : 5
New Size : 3

```

Note that is the code above is the safe way to remove elements while iterating a typical collection.  If instead, you attempt to do remove elements from a collection like this:

```java
for (String el: names) {
    if (el.equals("")) {
        names.remove(el); // WRONG!
    }
}

```

a typical collection (such as `ArrayList`) which provides iterators with **fail fast** iterator semantics will throw a `ConcurrentModificationException`.

The `remove()` method can only called (once) following a `next()` call.  If it is called before calling `next()` or if it is called twice following a `next()` call, then the `remove()` call will throw an `IllegalStateException`.

The `remove` operation is described as an **optional** operation; i.e. not all iterators will allow it.  Examples where it is not supported include iterators for immutable collections, read-only views of collections, or fixed sized collections.  If `remove()` is called when the iterator does not support removal, it will throw an `UnsupportedOperationException`.



## Using Iterable in for loop


Classes implementing `Iterable<>` interface can be used in `for` loops. This is actually only [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for getting an iterator from the object and using it to get all elements sequentially; it makes code clearer, faster to write end less error-prone.

```java
public class UsingIterable {

    public static void main(String[] args) {
        List<Integer> intList = Arrays.asList(1,2,3,4,5,6,7);
        
        // List extends Collection, Collection extends Iterable
        Iterable<Integer> iterable = intList;
        
        // foreach-like loop
        for (Integer i: iterable) {
            System.out.println(i);
        }
        
        // pre java 5 way of iterating loops
        for(Iterator<Integer> i = iterable.iterator(); i.hasNext(); ) {
            Integer item = i.next();
            System.out.println(item);
        }
    }
}

```



## Using the raw iterator


While using the foreach loop (or "extended for loop") is simple, it's sometimes beneficial to use the iterator directly. For example, if you want to output a bunch of comma-separated values, but don't want the last item to have a comma:

```java
List<String> yourData = //...
Iterator<String> iterator = yourData.iterator();
while (iterator.hasNext()){
    // next() "moves" the iterator to the next entry and returns it's value.
    String entry = iterator.next();
    System.out.print(entry);
    if (iterator.hasNext()){
        // If the iterator has another element after the current one:
        System.out.print(",");
    }
}

```

This is much easier and clearer than having a `isLastEntry` variable or doing calculations with the loop index.



#### Remarks


It is possible to iterate over an array using the `for`-each loop, though java arrays do not implement Iterable; iterating is done by JVM using a non-accessible index in the background.

