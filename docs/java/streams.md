---
metaTitle: "Streams"
description: "Using Streams, Consuming Streams, Creating a Frequency Map, Infinite Streams, Collect Elements of a Stream into a Collection, Using Streams to Implement Mathematical Functions, Parallel Stream, Flatten Streams with flatMap(), Creating a Stream, Finding Statistics about Numerical Streams, Using IntStream to iterate over indexes, Converting an iterator to a stream, Concatenate Streams, IntStream to String, Finding the First Element that Matches a Predicate, Using Streams and Method References to Write Self-Documenting Processes, Using Streams of Map.Entry to Preserve Initial Values after Mapping, Reduction with Streams, Converting a Stream of Optional to a Stream of Values, Get a Slice of a Stream, Create a Map based on a Stream, Sort Using Stream, Streams of Primitives, Stream operations categories, Joining a stream to a single String, Collect Results of a Stream into an Array, Generating random Strings using Streams"
---

# Streams


A `Stream` represents a sequence of elements and supports different kind of operations to perform computations upon those elements. With Java 8, `Collection` interface has two methods to generate a `Stream`: `stream()` and `parallelStream()`. `Stream` operations are either intermediate or terminal. Intermediate operations return a `Stream` so multiple intermediate operations can be chained before the `Stream` is closed. Terminal operations are either void or return a non-stream result.



## Using Streams


A [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html) is a sequence of elements upon which sequential and parallel aggregate operations can be performed. Any given `Stream` can potentially have an unlimited amount of data flowing through it. As a result, data received from a `Stream` is processed individually as it arrives, as opposed to performing batch processing on the data altogether. When combined with [lambda expressions](https://stackoverflow.com/documentation/java/91/lambda-expressions) they provide a concise way to perform operations on sequences of data using a functional approach.

**Example:** ([see it work on Ideone](https://ideone.com/IUWRdv))

```java
Stream<String> fruitStream = Stream.of("apple", "banana", "pear", "kiwi", "orange");

fruitStream.filter(s -> s.contains("a"))
           .map(String::toUpperCase)
           .sorted()
           .forEach(System.out::println);

```

**Output:**

> 
<p>APPLE<br />
BANANA<br />
ORANGE<br />
PEAR</p>


The operations performed by the above code can be summarized as follows:

<li>
Create a `Stream<String>` containing a sequenced ordered [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html) of fruit `String` elements using the static factory method [`Stream.of(values)`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#of-T...-).
</li>
<li>
The [`filter()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#filter-java.util.function.Predicate-) operation retains only elements that match a given predicate (the elements that when tested by the predicate return true). In this case, it retains the elements containing an `"a"`. The predicate is given as a [lambda expression](https://stackoverflow.com/documentation/java/91/lambda-expressions).
</li>
<li>
The [`map()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#map-java.util.function.Function-) operation transforms each element using a given function, called a mapper. In this case, each fruit `String` is mapped to its uppercase `String` version using the [method-reference](https://docs.oracle.com/javase/tutorial/java/javaOO/methodreferences.html) [`String::toUppercase`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--).
<blockquote>
<p>**Note** that the `map()` operation will return a stream with a different generic type if the mapping function returns a type
different to its input parameter. For example on a `Stream<String>`
calling `.map(String::isEmpty)` returns a `Stream<Boolean>`</p>
</blockquote>
</li>
<li>
The [`sorted()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#sorted--) operation sorts the elements of the `Stream` according to their natural ordering (lexicographically, in the case of `String`).
</li>
<li>
Finally, the [`forEach(action)`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#forEach-java.util.function.Consumer-) operation performs an action which acts on each element of the `Stream`, passing it to a [Consumer](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#sorted-java.util.Comparator-). In the example, each element is simply being printed to the console. This operation is a terminal operation, thus making it impossible to operate on it again.
<blockquote>
**Note** that operations defined on the `Stream` are performed **because** of the terminal operation. Without a terminal operation, the stream is not processed. Streams can not be reused. Once a terminal operation is called, the `Stream` object becomes unusable.
</blockquote>
</li>

> 
**Note** that operations defined on the `Stream` are performed **because** of the terminal operation. Without a terminal operation, the stream is not processed. Streams can not be reused. Once a terminal operation is called, the `Stream` object becomes unusable.


[<img src="https://i.stack.imgur.com/6p8aM.png" alt="Chained operations" />](https://i.stack.imgur.com/6p8aM.png)

Operations (as seen above) are chained together to form what can be seen as a query on the data.

### Closing Streams

> 
**Note that a `Stream` generally does not have to be closed.** It is only required to close streams that operate on IO channels. Most `Stream` types don't operate on resources and therefore don't require closing.


The `Stream` interface extends [`AutoCloseable`](https://docs.oracle.com/javase/8/docs/api/java/lang/AutoCloseable.html). Streams can be closed by calling the [`close`](https://docs.oracle.com/javase/8/docs/api/java/lang/AutoCloseable.html#close--) method or by using  try-with-resource statements.

An example use case where a `Stream` should be closed is when you create a `Stream` of lines from a file:

```java
try (Stream<String> lines = Files.lines(Paths.get("somePath"))) {
    lines.forEach(System.out::println);
}

```

The `Stream` interface also declares the [`Stream.onClose()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/BaseStream.html#onClose-java.lang.Runnable-) method which allows you to register [`Runnable`](https://docs.oracle.com/javase/8/docs/api/java/lang/Runnable.html) handlers which will be called when the stream is closed. An example use case is where code which produces a stream needs to know when it is consumed to perform some cleanup.

```java
public Stream<String>streamAndDelete(Path path) throws IOException {
    return Files.lines(path).onClose(() -> someClass.deletePath(path));
}

```

The run handler will only execute if the `close()` method gets called, either explicitly or implicitly by a try-with-resources statement.

### Processing Order

A `Stream` object's processing can be sequential or [parallel](http://stackoverflow.com/documentation/java/88/streams/2785/parallel-stream#t=201607281158351104612).

In a **sequential** mode, the elements are processed in the order of the source of the `Stream`. If the `Stream` is ordered (such as a [`SortedMap`](https://docs.oracle.com/javase/8/docs/api/java/util/SortedMap.html) implementation or a [`List`](https://docs.oracle.com/javase/8/docs/api/java/util/List.html)) the processing is guaranteed to match the ordering of the source. In other cases, however, care should be taken not to depend on the ordering (see: [is the Java `HashMap` `keySet()` iteration order consistent?](http://stackoverflow.com/q/1882762/1667004)).

**Example:**

```java
List<Integer> integerList = Arrays.asList(0, 1, 2, 3, 42); 

// sequential 
long howManyOddNumbers = integerList.stream()
                                    .filter(e -> (e % 2) == 1)
                                    .count(); 

System.out.println(howManyOddNumbers); // Output: 2

```

[Live on Ideone](https://ideone.com/FuMz1c)

**Parallel** mode allows the use of multiple threads on multiple cores but there is no guarantee of the order in which elements are processed.

If multiple methods are called on a sequential `Stream`, not every method has to be invoked. For example, if a `Stream` is filtered and the number of elements is reduced to one, a subsequent call to a method such as `sort` will not occur. This can increase the performance of a sequential `Stream` — an optimization that is not possible with a parallel `Stream`.

**Example:**

```java
// parallel
long howManyOddNumbersParallel = integerList.parallelStream()
                                            .filter(e -> (e % 2) == 1)
                                            .count();

System.out.println(howManyOddNumbersParallel); // Output: 2

```

[Live on Ideone](https://ideone.com/FuMz1c)

### Differences from Containers (or [Collections](https://stackoverflow.com/documentation/java/90/collections))

While some actions can be performed on both Containers and Streams, they ultimately serve different purposes and support different operations. Containers are more focused on how the elements are stored and how those elements can be accessed efficiently. A `Stream`, on the other hand, doesn't provide direct access and manipulation to its elements; it is more dedicated to the group of objects as a collective entity and performing operations on that entity as a whole. `Stream` and `Collection` are separate high-level abstractions for these differing purposes.



## Consuming Streams


A [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html) will only be traversed when there is a **terminal operation**, like [`count()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#count--), [`collect()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#collect-java.util.stream.Collector-) or [`forEach()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#forEach-java.util.function.Consumer-). Otherwise, no operation on the [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html) will be performed.

In the following example, no terminal operation is added to the [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html), so the [`filter()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#filter-java.util.function.Predicate-) operation will not be invoked and no output will be produced because [`peek()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#peek-java.util.function.Consumer-) is NOT a **terminal operation**.

```java
IntStream.range(1, 10).filter(a -> a % 2 == 0).peek(System.out::println);

```

[Live on Ideone](https://ideone.com/YDijRj)

This is a `Stream` sequence with a valid **terminal operation**, thus an output is produced.

You could also use `forEach` instead of `peek`:

```java
IntStream.range(1, 10).filter(a -> a % 2 == 0).forEach(System.out::println); 

```

[Live on Ideone](https://ideone.com/LzUoM6)

Output:

> 
<p>2<br>
4<br>
6<br>
8</p>


After the terminal operation is performed, the `Stream` is consumed and cannot be reused.

Although a given stream object cannot be reused, it's easy to create a reusable `Iterable` that delegates to a stream pipeline. This can be useful for returning a modified view of a live data set without having to collect results into a temporary structure.

```java
List<String> list = Arrays.asList("FOO", "BAR");
Iterable<String> iterable = () -> list.stream().map(String::toLowerCase).iterator();

for (String str : iterable) {
    System.out.println(str);
}
for (String str : iterable) {
    System.out.println(str);
}

```

Output:

> 
<p>foo<br>
bar<br>
foo<br>
bar<br></p>


This works because `Iterable` declares a single abstract method `Iterator<T> iterator()`. That makes it effectively a functional interface, implemented by a lambda that creates a new stream on each call.

In general, a `Stream` operates as shown in the following image:

### [<img src="https://i.stack.imgur.com/lrwjM.jpg" alt="Stream Operation" />](https://i.stack.imgur.com/lrwjM.jpg)

**NOTE**: Argument checks are always performed, even without a **terminal operation**:

```java
try {
    IntStream.range(1, 10).filter(null);
} catch (NullPointerException e) {
    System.out.println("We got a NullPointerException as null was passed as an argument to filter()");
}

```

[Live on Ideone](https://ideone.com/zrkoRz)

Output:

> 
We got a NullPointerException as null was passed as an argument to filter()




## Creating a Frequency Map


The [`groupingBy(classifier, downstream)`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#groupingBy-java.util.function.Function-) collector allows the collection of [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html) elements into a [`Map`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.html) by classifying each element in a group and performing a downstream operation on the elements classified in the same group.

A classic example of this principle is to use a [`Map`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.html) to count the occurrences of elements in a [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html). In this example, the classifier is simply the identity function, which returns the element as-is. The downstream operation counts the number of equal elements, using [`counting()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#counting--).

```java
Stream.of("apple", "orange", "banana", "apple")
      .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()))
      .entrySet()
      .forEach(System.out::println);

```

The downstream operation is itself a collector ([`Collectors.counting()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#counting--)) that operates on elements of type String and produces a result of type `Long`. The result of the `collect` method call is a `Map<String, Long>`.

This would produce the following output:

> 
<p>banana=1<br>
orange=1<br>
apple=2</p>




## Infinite Streams


It is possible to generate a `Stream` that does not end. Calling a terminal method on an infinite `Stream` causes the `Stream` to enter an infinite loop. The [`limit`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#limit-long-) method of a `Stream` can be used to limit the number of terms of the `Stream` that Java processes.

This example generates a `Stream` of all natural numbers, starting with the number 1. Each successive term of the `Stream` is one higher than the previous. By calling the limit method of this `Stream`, only the first five terms of the `Stream` are considered and printed.

```java
// Generate infinite stream - 1, 2, 3, 4, 5, 6, 7, ...
IntStream naturalNumbers = IntStream.iterate(1, x -> x + 1);

// Print out only the first 5 terms
naturalNumbers.limit(5).forEach(System.out::println);

```

Output:

> 
<p>1<br>
2<br>
3<br>
4<br>
5</p>


Another way of generating an infinite stream is using the [Stream.generate](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#generate-java.util.function.Supplier-) method. This method takes a [lambda](https://stackoverflow.com/documentation/java/91/lambda-expressions) of type [Supplier](https://docs.oracle.com/javase/8/docs/api/java/util/function/Supplier.html).

```java
// Generate an infinite stream of random numbers
Stream<Double> infiniteRandomNumbers = Stream.generate(Math::random);

// Print out only the first 10 random numbers
infiniteRandomNumbers.limit(10).forEach(System.out::println);

```



## Collect Elements of a Stream into a Collection


### Collect with [`toList()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toList--) and [`toSet()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toSet--)

Elements from a [`Stream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html) can be easily collected into a container by using the [`Stream.collect`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#collect-java.util.stream.Collector-) operation:

```java
System.out.println(Arrays
    .asList("apple", "banana", "pear", "kiwi", "orange")
    .stream()
    .filter(s -> s.contains("a"))
    .collect(Collectors.toList())
);
// prints: [apple, banana, pear, orange]

```

Other collection instances, such as a [`Set`](https://docs.oracle.com/javase/8/docs/api/java/util/Set.html), can be made by using other [`Collectors`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html) built-in methods. For example, [`Collectors.toSet()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toSet--) collects the elements of a `Stream` into a [`Set`](https://docs.oracle.com/javase/8/docs/api/java/util/Set.html).

### Explicit control over the implementation of [`List`](https://docs.oracle.com/javase/8/docs/api/java/util/List.html) or [`Set`](https://docs.oracle.com/javase/8/docs/api/java/util/Set.html)

According to documentation of [`Collectors#toList()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toList--) and [`Collectors#toSet()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toSet--), there are no guarantees on the type, mutability, serializability, or thread-safety of the [`List`](https://docs.oracle.com/javase/8/docs/api/java/util/List.html) or [`Set`](https://docs.oracle.com/javase/8/docs/api/java/util/Set.html) returned.

For explicit control over the implementation to be returned, [`Collectors#toCollection(Supplier)`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toCollection-java.util.function.Supplier-) can be used instead, where the given supplier returns a new and empty collection.

```java
// syntax with method reference
System.out.println(strings
        .stream()
        .filter(s -> s != null && s.length() <= 3)
        .collect(Collectors.toCollection(ArrayList::new))
);

// syntax with lambda
System.out.println(strings
        .stream()
        .filter(s -> s != null && s.length() <= 3)
        .collect(Collectors.toCollection(() -> new LinkedHashSet<>()))
);

```

**Collecting Elements using [toMap](http://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toMap-java.util.function.Function-java.util.function.Function-)**

Collector accumulates elements into a Map, Where key is the Student Id and Value is Student Value.

```

 List<Student> students = new ArrayList<Student>(); 
    students.add(new Student(1,"test1"));
    students.add(new Student(2,"test2"));
    students.add(new Student(3,"test3"));
    
    Map<Integer, String> IdToName = students.stream()
        .collect(Collectors.toMap(Student::getId, Student::getName));
    System.out.println(IdToName);

```

Output :

```java
{1=test1, 2=test2, 3=test3}

```

The Collectors.toMap has another implementation `Collector<T, ?, Map<K,U>> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction)`.The mergeFunction is mostly used to select either new value or retain old value if the key is repeated when adding a new member in the Map from a list.

The mergeFunction often looks like: `(s1, s2) -> s1` to retain value corresponding to the repeated key, or `(s1, s2) -> s2` to put new value for the repeated key.

**Collecting Elements to Map of Collections**

Example: from ArrayList to Map<String, List<>>

Often it requires to make a map of list out of a primary list.
Example: From a student of list, we need to make a map of list of subjects for each student.

```

   List<Student> list = new ArrayList<>();
    list.add(new Student("Davis", SUBJECT.MATH, 35.0));
    list.add(new Student("Davis", SUBJECT.SCIENCE, 12.9));
    list.add(new Student("Davis", SUBJECT.GEOGRAPHY, 37.0));

    list.add(new Student("Sascha", SUBJECT.ENGLISH, 85.0));
    list.add(new Student("Sascha", SUBJECT.MATH, 80.0));
    list.add(new Student("Sascha", SUBJECT.SCIENCE, 12.0));
    list.add(new Student("Sascha", SUBJECT.LITERATURE, 50.0));

    list.add(new Student("Robert", SUBJECT.LITERATURE, 12.0));

    Map<String, List<SUBJECT>> map = new HashMap<>();
    list.stream().forEach(s -> {
                map.computeIfAbsent(s.getName(), x -> new ArrayList<>()).add(s.getSubject());
            });
    System.out.println(map);

```

Output:

```java
{ Robert=[LITERATURE], 
Sascha=[ENGLISH, MATH, SCIENCE, LITERATURE], 
Davis=[MATH, SCIENCE, GEOGRAPHY] }

```

Example: from ArrayList to Map<String, Map<>>

```

   List<Student> list = new ArrayList<>();
    list.add(new Student("Davis", SUBJECT.MATH, 1, 35.0));
    list.add(new Student("Davis", SUBJECT.SCIENCE, 2, 12.9));
    list.add(new Student("Davis", SUBJECT.MATH, 3, 37.0));
    list.add(new Student("Davis", SUBJECT.SCIENCE, 4, 37.0));

    list.add(new Student("Sascha", SUBJECT.ENGLISH, 5, 85.0));
    list.add(new Student("Sascha", SUBJECT.MATH, 1, 80.0));
    list.add(new Student("Sascha", SUBJECT.ENGLISH, 6, 12.0));
    list.add(new Student("Sascha", SUBJECT.MATH, 3, 50.0));

    list.add(new Student("Robert", SUBJECT.ENGLISH, 5, 12.0));

    Map<String, Map<SUBJECT, List<Double>>> map = new HashMap<>();

    list.stream().forEach(student -> {
        map.computeIfAbsent(student.getName(), s -> new HashMap<>())
                .computeIfAbsent(student.getSubject(), s -> new ArrayList<>())
                .add(student.getMarks());
    });

    System.out.println(map);

```

Output:

```java
{ Robert={ENGLISH=[12.0]}, 
Sascha={MATH=[80.0, 50.0], ENGLISH=[85.0, 12.0]}, 
Davis={MATH=[35.0, 37.0], SCIENCE=[12.9, 37.0]} }

```

### Cheat-Sheet

|Goal|Code
|---|---|---|---|---|---|---|---|---|---
|Collect to a `List`|`Collectors.toList()`
|Collect to an `ArrayList` with pre-allocated size|`Collectors.toCollection(() -> new ArrayList<>(size))`
|Collect to a `Set`|`Collectors.toSet()`
|Collect to a `Set` with better iteration performance|`Collectors.toCollection(() -> new LinkedHashSet<>())`
|Collect to a case-insensitive `Set<String>`|`Collectors.toCollection(() -> new TreeSet<>(String.CASE_INSENSITIVE_ORDER))`
|Collect to an `EnumSet<AnEnum>` (best performance for enums)|`Collectors.toCollection(() -> EnumSet.noneOf(AnEnum.class))`
|Collect to a `Map<K,V>` with unique keys|`Collectors.toMap(keyFunc,valFunc)`
|Map MyObject.getter() to unique MyObject|`Collectors.toMap(MyObject::getter, Function.identity())`
|Map MyObject.getter() to multiple MyObjects|`Collectors.groupingBy(MyObject::getter)`



## Using Streams to Implement Mathematical Functions


`Stream`s, and especially `IntStream`s, are an elegant way of implementing summation terms (∑). The ranges of the `Stream` can be used as the bounds of the summation.

E.g., Madhava's approximation of Pi is given by the formula (Source: [wikipedia](https://en.wikipedia.org/wiki/Approximations_of_%CF%80)):
[<img src="http://i.stack.imgur.com/XY9Kr.png" alt="Madhava's approximation" />](http://i.stack.imgur.com/XY9Kr.png)

This can be calculated with an arbitrary precision. E.g., for 101 terms:

```java
double pi = Math.sqrt(12) * 
            IntStream.rangeClosed(0, 100)
                     .mapToDouble(k -> Math.pow(-3, -1 * k) / (2 * k + 1))
                     .sum();

```

**Note:** With `double`'s precision, selecting an upper bound of 29 is sufficient to get a result that's indistinguishable from `Math.Pi`.



## Parallel Stream


**Note:** Before deciding which `Stream` to use please have a look at [ParallelStream vs Sequential Stream behavior](http://stackoverflow.com/a/20375622/585398).

When you want to perform `Stream` operations concurrently, you could use either of these ways.

```java
List<String> data = Arrays.asList("One", "Two", "Three", "Four", "Five");
Stream<String> aParallelStream = data.stream().parallel();

```

Or:

```java
Stream<String> aParallelStream = data.parallelStream();

```

To execute the operations defined for the parallel stream, call a terminal operator:

```java
aParallelStream.forEach(System.out::println);

```

(A possible) output from the parallel `Stream`:

> 
<p>Three<br>
Four<br>
One<br>
Two<br>
Five</p>


The order might change as all the elements are processed in parallel (Which **may** make it faster). Use [`parallelStream`](https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html#parallelStream--) when ordering does not matter.

### Performance impact

In case networking is involved, parallel `Stream`s may degrade the overall performance of an application because all parallel `Stream`s use a common fork-join thread pool for the network.

On the other hand, parallel `Stream`s may significantly improve performance in many other cases, depending of the number of available cores in the running CPU at the moment.



## Flatten Streams with flatMap()


A `Stream` of items that are in turn streamable can be flattened into a single continuous `Stream`:

Array of List of Items can be converted into a single List.

```java
List<String> list1 =  Arrays.asList("one", "two");
      List<String> list2 =  Arrays.asList("three","four","five");
      List<String> list3 =  Arrays.asList("six");
          List<String> finalList = Stream.of(list1, list2, list3).flatMap(Collection::stream).collect(Collectors.toList());
System.out.println(finalList);

// [one, two, three, four, five, six]

```

Map containing List of Items as values can be Flattened to a Combined List

```java
Map<String, List<Integer>> map = new LinkedHashMap<>();
map.put("a", Arrays.asList(1, 2, 3));
map.put("b", Arrays.asList(4, 5, 6));

List<Integer> allValues = map.values() // Collection<List<Integer>>
        .stream()                      // Stream<List<Integer>>
        .flatMap(List::stream)         // Stream<Integer>
        .collect(Collectors.toList());

System.out.println(allValues);
// [1, 2, 3, 4, 5, 6]

```

`List` of `Map` can be flattened into a single continuous `Stream`

```java
List<Map<String, String>> list = new ArrayList<>();
Map<String,String> map1 = new HashMap();
map1.put("1", "one");
map1.put("2", "two");

Map<String,String> map2 = new HashMap();
map2.put("3", "three");
map2.put("4", "four");
list.add(map1);
list.add(map2);


Set<String> output= list.stream()  //  Stream<Map<String, String>>
    .map(Map::values)              // Stream<List<String>>
    .flatMap(Collection::stream)   // Stream<String>
    .collect(Collectors.toSet());  //Set<String>
// [one, two, three,four]

```



## Creating a Stream


All java `Collection<E>`s have [`stream()`](https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html#stream--) and [`parallelStream()`](https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html#parallelStream--) methods from which a `Stream<E>` can be constructed:

```java
Collection<String> stringList = new ArrayList<>();
Stream<String> stringStream = stringList.parallelStream();

```

A `Stream<E>` can be created from an array using one of two methods:

```java
String[] values = { "aaa", "bbbb", "ddd", "cccc" };
Stream<String> stringStream = Arrays.stream(values);
Stream<String> stringStreamAlternative = Stream.of(values);

```

The difference between [`Arrays.stream()`](https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#stream-T:A-) and [`Stream.of()`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#of-T...-) is that `Stream.of()` has a varargs parameter, so it can be used like:

```java
Stream<Integer> integerStream = Stream.of(1, 2, 3);

```

There are also primitive `Stream`s that you can use. For example:

```java
IntStream intStream = IntStream.of(1, 2, 3);
DoubleStream doubleStream = DoubleStream.of(1.0, 2.0, 3.0);

```

These primitive streams can also be constructed using the `Arrays.stream()` method:

```java
IntStream intStream = Arrays.stream(new int[]{ 1, 2, 3 });

```

It is possible to create a `Stream` from an array with a specified range.

```java
int[] values= new int[]{1, 2, 3, 4, 5};
IntStream intStram = Arrays.stream(values, 1, 3);

```

Note that any primitive stream can be converted to boxed type stream using the `boxed` method :

```java
Stream<Integer> integerStream = intStream.boxed();

```

This can be useful in some case if you want to collect the data since primitive stream does not have any `collect` method that takes a `Collector` as argument.

**Reusing intermediate operations of a stream chain**

Stream is closed when ever terminal operation is called. Reusing the stream of intermediate operations, when only terminal operation is only varying.
we could create a stream supplier to construct a new stream with all intermediate operations already set up.

```java
Supplier<Stream<String>> streamSupplier = () -> Stream.of("apple", "banana","orange", "grapes", "melon","blueberry","blackberry")
.map(String::toUpperCase).sorted();

  streamSupplier.get().filter(s ->   s.startsWith("A")).forEach(System.out::println);

// APPLE

  streamSupplier.get().filter(s -> s.startsWith("B")).forEach(System.out::println);

  // BANANA
  // BLACKBERRY
  // BLUEBERRY

```

`int[]` arrays can be converted to `List<Integer>` using streams

```java
int[] ints = {1,2,3};
List<Integer> list = IntStream.of(ints).boxed().collect(Collectors.toList());

```



## Finding Statistics about Numerical Streams


Java 8 provides classes called [`IntSummaryStatistics`](http://docs.oracle.com/javase/8/docs/api/java/util/IntSummaryStatistics.html), [`DoubleSummaryStatistics`](https://docs.oracle.com/javase/8/docs/api/java/util/DoubleSummaryStatistics.html)  and [`LongSummaryStatistics`](https://docs.oracle.com/javase/8/docs/api/java/util/LongSummaryStatistics.html) which give a state object for collecting statistics such as `count`, `min`, `max`, `sum`, and `average`.

```java
List<Integer> naturalNumbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
IntSummaryStatistics stats = naturalNumbers.stream()
                                           .mapToInt((x) -> x)
                                           .summaryStatistics();
System.out.println(stats);

```

Which will result in:

```java
IntSummaryStatistics{count=10, sum=55, min=1, max=10, average=5.500000}

```



## Using IntStream to iterate over indexes


`Stream`s of elements usually do not allow access to the index value of the current item. To iterate over an array or `ArrayList` while having access to indexes, use `IntStream.range(start, endExclusive)`.

```java
String[] names = { "Jon", "Darin", "Bauke", "Hans", "Marc" };

IntStream.range(0, names.length)
    .mapToObj(i -> String.format("#%d %s", i + 1, names[i]))
    .forEach(System.out::println);

```

The [`range(start, endExclusive)`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/IntStream.html#range-int-int-) method returns another [`ÌntStream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/IntStream.html) and the [`mapToObj(mapper)`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/IntStream.html#mapToObj-java.util.function.IntFunction-) returns a stream of `String`.

Output:

> 
<p>#1 Jon<br>
#2 Darin<br>
#3 Bauke<br>
#4 Hans<br>
#5 Marc</p>


This is very similar to using a normal `for` loop with a counter, but with the benefit of pipelining and parallelization:

```java
for (int i = 0; i < names.length; i++) {
    String newName = String.format("#%d %s", i + 1, names[i]);
    System.out.println(newName);
}

```



## Converting an iterator to a stream


Use `Spliterators.spliterator()` or `Spliterators.spliteratorUnknownSize()` to convert an iterator to a stream:

```java
Iterator<String> iterator = Arrays.asList("A", "B", "C").iterator();    
Spliterator<String> spliterator = Spliterators.spliteratorUnknownSize(iterator, 0);
Stream<String> stream = StreamSupport.stream(spliterator, false);

```



## Concatenate Streams


Variable declaration for examples:

```java
Collection<String> abc = Arrays.asList("a", "b", "c");
Collection<String> digits = Arrays.asList("1", "2", "3");
Collection<String> greekAbc = Arrays.asList("alpha", "beta", "gamma");

```

**Example 1 - Concatenate two `Stream`s**

```java
final Stream<String> concat1 = Stream.concat(abc.stream(), digits.stream());

concat1.forEach(System.out::print);
// prints: abc123

```

**Example 2 - Concatenate more than two `Stream`s**

```java
final Stream<String> concat2 = Stream.concat(
    Stream.concat(abc.stream(), digits.stream()),
    greekAbc.stream());

System.out.println(concat2.collect(Collectors.joining(", ")));
// prints: a, b, c, 1, 2, 3, alpha, beta, gamma

```

Alternatively to simplify the nested `concat()` syntax the `Stream`s can also be concatenated with `flatMap()`:

```java
final Stream<String> concat3 = Stream.of(
    abc.stream(), digits.stream(), greekAbc.stream())
    .flatMap(s -> s);
    // or `.flatMap(Function.identity());` (java.util.function.Function)

System.out.println(concat3.collect(Collectors.joining(", ")));
// prints: a, b, c, 1, 2, 3, alpha, beta, gamma

```

Be careful when constructing `Stream`s from repeated concatenation, because accessing an element of a deeply concatenated `Stream` can result in deep call chains or even a `StackOverflowException`.



## IntStream to String


Java does not have a **Char Stream**, so when working with `String`s and constructing a `Stream` of `Character`s, an option is to get a [`IntStream`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/IntStream.html) of code points using `String.codePoints()` method. So `IntStream` can be obtained as below:

```java
public IntStream stringToIntStream(String in) {
  return in.codePoints();
}

```

It is a bit more involved to do the conversion other way around i.e. IntStreamToString. That can be done as follows:

```java
public String intStreamToString(IntStream intStream) {
  return intStream.collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append).toString();
}

```



## Finding the First Element that Matches a Predicate


It is possible to find the first element of a `Stream` that matches a condition.

For this example, we will find the first `Integer` whose square is over `50000`.

```java
IntStream.iterate(1, i -> i + 1) // Generate an infinite stream 1,2,3,4...
    .filter(i -> (i*i) > 50000) // Filter to find elements where the square is >50000
    .findFirst(); // Find the first filtered element

```

This expression will return an `OptionalInt` with the result.

Note that with an infinite `Stream`, Java will keep checking each element until it finds a result. With a finite `Stream`, if Java runs out of elements but still can't find a result, it returns an empty `OptionalInt`.



## Using Streams and Method References to Write Self-Documenting Processes


Method references make excellent self-documenting code, and using method references with `Stream`s makes complicated processes simple to read and understand. Consider the following code:

```java
public interface Ordered {
    default int getOrder(){
        return 0;
    }
}

public interface Valued<V extends Ordered> {
    boolean hasPropertyTwo();
    V getValue();
}

public interface Thing<V extends Ordered> {
    boolean hasPropertyOne();
    Valued<V> getValuedProperty();
}

public <V extends Ordered> List<V> myMethod(List<Thing<V>> things) {
    List<V> results = new ArrayList<V>();
    for (Thing<V> thing : things) {
        if (thing.hasPropertyOne()) {
            Valued<V> valued = thing.getValuedProperty();
            if (valued != null && valued.hasPropertyTwo()){
                V value = valued.getValue();
                if (value != null){
                    results.add(value);
                }
            }
        }
    }
    results.sort((a, b)->{
        return Integer.compare(a.getOrder(), b.getOrder());
    });
    return results;
}

```

This last method rewritten using `Stream`s and method references is much more legible and each step of the process is quickly and easily understood - it's not just shorter, it also shows at a glance which interfaces and classes are responsible for the code in each step:

```java
public <V extends Ordered> List<V> myMethod(List<Thing<V>> things) {
    return things.stream()
        .filter(Thing::hasPropertyOne)
        .map(Thing::getValuedProperty)
        .filter(Objects::nonNull)
        .filter(Valued::hasPropertyTwo)
        .map(Valued::getValue)
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(Ordered::getOrder))
        .collect(Collectors.toList());
}    

```



## Using Streams of Map.Entry to Preserve Initial Values after Mapping


When you have a `Stream` you need to map but want to preserve the initial values as well, you can map the `Stream` to a `Map.Entry<K,V>` using a utility method like the following:

```java
public static <K, V> Function<K, Map.Entry<K, V>> entryMapper(Function<K, V> mapper){
    return (k)->new AbstractMap.SimpleEntry<>(k, mapper.apply(k));
}

```

Then you can use your converter to process `Stream`s having access to both the original and mapped values:

```java
Set<K> mySet;
Function<K, V> transformer = SomeClass::transformerMethod;
Stream<Map.Entry<K, V>> entryStream = mySet.stream()
    .map(entryMapper(transformer));

```

You can then continue to process that `Stream` as normal. This avoids the overhead of creating an intermediate collection.



## Reduction with Streams


Reduction is the process of applying a binary operator to every element of a stream to result in one value.

The `sum()` method of an `IntStream` is an example of a reduction; it applies addition to every term of the Stream, resulting in one final value:
[<img src="http://i.stack.imgur.com/2Krwx.png" alt="Sum Reduction" />](http://i.stack.imgur.com/2Krwx.png)

This is equivalent to `(((1+2)+3)+4)`

The `reduce` method of a Stream allows one to create a custom reduction. It is possible to use the `reduce` method to implement the `sum()` method:

```java
IntStream istr;
    
//Initialize istr
    
OptionalInt istr.reduce((a,b)->a+b);

```

The `Optional` version is returned so that empty Streams can be handled appropriately.

Another example of reduction is combining a `Stream<LinkedList<T>>` into a single `LinkedList<T>`:

```java
Stream<LinkedList<T>> listStream;
    
//Create a Stream<LinkedList<T>>
    
Optional<LinkedList<T>> bigList = listStream.reduce((LinkedList<T> list1, LinkedList<T> list2)->{
    LinkedList<T> retList = new LinkedList<T>();
    retList.addAll(list1);
    retList.addAll(list2);
    return retList;
});

```

You can also provide an **identity element**. For example, the identity element for addition is 0, as `x+0==x`. For multiplication, the identity element is 1, as `x*1==x`. In the case above, the identity element is an empty `LinkedList<T>`, because if you add an empty list to another list, the list that you are "adding" to doesn't change:

```java
Stream<LinkedList<T>> listStream;

//Create a Stream<LinkedList<T>>

LinkedList<T> bigList = listStream.reduce(new LinkedList<T>(), (LinkedList<T> list1, LinkedList<T> list2)->{
    LinkedList<T> retList = new LinkedList<T>();
    retList.addAll(list1);
    retList.addAll(list2);
    return retList;
});

```

Note that when an identity element is provided, the return value is not wrapped in an `Optional`—if called on an empty stream, `reduce()` will return the identity element.

The binary operator must also be **associative**, meaning that `(a+b)+c==a+(b+c)`. This is because the elements may be reduced in any order. For example, the above addition reduction could be performed like this:

[<img src="http://i.stack.imgur.com/sCqVF.png" alt="Other sum reduction" />](http://i.stack.imgur.com/sCqVF.png)

This reduction is equivalent to writing `((1+2)+(3+4))`. The property of associativity also allows Java to reduce the `Stream` in parallel—a portion of the Stream can be reduced by each processor, with a reduction combining the result of each processor at the end.



## Converting a Stream of Optional to a Stream of Values


You may need to convert a `Stream` emitting `Optional` to a `Stream` of values, emitting only values from existing `Optional`. (ie: without `null` value and not dealing with [`Optional.empty()`](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#empty--)).

```

Optional<String> op1 = Optional.empty();
 Optional<String> op2 = Optional.of("Hello World");

 List<String> result = Stream.of(op1, op2)
                             .filter(Optional::isPresent)
                             .map(Optional::get)
                             .collect(Collectors.toList());

 System.out.println(result); //[Hello World]

```



## Get a Slice of a Stream


**Example:** Get a `Stream` of 30 elements, containing 21st to 50th (inclusive) element of a collection.

```java
final long n = 20L; // the number of elements to skip
final long maxSize = 30L; // the number of elements the stream should be limited to
final Stream<T> slice = collection.stream().skip(n).limit(maxSize);

```

**Notes:**

<li>`IllegalArgumentException` is thrown if `n` is negative or `maxSize`
is negative</li>
- both `skip(long)` and `limit(long)` are intermediate operations
- if a stream contains fewer than `n` elements then `skip(n)` returns an empty stream
- both `skip(long)` and `limit(long)` are cheap operations on sequential stream pipelines, but can be quite expensive on ordered parallel pipelines



## Create a Map based on a Stream


**Simple case without duplicate keys**

```java
Stream<String> characters = Stream.of("A", "B", "C");

Map<Integer, String> map = characters
            .collect(Collectors.toMap(element -> element.hashCode(), element -> element));
// map = {65=A, 66=B, 67=C}

```

To make things more declarative, we can use static method in `Function` interface - [`Function.identity()`](https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html#identity--). We can replace this lambda `element -> element` with `Function.identity()`.

**Case where there might be duplicate keys**

The [javadoc](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#toMap-java.util.function.Function-java.util.function.Function-java.util.function.BinaryOperator-) for `Collectors.toMap` states:

> 
If the mapped keys contains duplicates (according to `Object.equals(Object)`), an `IllegalStateException` is thrown when the collection operation is performed. If the mapped keys may have duplicates, use `toMap(Function, Function, BinaryOperator)` instead.


```java
Stream<String> characters = Stream.of("A", "B", "B", "C");

Map<Integer, String> map = characters
            .collect(Collectors.toMap(
                element -> element.hashCode(),
                element -> element,
                (existingVal, newVal) -> (existingVal + newVal)));

// map = {65=A, 66=BB, 67=C}

```

The `BinaryOperator` passed to `Collectors.toMap(...)` generates the value to be stored in the case of a collision.  It can:

- return the old value, so that the first value in the stream takes precedence,
- return the new value, so that the last value in the stream takes precedence, or
- combine the old and new values

**Grouping by value**

You can use [`Collectors.groupingBy`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html#groupingBy-java.util.function.Function-java.util.stream.Collector-) when you need to perform the equivalent of a database cascaded "group by" operation.  To illustrate, the following creates a map in which people's names are mapped to surnames:

```java
List<Person> people = Arrays.asList(
    new Person("Sam", "Rossi"),
    new Person("Sam", "Verdi"),
    new Person("John", "Bianchi"),
    new Person("John", "Rossi"),
    new Person("John", "Verdi")
);

Map<String, List<String>> map = people.stream()
        .collect(
                // function mapping input elements to keys
                Collectors.groupingBy(Person::getName, 
                // function mapping input elements to values,
                // how to store values
                Collectors.mapping(Person::getSurname, Collectors.toList()))
        );

// map = {John=[Bianchi, Rossi, Verdi], Sam=[Rossi, Verdi]}

```

[Live on Ideone](https://ideone.com/2Ar9IA)



## Sort Using Stream


```java
List<String> data = new ArrayList<>();
data.add("Sydney");
data.add("London");
data.add("New York");
data.add("Amsterdam");
data.add("Mumbai");
data.add("California");

System.out.println(data);

List<String> sortedData = data.stream().sorted().collect(Collectors.toList());

System.out.println(sortedData);

```

Output:

```java
[Sydney, London, New York, Amsterdam, Mumbai, California]
[Amsterdam, California, London, Mumbai, New York, Sydney]

```

It's also possible to use different comparison mechanism as there is a overloaded [`sorted`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#sorted-java.util.Comparator-) version which takes a comparator as its argument.

Also, you can use a lambda expression for sorting:

```java
List<String> sortedData2 = data.stream().sorted((s1,s2) -> s2.compareTo(s1)).collect(Collectors.toList());

```

This would output `[Sydney, New York, Mumbai, London, California, Amsterdam]`

You can use `Comparator.reverseOrder()` to have a comparator that imposes the `reverse` of the natural ordering.

```java
List<String> reverseSortedData = data.stream().sorted(Comparator.reverseOrder()).collect(Collectors.toList());

```



## Streams of Primitives


Java provides specialized `Stream`s for three types of primitives `IntStream` (for `int`s), `LongStream` (for `long`s) and `DoubleStream` (for `double`s). Besides being optimized implementations for their respective primitives, they also provide several specific terminal methods, typically for mathematical operations. E.g.:

```java
IntStream is = IntStream.of(10, 20, 30);
double average = is.average().getAsDouble(); // average is 20.0

```



## Stream operations categories


Stream operations fall into two main categories, intermediate and terminal operations, and two sub-categories, stateless and stateful.

### Intermediate Operations:

An intermediate operation is always **lazy**, such as a simple `Stream.map`. It is not invoked until the stream is actually consumed. This can be verified easily:

```java
Arrays.asList(1, 2 ,3).stream().map(i -> {
    throw new RuntimeException("not gonna happen");
    return i;
});

```

Intermediate operations are the common building blocks of a stream, chained after the source and are usually followed by a terminal operation triggering the stream chain.

### Terminal Operations

Terminal operations are what triggers the consumption of a stream. Some of the more common are `Stream.forEach` or `Stream.collect`. They are usually placed after a chain of intermediate operations and are almost always **eager**.

### Stateless Operations

Statelessness means that each item is processed without the context of other items. Stateless operations allow for memory-efficient processing of streams.  Operations like `Stream.map` and `Stream.filter` that do not require information on other items of the stream are considered to be stateless.

### Stateful operations

Statefulness means the operation on each item depends on (some) other items of the stream. This requires a state to be preserved. Statefulness operations may break with long, or infinite, streams.  Operations like `Stream.sorted` require the entirety of the stream to be processed before any item is emitted which will break in a long enough stream of items. This can be demonstrated by a long stream (**run at your own risk**):

```java
// works - stateless stream
long BIG_ENOUGH_NUMBER = 999999999;
IntStream.iterate(0, i -> i + 1).limit(BIG_ENOUGH_NUMBER).forEach(System.out::println);

```

This will cause an out-of-memory due to statefulness of `Stream.sorted`:

```java
// Out of memory - stateful stream
IntStream.iterate(0, i -> i + 1).limit(BIG_ENOUGH_NUMBER).sorted().forEach(System.out::println);

```



## Joining a stream to a single String


A use case that comes across frequently, is creating a `String` from a stream, where the stream-items are separated by a certain character. The `Collectors.joining()` method can be used for this, like in the following example:

```java
Stream<String> fruitStream = Stream.of("apple", "banana", "pear", "kiwi", "orange");

String result = fruitStream.filter(s -> s.contains("a"))
           .map(String::toUpperCase)
           .sorted()
           .collect(Collectors.joining(", "));
           
System.out.println(result);

```

Output:

> 
APPLE, BANANA, ORANGE, PEAR


The `Collectors.joining()` method can also cater for pre- and postfixes:

```java
String result = fruitStream.filter(s -> s.contains("e"))
           .map(String::toUpperCase)
           .sorted()
           .collect(Collectors.joining(", ", "Fruits: ", "."));
           
System.out.println(result);

```

Output:

> 
Fruits: APPLE, ORANGE, PEAR.


[Live on Ideone](http://ideone.com/e.js/MH4kmG)



## Collect Results of a Stream into an Array


Analog to get a collection for a `Stream` by `collect()` an array can be obtained by the `Stream.toArray()` method:

```java
List<String> fruits = Arrays.asList("apple", "banana", "pear", "kiwi", "orange");

String[] filteredFruits = fruits.stream()
    .filter(s -> s.contains("a"))
    .toArray(String[]::new);     

// prints: [apple, banana, pear, orange]
System.out.println(Arrays.toString(filteredFruits));

```

`String[]::new` is a special kind of method reference: a constructor reference.



## Generating random Strings using Streams


It is sometimes useful to create random `Strings`, maybe as Session-ID for a web-service or an initial password after registration for an application.
This can be easily achieved using `Stream`s.

First we need to initialize a random number generator. To enhance security for the generated `String`s, it is a good idea to use `SecureRandom`.

**Note**: Creating a `SecureRandom` is quite expensive, so it is best practice to only do this once and call one of its `setSeed()` methods from time to time to reseed it.

```java
private static final SecureRandom rng = new SecureRandom(SecureRandom.generateSeed(20)); 
//20 Bytes as a seed is rather arbitrary, it is the number used in the JavaDoc example

```

When creating random `String`s, we usually want them to use only certain characters (e.g. only letters and digits). Therefore we can create a method returning a `boolean` which can later be used to filter the `Stream`.

```java
//returns true for all chars in 0-9, a-z and A-Z
boolean useThisCharacter(char c){
    //check for range to avoid using all unicode Letter (e.g. some chinese symbols)
    return c >= '0' && c <= 'z' && Character.isLetterOrDigit(c);
}

```

Next we can utilize the RNG to generate a random String of specific length containing the charset which pass our `useThisCharacter` check.

```java
public String generateRandomString(long length){
    //Since there is no native CharStream, we use an IntStream instead
    //and convert it to a Stream<Character> using mapToObj.
    //We need to specify the boundaries for the int values to ensure they can safely be cast to char
    Stream<Character> randomCharStream = rng.ints(Character.MIN_CODE_POINT, Character.MAX_CODE_POINT).mapToObj(i -> (char)i).filter(c -> this::useThisCharacter).limit(length);

    //now we can use this Stream to build a String utilizing the collect method.
    String randomString = randomCharStream.collect(StringBuilder::new, StringBuilder::append, StringBuilder::append).toString();
    return randomString;
}

```



#### Syntax


- collection.stream()
- Arrays.stream(array)
- Stream.iterate(firstValue, currentValue -> nextValue)
- Stream.generate(() -> value)
- Stream.of(elementOfT[, elementOfT, ...])
- Stream.empty()
- StreamSupport.stream( iterable.spliterator(), false )

