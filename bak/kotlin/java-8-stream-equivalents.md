---
metaTitle: "Kotlin - Java 8 Stream Equivalents"
description: "Accumulate names in a List, Collect example #5 - find people of legal age, output formatted string, Collect example #6 - group people by age, print age and names together, Different Kinds of Streams #7 - lazily iterate Doubles, map to Int, map to String, print each, Counting items in a list after filter is applied, Convert elements to strings and concatenate them, separated by commas, Compute sum of salaries of employee, Group employees by department, Compute sum of salaries by department, Partition students into passing and failing, Names of male members, Group names of members in roster by gender, Filter a list to another list, Finding shortest string a list, Different Kinds of Streams #2 - lazily using first item if exists, Different Kinds of Streams #3 - iterate a range of Integers, Different Kinds of Streams #4 - iterate an array, map the values, calculate the average, Different Kinds of Streams #5 - lazily iterate a list of strings, map the values, convert to Int, find max, Different Kinds of Streams #6 - lazily iterate a stream of Ints, map the values, print results, How streams work - filter, upper case, then sort a list, Different Kinds of Streams #1 - eager using first item if it exists, Collect example #7a - Map names, join together with delimiter, Collect example #7b - Collect with SummarizingInt"
---

# Java 8 Stream Equivalents


Kotlin provides many extension methods on collections and iterables for applying functional-style operations. A dedicated `Sequence` type allows for lazy composition of several such operations.



## Accumulate names in a List


```kotlin
// Java:  
List<String> list = people.stream().map(Person::getName).collect(Collectors.toList());

```

```kotlin
// Kotlin:
val list = people.map { it.name }  // toList() not needed

```



## Collect example #5 - find people of legal age, output formatted string


```kotlin
// Java:
String phrase = persons
        .stream()
        .filter(p -> p.age >= 18)
        .map(p -> p.name)
        .collect(Collectors.joining(" and ", "In Germany ", " are of legal age."));

System.out.println(phrase);
// In Germany Max and Peter and Pamela are of legal age.    

```

```kotlin
// Kotlin:
val phrase = persons
        .filter { it.age >= 18 }
        .map { it.name }
        .joinToString(" and ", "In Germany ", " are of legal age.")

println(phrase)
// In Germany Max and Peter and Pamela are of legal age.

```

And as a side note, in Kotlin we can create simple [data classes](https://kotlinlang.org/docs/reference/data-classes.html) and instantiate the test data as follows:

```kotlin
// Kotlin:
// data class has equals, hashcode, toString, and copy methods automagically
data class Person(val name: String, val age: Int) 

val persons = listOf(Person("Tod", 5), Person("Max", 33), 
                     Person("Frank", 13), Person("Peter", 80),
                     Person("Pamela", 18))

```



## Collect example #6 - group people by age, print age and names together


```kotlin
// Java:
Map<Integer, String> map = persons
        .stream()
        .collect(Collectors.toMap(
                p -> p.age,
                p -> p.name,
                (name1, name2) -> name1 + ";" + name2));

System.out.println(map);
// {18=Max, 23=Peter;Pamela, 12=David}    

```

Ok, a more interest case here for Kotlin.  First the wrong answers to explore variations of creating a `Map` from a collection/sequence:

```kotlin
// Kotlin:
val map1 = persons.map { it.age to it.name }.toMap()
println(map1)
// output: {18=Max, 23=Pamela, 12=David} 
// Result: duplicates overridden, no exception similar to Java 8

val map2 = persons.toMap({ it.age }, { it.name })
println(map2)
// output: {18=Max, 23=Pamela, 12=David} 
// Result: same as above, more verbose, duplicates overridden

val map3 = persons.toMapBy { it.age }
println(map3)
// output: {18=Person(name=Max, age=18), 23=Person(name=Pamela, age=23), 12=Person(name=David, age=12)}
// Result: duplicates overridden again

val map4 = persons.groupBy { it.age }
println(map4)
// output: {18=[Person(name=Max, age=18)], 23=[Person(name=Peter, age=23), Person(name=Pamela, age=23)], 12=[Person(name=David, age=12)]}
// Result: closer, but now have a Map<Int, List<Person>> instead of Map<Int, String>

val map5 = persons.groupBy { it.age }.mapValues { it.value.map { it.name } }
println(map5)
// output: {18=[Max], 23=[Peter, Pamela], 12=[David]}
// Result: closer, but now have a Map<Int, List<String>> instead of Map<Int, String>

```

And now for the correct answer:

```kotlin
// Kotlin:
val map6 = persons.groupBy { it.age }.mapValues { it.value.joinToString(";") { it.name } }

println(map6)
// output: {18=Max, 23=Peter;Pamela, 12=David}
// Result: YAY!!

```

We just needed to join the matching values to collapse the lists and provide a transformer to `joinToString` to move from `Person` instance to the `Person.name`.



## Different Kinds of Streams #7 - lazily iterate Doubles, map to Int, map to String, print each


```kotlin
// Java:
Stream.of(1.0, 2.0, 3.0)
    .mapToInt(Double::intValue)
    .mapToObj(i -> "a" + i)
    .forEach(System.out::println);

// a1
// a2
// a3

```

```kotlin
// Kotlin:
sequenceOf(1.0, 2.0, 3.0).map(Double::toInt).map { "a$it" }.forEach(::println)

```



## Counting items in a list after filter is applied


```kotlin
// Java:
long count = items.stream().filter( item -> item.startsWith("t")).count();

```

```kotlin
// Kotlin:
val count = items.filter { it.startsWith('t') }.size
// but better to not filter, but count with a predicate
val count = items.count { it.startsWith('t') }

```



## Convert elements to strings and concatenate them, separated by commas


```kotlin
// Java:
String joined = things.stream()
                       .map(Object::toString)
                       .collect(Collectors.joining(", "));

```

```kotlin
// Kotlin:
val joined = things.joinToString() // ", " is used as separator, by default

```



## Compute sum of salaries of employee


```kotlin
// Java:
int total = employees.stream()
                      .collect(Collectors.summingInt(Employee::getSalary)));

```

```kotlin
// Kotlin:
val total = employees.sumBy { it.salary }

```



## Group employees by department


```kotlin
// Java:
Map<Department, List<Employee>> byDept
     = employees.stream()
                .collect(Collectors.groupingBy(Employee::getDepartment));

```

```kotlin
// Kotlin:
val byDept = employees.groupBy { it.department }

```



## Compute sum of salaries by department


```kotlin
// Java:
Map<Department, Integer> totalByDept
     = employees.stream()
                .collect(Collectors.groupingBy(Employee::getDepartment,
                     Collectors.summingInt(Employee::getSalary)));

```

```kotlin
// Kotlin:
val totalByDept = employees.groupBy { it.dept }.mapValues { it.value.sumBy { it.salary }}

```



## Partition students into passing and failing


```kotlin
// Java:
Map<Boolean, List<Student>> passingFailing =
     students.stream()
             .collect(Collectors.partitioningBy(s -> s.getGrade() >= PASS_THRESHOLD));

```

```kotlin
// Kotlin:
val passingFailing = students.partition { it.grade >= PASS_THRESHOLD }

```



## Names of male members


```kotlin
// Java:
List<String> namesOfMaleMembersCollect = roster
    .stream()
    .filter(p -> p.getGender() == Person.Sex.MALE)
    .map(p -> p.getName())
    .collect(Collectors.toList());

```

```kotlin
// Kotlin:
val namesOfMaleMembers = roster.filter { it.gender == Person.Sex.MALE }.map { it.name }

```



## Group names of members in roster by gender


```kotlin
// Java:
Map<Person.Sex, List<String>> namesByGender =
      roster.stream().collect(
        Collectors.groupingBy(
            Person::getGender,                      
            Collectors.mapping(
                Person::getName,
                Collectors.toList())));

```

```kotlin
// Kotlin:
val namesByGender = roster.groupBy { it.gender }.mapValues { it.value.map { it.name } }   

```



## Filter a list to another list


```kotlin
// Java:
List<String> filtered = items.stream()
    .filter( item -> item.startsWith("o") )
    .collect(Collectors.toList());

```

```kotlin
// Kotlin:
val filtered = items.filter { item.startsWith('o') } 

```



## Finding shortest string a list


```kotlin
// Java:
String shortest = items.stream()
    .min(Comparator.comparing(item -> item.length()))
    .get();

```

```kotlin
// Kotlin:
val shortest = items.minBy { it.length }

```



## Different Kinds of Streams #2 - lazily using first item if exists


```kotlin
// Java:
Stream.of("a1", "a2", "a3")
    .findFirst()
    .ifPresent(System.out::println);    

```

```kotlin
// Kotlin:
sequenceOf("a1", "a2", "a3").firstOrNull()?.apply(::println)

```



## Different Kinds of Streams #3 - iterate a range of Integers


```kotlin
// Java:
IntStream.range(1, 4).forEach(System.out::println);

```

```kotlin
// Kotlin:  (inclusive range)
(1..3).forEach(::println)

```



## Different Kinds of Streams #4 - iterate an array, map the values, calculate the average


```kotlin
// Java:
Arrays.stream(new int[] {1, 2, 3})
    .map(n -> 2 * n + 1)
    .average()
    .ifPresent(System.out::println); // 5.0    

```

```kotlin
// Kotlin:
arrayOf(1,2,3).map { 2 * it + 1}.average().apply(::println)

```



## Different Kinds of Streams #5 - lazily iterate a list of strings, map the values, convert to Int, find max


```kotlin
// Java:
Stream.of("a1", "a2", "a3")
    .map(s -> s.substring(1))
    .mapToInt(Integer::parseInt)
    .max()
    .ifPresent(System.out::println);  // 3

```

```kotlin
// Kotlin:
sequenceOf("a1", "a2", "a3")
    .map { it.substring(1) }
    .map(String::toInt)
    .max().apply(::println)

```



## Different Kinds of Streams #6 - lazily iterate a stream of Ints, map the values, print results


```kotlin
// Java:
IntStream.range(1, 4)
    .mapToObj(i -> "a" + i)
    .forEach(System.out::println);

// a1
// a2
// a3    

```

```kotlin
// Kotlin:  (inclusive range)
(1..3).map { "a$it" }.forEach(::println)

```



## How streams work - filter, upper case, then sort a list


```kotlin
// Java:
List<String> myList = Arrays.asList("a1", "a2", "b1", "c2", "c1");

myList.stream()
      .filter(s -> s.startsWith("c"))
      .map(String::toUpperCase)
     .sorted()
     .forEach(System.out::println);

// C1
// C2

```

```kotlin
// Kotlin:
val list = listOf("a1", "a2", "b1", "c2", "c1")
list.filter { it.startsWith('c') }.map (String::toUpperCase).sorted()
        .forEach (::println)

```



## Different Kinds of Streams #1 - eager using first item if it exists


```kotlin
// Java:
Arrays.asList("a1", "a2", "a3")
    .stream()
    .findFirst()
    .ifPresent(System.out::println);    

```

```kotlin
// Kotlin:
listOf("a1", "a2", "a3").firstOrNull()?.apply(::println)

```

or, create an extension function on String called ifPresent:

```kotlin
// Kotlin:
inline fun String?.ifPresent(thenDo: (String)->Unit) = this?.apply { thenDo(this) }

// now use the new extension function:
listOf("a1", "a2", "a3").firstOrNull().ifPresent(::println)

```

See also: [`apply()` function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/apply.html)

See also: [Extension Functions](https://kotlinlang.org/docs/reference/extensions.html)

See also:  [`?.` Safe Call operator](https://kotlinlang.org/docs/reference/null-safety.html#safe-calls), and in general nullability: [http://stackoverflow.com/questions/34498562/in-kotlin-what-is-the-idiomatic-way-to-deal-with-nullable-values-referencing-o/34498563#34498563](http://stackoverflow.com/questions/34498562/in-kotlin-what-is-the-idiomatic-way-to-deal-with-nullable-values-referencing-o/34498563#34498563)



## Collect example #7a - Map names, join together with delimiter


```kotlin
// Java (verbose):
Collector<Person, StringJoiner, String> personNameCollector =
Collector.of(
        () -> new StringJoiner(" | "),          // supplier
        (j, p) -> j.add(p.name.toUpperCase()),  // accumulator
        (j1, j2) -> j1.merge(j2),               // combiner
        StringJoiner::toString);                // finisher

String names = persons
        .stream()
        .collect(personNameCollector);

System.out.println(names);  // MAX | PETER | PAMELA | DAVID    

// Java (concise)
String names = persons.stream().map(p -> p.name.toUpperCase()).collect(Collectors.joining(" | "));

```

```kotlin
// Kotlin:
val names = persons.map { it.name.toUpperCase() }.joinToString(" | ")

```



## Collect example #7b - Collect with SummarizingInt


```kotlin
// Java:
IntSummaryStatistics ageSummary =
    persons.stream()
           .collect(Collectors.summarizingInt(p -> p.age));

System.out.println(ageSummary);
// IntSummaryStatistics{count=4, sum=76, min=12, average=19.000000, max=23}    

```

```kotlin
// Kotlin:

// something to hold the stats...
data class SummaryStatisticsInt(var count: Int = 0,  
                                var sum: Int = 0, 
                                var min: Int = Int.MAX_VALUE, 
                                var max: Int = Int.MIN_VALUE, 
                                var avg: Double = 0.0) {
    fun accumulate(newInt: Int): SummaryStatisticsInt {
        count++
        sum += newInt
        min = min.coerceAtMost(newInt)
        max = max.coerceAtLeast(newInt)
        avg = sum.toDouble() / count
        return this
    }
}

// Now manually doing a fold, since Stream.collect is really just a fold
val stats = persons.fold(SummaryStatisticsInt()) { stats, person -> stats.accumulate(person.age) }

println(stats)
// output: SummaryStatisticsInt(count=4, sum=76, min=12, max=23, avg=19.0)

```

But it is better to create an extension function, 2 actually to match styles in Kotlin stdlib:

```kotlin
// Kotlin:
inline fun Collection<Int>.summarizingInt(): SummaryStatisticsInt
        = this.fold(SummaryStatisticsInt()) { stats, num -> stats.accumulate(num) }

inline fun <T: Any> Collection<T>.summarizingInt(transform: (T)->Int): SummaryStatisticsInt =
        this.fold(SummaryStatisticsInt()) { stats, item -> stats.accumulate(transform(item)) }

```

Now you have two ways to use the new `summarizingInt` functions:

```kotlin
val stats2 = persons.map { it.age }.summarizingInt()

// or

val stats3 = persons.summarizingInt { it.age }

```

And all of these produce the same results.  We can also create this extension to work on `Sequence` and for appropriate primitive types.



#### Remarks


### About laziness

If you want to lazy process a chain, you can convert to a `Sequence` using `asSequence()` before the chain.  At the end of the chain of functions, you usually end up with a `Sequence` as well.  Then you can use `toList()`, `toSet()`, `toMap()` or some other function to materialize the `Sequence` at the end.

```kotlin
// switch to and from lazy
val someList = items.asSequence().filter { ... }.take(10).map { ... }.toList()

// switch to lazy, but sorted() brings us out again at the end
val someList = items.asSequence().filter { ... }.take(10).map { ... }.sorted()

```

### Why are there no Types?!?

You will notice the Kotlin examples do not specify the types.  This is because Kotlin has full type inference and is completely type safe at compile time.  More so than Java because it also has nullable types and can help prevent the dreaded NPE.  So this in Kotlin:

```kotlin
val someList = people.filter { it.age <= 30 }.map { it.name }

```

is the same as:

```kotlin
val someList: List<String> = people.filter { it.age <= 30 }.map { it.name }

```

Because Kotlin knows what `people` is, and that `people.age` is `Int` therefore the filter expression only allows comparison to an `Int`, and that `people.name` is a `String` therefore the `map` step produces a `List<String>` (readonly `List` of `String`).

Now, if `people` were possibly `null`, as-in a `List<People>?` then:

```kotlin
val someList = people?.filter { it.age <= 30 }?.map { it.name }

```

Returns a `List<String>?` that would need to be null checked (**or use one of the other Kotlin operators for nullable values, see this [Kotlin idiomatic way to deal with nullable values](http://stackoverflow.com/questions/34498562/in-kotlin-what-is-the-idiomatic-way-to-deal-with-nullable-values-referencing-o) and also [Idiomatic way of handling nullable or empty list in Kotlin](http://stackoverflow.com/questions/26341225/idiomatic-way-of-handling-nullable-or-empty-list-in-kotlin)**)

### Reusing Streams

In Kotlin, it depends on the type of collection whether it can be consumed more than once.  A `Sequence` generates a new iterator every time, and unless it asserts "use only once" it can reset to the start each time it is acted upon.  Therefore while the following fails in Java 8 stream, but works in Kotlin:

```kotlin
// Java:
Stream<String> stream =
Stream.of("d2", "a2", "b1", "b3", "c").filter(s -> s.startsWith("b"));

stream.anyMatch(s -> true);    // ok
stream.noneMatch(s -> true);   // exception

```

```kotlin
// Kotlin:  
val stream = listOf("d2", "a2", "b1", "b3", "c").asSequence().filter { it.startsWith('b' ) }

stream.forEach(::println) // b1, b2

println("Any B ${stream.any { it.startsWith('b') }}") // Any B true
println("Any C ${stream.any { it.startsWith('c') }}") // Any C false

stream.forEach(::println) // b1, b2

```

And in Java to get the same behavior:

```kotlin
// Java:
Supplier<Stream<String>> streamSupplier =
    () -> Stream.of("d2", "a2", "b1", "b3", "c")
          .filter(s -> s.startsWith("a"));

streamSupplier.get().anyMatch(s -> true);   // ok
streamSupplier.get().noneMatch(s -> true);  // ok

```

Therefore in Kotlin the provider of the data decides if it can reset back and provide a new iterator or not.  But if you want to intentionally constrain a `Sequence` to one time iteration, you can use `constrainOnce()` function for `Sequence` as follows:

```kotlin
val stream = listOf("d2", "a2", "b1", "b3", "c").asSequence().filter { it.startsWith('b' ) }
        .constrainOnce()

stream.forEach(::println) // b1, b2
stream.forEach(::println) // Error:java.lang.IllegalStateException: This sequence can be consumed only once. 

```

### See also:

- API Reference for [extension functions for Iterable](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/kotlin.-iterable/index.html)
- API reference for [extension functions for Array](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/kotlin.-array/index.html)
- API reference for [extension functions for List](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/kotlin.-list/index.html)
- API reference for [extension functions to Map](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/kotlin.-map/index.html)

