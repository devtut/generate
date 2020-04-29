---
metaTitle: "Maps"
description: "Iterating Map Entries Efficiently, Using Default Methods of Map from Java 8, Usage of HashMap, Iterating through the contents of a Map, Add multiple items, Merging, combine and composing Maps, Add an element, Clear the map, Check if key exists, Use custom object as key, Creating and Initializing Maps"
---

# Maps


The [java.util.Map interface](https://docs.oracle.com/javase/7/docs/api/java/util/Map.html) represents a mapping between keys and their values. A map cannot contain duplicate keys; and each key can map to at most one value.

Since `Map` is an interface, then you need to instantiate a concrete implementation of that interface in order to use it; there are several `Map` implementations, and mostly used are the `java.util.HashMap` and `java.util.TreeMap`



## Iterating Map Entries Efficiently


This section provides code and benchmarks for ten unique example implementations which iterate over the entries of a `Map<Integer, Integer>` and generate the sum of the `Integer` values. All of the examples have an algorithmic complexity of `Θ(n)`, however, the benchmarks are still useful for providing insight on which implementations are more efficient in a "real world" environment.

1. Implementation using **[`Iterator`](https://docs.oracle.com/javase/8/docs/api/java/util/Iterator.html)** with **[`Map.Entry`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.Entry.html)**

```

   Iterator<Map.Entry<Integer, Integer>> it = map.entrySet().iterator();
    while (it.hasNext()) {
        Map.Entry<Integer, Integer> pair = it.next();
        sum += pair.getKey() + pair.getValue();
    }

```


1. Implementation using **[`for`](https://docs.oracle.com/javase/8/docs/technotes/guides/language/foreach.html)** with **[`Map.Entry`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.Entry.html)**

```

   for (Map.Entry<Integer, Integer> pair : map.entrySet()) {
        sum += pair.getKey() + pair.getValue();
    }

```


1. Implementation using **[`Map.forEach`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.html#forEach-java.util.function.BiConsumer-)** (Java 8+)

```

   map.forEach((k, v) -> sum[0] += k + v);

```


1. Implementation using **[`Map.keySet`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.html#keySet--)** with **[`for`](https://docs.oracle.com/javase/8/docs/technotes/guides/language/foreach.html)**

```

   for (Integer key : map.keySet()) {
        sum += key + map.get(key);
    }

```


1. Implementation using **[`Map.keySet`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.html#keySet--)** with **[`Iterator`](https://docs.oracle.com/javase/8/docs/api/java/util/Iterator.html)**

```

   Iterator<Integer> it = map.keySet().iterator();
    while (it.hasNext()) {
        Integer key = it.next();
        sum += key + map.get(key);
    }

```


1. Implementation using **[`for`](https://docs.oracle.com/javase/8/docs/technotes/guides/language/foreach.html)** with **[`Iterator`](https://docs.oracle.com/javase/8/docs/api/java/util/Iterator.html)** and **[`Map.Entry`](https://docs.oracle.com/javase/8/docs/api/java/util/Map.Entry.html)**

```

   for (Iterator<Map.Entry<Integer, Integer>> entries = 
             map.entrySet().iterator(); entries.hasNext(); ) {
        Map.Entry<Integer, Integer> entry = entries.next();
        sum += entry.getKey() + entry.getValue();
    }

```


1. Implementation using **[`Stream.forEach`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#forEach-java.util.function.Consumer-)** (Java 8+)

```

   map.entrySet().stream().forEach(e -> sum += e.getKey() + e.getValue());

```


1. Implementation using **[`Stream.forEach`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#forEach-java.util.function.Consumer-)** with **[`Stream.parallel`](https://docs.oracle.com/javase/8/docs/api/java/util/stream/BaseStream.html#parallel--)** (Java 8+)

```

   map.entrySet()
       .stream()
       .parallel()
       .forEach(e -> sum += e.getKey() + e.getValue());

```


1. Implementation using **[`IterableMap`](https://commons.apache.org/proper/commons-collections/javadocs/api-release/org/apache/commons/collections4/IterableMap.html)** from [Apache Collections](https://commons.apache.org/proper/commons-collections/)

```

   MapIterator<Integer, Integer> mit = iterableMap.mapIterator();
    while (mit.hasNext()) {
        sum += mit.next() + it.getValue();
    }

```


1. Implementation using **[`MutableMap`](https://www.eclipse.org/collections/javadoc/8.0.0/org/eclipse/collections/api/map/MutableMap.html)** from [Eclipse Collections](https://www.eclipse.org/collections/)

```

    mutableMap.forEachKeyValue((key, value) -> {
         sum += key + value;
     });

```

**Performance Tests** (**Code available on [Github](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/5.0-other-examples/src/main/java/other_examples/IterateThroughHashMapTest.java)**)<br />
**Test Environment: Windows 8.1 64-bit, Intel i7-4790 3.60GHz, 16 GB**

<li>
Average Performance of 10 Trials (100 elements) Best: 308±21 ns/op

```java
Benchmark                           Score     Error  Units
test3_UsingForEachAndJava8            308  ±     21  ns/op
test10_UsingEclipseMutableMap         309  ±      9  ns/op
test1_UsingWhileAndMapEntry           380  ±     14  ns/op
test6_UsingForAndIterator             387  ±     16  ns/op
test2_UsingForEachAndMapEntry         391  ±     23  ns/op
test7_UsingJava8StreamAPI             510  ±     14  ns/op
test9_UsingApacheIterableMap          524  ±      8  ns/op
test4_UsingKeySetAndForEach           816  ±     26  ns/op
test5_UsingKeySetAndIterator          863  ±     25  ns/op
test8_UsingJava8StreamAPIParallel    5552  ±    185  ns/op

```


</li>
<li>
Average Performance of 10 Trials (10000 elements) Best: 37.606±0.790 μs/op

```java
Benchmark                           Score     Error  Units
test10_UsingEclipseMutableMap       37606  ±    790  ns/op
test3_UsingForEachAndJava8          50368  ±    887  ns/op
test6_UsingForAndIterator           50332  ±    507  ns/op
test2_UsingForEachAndMapEntry       51406  ±   1032  ns/op
test1_UsingWhileAndMapEntry         52538  ±   2431  ns/op
test7_UsingJava8StreamAPI           54464  ±    712  ns/op
test4_UsingKeySetAndForEach         79016  ±  25345  ns/op
test5_UsingKeySetAndIterator        91105  ±  10220  ns/op
test8_UsingJava8StreamAPIParallel  112511  ±    365  ns/op
test9_UsingApacheIterableMap       125714  ±   1935  ns/op

```


</li>
<li>
Average Performance of 10 Trials (100000 elements) Best: 1184.767±332.968 μs/op

```java
Benchmark                            Score       Error  Units
test1_UsingWhileAndMapEntry       1184.767  ±  332.968  μs/op
test10_UsingEclipseMutableMap     1191.735  ±  304.273  μs/op
test2_UsingForEachAndMapEntry     1205.815  ±  366.043  μs/op
test6_UsingForAndIterator         1206.873  ±  367.272  μs/op
test8_UsingJava8StreamAPIParallel 1485.895  ±  233.143  μs/op
test5_UsingKeySetAndIterator      1540.281  ±  357.497  μs/op
test4_UsingKeySetAndForEach       1593.342  ±  294.417  μs/op
test3_UsingForEachAndJava8        1666.296  ±  126.443  μs/op
test7_UsingJava8StreamAPI         1706.676  ±  436.867  μs/op
test9_UsingApacheIterableMap      3289.866  ± 1445.564  μs/op

```


</li>
<li>
A Comparison of Performance Variations Respective to Map Size
</li>

[<img src="https://i.stack.imgur.com/17VGh.png" alt="Graph of Performance Tests" />](https://i.stack.imgur.com/17VGh.png)

```

      x: Size of Map
    f(x): Benchmark Score (μs/op)

                 100      600     1100     1600     2100
        ---------------------------------------------------
        10  |   0.333    1.631    2.752    5.937    8.024
         3  |   0.309    1.971    4.147    8.147   10.473
         6  |   0.372    2.190    4.470    8.322   10.531
         1  |   0.405    2.237    4.616    8.645   10.707
Tests    2  |   0.376    2.267    4.809    8.403   10.910
 f(x)    7  |   0.473    2.448    5.668    9.790   12.125
         9  |   0.565    2.830    5.952    13.22   16.965
         4  |   0.808    5.012    8.813    13.939  17.407
         5  |   0.81     5.104    8.533    14.064  17.422
         8  |   5.173   12.499   17.351    24.671  30.403

```



## Using Default Methods of Map from Java 8


Examples of using Default Methods introduced in Java 8 in Map interface

1. Using **getOrDefault**

Returns the value mapped to the key, or if the key is not present, returns the default value

```java
Map<Integer, String> map = new HashMap<>();
map.put(1, "First element");
map.get(1);                                 // => First element
map.get(2);                                 // => null
map.getOrDefault(2, "Default element");     // => Default element

```


1. Using **forEach**

Allows to perform the operation specified in the 'action' on each Map Entry

```

 Map<Integer, String> map = new HashMap<Integer, String>();
  map.put(1, "one");
  map.put(2, "two");
  map.put(3, "three");
  map.forEach((key, value) -> System.out.println("Key: "+key+ " :: Value: "+value));

   // Key: 1 :: Value: one
   // Key: 2 :: Value: two
   // Key: 3 :: Value: three

```


1. Using **replaceAll**

Will replace with new-value only if key is present

```

Map<String, Integer> map = new HashMap<String, Integer>();
 map.put("john", 20);
 map.put("paul", 30);
 map.put("peter", 40);
 map.replaceAll((key,value)->value+10);   //{john=30, paul=40, peter=50}

```


1. Using **putIfAbsent**

Key-Value pair is added to the map, if the key is not present or mapped to null

```

   Map<String, Integer> map = new HashMap<String, Integer>();
    map.put("john", 20);
    map.put("paul", 30);
    map.put("peter", 40);
    map.putIfAbsent("kelly", 50);     //{john=20, paul=30, peter=40, kelly=50}

```


<li>
Using **remove**
Removes the key only if its associated with the given value

```java
 Map<String, Integer> map = new HashMap<String, Integer>();
 map.put("john", 20);
 map.put("paul", 30);
 map.put("peter", 40);
 map.remove("peter",40); //{john=30, paul=40}

```


</li>
<li>
Using **replace**
<p>If the key is present then the value is replaced by new-value.
If the key is not present, does nothing.</p>
</li>

```java
Map<String, Integer> map = new HashMap<String, Integer>();
map.put("john", 20);
map.put("paul", 30);
map.put("peter", 40);
map.replace("peter",50); //{john=20, paul=30, peter=50}
map.replace("jack",60); //{john=20, paul=30, peter=50}

```


1. Using **computeIfAbsent**

This method adds an entry in the Map. the key is specified in the function and the value is the result of the application of the mapping function

```

   Map<String, Integer> map = new HashMap<String, Integer>();
    map.put("john", 20);
    map.put("paul", 30);
    map.put("peter", 40);
    map.computeIfAbsent("kelly", k->map.get("john")+10); //{john=20, paul=30, peter=40, kelly=30}
    map.computeIfAbsent("peter", k->map.get("john")+10); //{john=20, paul=30, peter=40, kelly=30} //peter already present

```


1. Using **computeIfPresent**

This method adds an entry or modifies an existing entry in the Map.
Does nothing if an entry with that key is not present

```

   Map<String, Integer> map = new HashMap<String, Integer>();
    map.put("john", 20);
    map.put("paul", 30);
    map.put("peter", 40);
    map.computeIfPresent("kelly", (k,v)->v+10); //{john=20, paul=30, peter=40} //kelly not present
    map.computeIfPresent("peter", (k,v)->v+10); //{john=20, paul=30, peter=50} // peter present, so increase the value 

```


1. Using **compute**

This method replaces the value of a key by the newly computed value

```

   Map<String, Integer> map = new HashMap<String, Integer>();
    map.put("john", 20);
    map.put("paul", 30);
    map.put("peter", 40);
    map.compute("peter", (k,v)->v+50); //{john=20, paul=30, peter=90} //Increase the value 

```


1. Using **merge**

Adds the key-value pair to the map, if key is not present or value for the key is null
Replaces the value with the newly computed value, if the key is present
Key is removed from the map , if new value computed is null

```

   Map<String, Integer> map = new HashMap<String, Integer>();
    map.put("john", 20);
    map.put("paul", 30);
    map.put("peter", 40);
    
    //Adds the key-value pair to the map, if key is not present or value for the key is null
    map.merge("kelly", 50 , (k,v)->map.get("john")+10); // {john=20, paul=30, peter=40, kelly=50}
    
    //Replaces the value with the newly computed value, if the key is present
    map.merge("peter", 50 , (k,v)->map.get("john")+10); //{john=20, paul=30, peter=30, kelly=50}
   
    //Key is removed from the map , if new value computed is null
    map.merge("peter", 30 , (k,v)->map.get("nancy")); //{john=20, paul=30, kelly=50}

```



## Usage of HashMap


HashMap is an implementation of the Map interface that provides a Data Structure to store data in Key-Value pairs.

**1. Declaring HashMap**

```java
Map<KeyType, ValueType> myMap = new HashMap<KeyType, ValueType>();

```

KeyType and ValueType must be valid types in Java, such as - String, Integer, Float or any custom class like Employee, Student etc..

For Example : `Map<String,Integer> myMap = new HashMap<String,Integer>();`

**2. Putting values in HashMap.**

To put a value in the HashMap, we have to call `put` method on the HashMap object by passing the Key and the Value as parameters.

```java
myMap.put("key1", 1);
myMap.put("key2", 2);

```

If you call the put method with the Key that already exists in the Map, the method will override its value and return the old value.

**3. Getting values from HashMap.**

For getting the value from a HashMap you have to call the `get` method, by passing the Key as a parameter.

```java
myMap.get("key1");    //return 1 (class Integer)

```

If you pass a key that does not exists in the HashMap, this method will return `null`

**4. Check whether the Key is in the Map or not.**

```java
myMap.containsKey(varKey);

```

**5. Check whether the Value is in the Map or not.**

```java
myMap.containsValue(varValue);

```

The above methods will return a `boolean` value true or false if key, value exists in the Map or not.



## Iterating through the contents of a Map


Maps provide methods which let you access the keys, values, or key-value pairs of the map as collections. You can iterate through these collections. Given the following map for example:

```java
Map<String, Integer> repMap = new HashMap<>();
repMap.put("Jon Skeet", 927_654);
repMap.put("BalusC", 708_826);
repMap.put("Darin Dimitrov", 715_567);

```

**Iterating through map keys:**

```java
for (String key : repMap.keySet()) {
    System.out.println(key);
}

```

Prints:

> 
<p>Darin Dimitrov<br>
Jon Skeet<br>
BalusC</p>


`keySet()` provides the keys of the map as a [`Set`](http://docs.oracle.com/javase/8/docs/api/java/util/Set.html). `Set` is used as the keys cannot contain duplicate values. Iterating through the set yields each key in turn. HashMaps are not ordered, so in this example the keys may be returned in any order.

**Iterating through map values:**

```java
for (Integer value : repMap.values()) {
    System.out.println(value);
}

```

Prints:

> 
<p>715567<br>
927654<br>
708826</p>


`values()` returns the values of the map as a [`Collection`](http://docs.oracle.com/javase/8/docs/api/java/util/Collection.html). Iterating through the collection yields each value in turn. Again, the values may be returned in any order.

**Iterating through keys and values together**

```java
for (Map.Entry<String, Integer> entry : repMap.entrySet()) {
    System.out.printf("%s = %d\n", entry.getKey(), entry.getValue());
}

```

Prints:

> 
<p>Darin Dimitrov = 715567<br>
Jon Skeet = 927654<br>
BalusC = 708826</p>


`entrySet()` returns a collection of [`Map.Entry`](http://docs.oracle.com/javase/8/docs/api/java/util/Map.Entry.html) objects. Map.Entry gives access to the key and value for each entry.



## Add multiple items


We can use `V put(K key,V value)`:

> 
<p>Associates the specified value with the specified key in this map
(optional operation). If the map previously contained a mapping for
the key, the old value is replaced by the specified value.</p>


```java
String currentVal;
Map<Integer, String> map = new TreeMap<>();
currentVal = map.put(1, "First element.");
System.out.println(currentVal);// Will print null
currentVal = map.put(2, "Second element.");
System.out.println(currentVal); // Will print null yet again    
currentVal = map.put(2, "This will replace 'Second element'");
System.out.println(currentVal); // will print Second element.
System.out.println(map.size()); // Will print 2 as key having
// value 2 was replaced.

Map<Integer, String> map2 = new HashMap<>();
map2.put(2, "Element 2");
map2.put(3, "Element 3");

map.putAll(map2);

System.out.println(map.size());  

```

Output:

`3`

To add many items you can use an inner classes like this:

```java
Map<Integer, String> map = new HashMap<>() {{
    // This is now an anonymous inner class with an unnamed instance constructor
    put(5, "high");
    put(4, "low");
    put(1, "too slow");
}};

```

Keep in mind that creating an anonymous inner class is not always efficient and can lead to memory leaks so when possible, use an initializer block instead:

```java
static Map<Integer, String> map = new HashMap<>();

static {
    // Now no inner classes are created so we can avoid memory leaks
    put(5, "high");
    put(4, "low");
    put(1, "too slow");
}

```

The example above makes the map static. It can also be used in a non-static context by removing all occurences of `static`.

In addition to that most implementations support `putAll`, which can add all entries in one map to another like this:

```java
another.putAll(one);

```



## Merging, combine and composing Maps


Use `putAll` to put every member of one map into another.  Keys already present in the map will have their corresponding values overwritten.

```java
Map<String, Integer> numbers = new HashMap<>();
numbers.put("One", 1)
numbers.put("Three", 3)
Map<String, Integer> other_numbers = new HashMap<>();
other_numbers.put("Two", 2)
other_numbers.put("Three", 4)

numbers.putAll(other_numbers)

```

This yields the following mapping in `numbers`:

```java
"One" -> 1
"Two" -> 2
"Three" -> 4 //old value 3 was overwritten by new value 4

```

If you want to combine values instead of overwriting them, you can use [`Map.merge`](http://docs.oracle.com/javase/8/docs/api/java/util/Map.html#merge-K-V-java.util.function.BiFunction-), added in Java 8, which uses a user-provided `BiFunction` to merge values for duplicate keys.  `merge` operates on individual keys and values, so you'll need to use a loop or `Map.forEach`.  Here we concatenate strings for duplicate keys:

```java
for (Map.Entry<String, Integer> e : other_numbers.entrySet())
    numbers.merge(e.getKey(), e.getValue(), Integer::sum);
//or instead of the above loop
other_numbers.forEach((k, v) -> numbers.merge(k, v, Integer::sum));

```

If you want to enforce the constraint there are no duplicate keys, you can use a merge function that throws an `AssertionError`:

```java
mapA.forEach((k, v) ->
    mapB.merge(k, v, (v1, v2) ->
        {throw new AssertionError("duplicate values for key: "+k);}));

```

### Composing Map<X,Y> and Map<Y,Z> to get Map<X,Z>

If you want to compose two mappings, you can do it as follows

```

   Map<String, Integer> map1 = new HashMap<String, Integer>();
    map1.put("key1", 1);
    map1.put("key2", 2);
    map1.put("key3", 3);
    
    Map<Integer, Double> map2 = new HashMap<Integer, Double>();
    map2.put(1, 1.0);
    map2.put(2, 2.0);
    map2.put(3, 3.0);

    Map<String, Double> map3 = new new HashMap<String, Double>();
    map1.forEach((key,value)->map3.put(key,map2.get(value)));

```

This yields the following mapping

```

   "key1" -> 1.0
    "key2" -> 2.0
    "key3" -> 3.0

```



## Add an element


1. Addition

```java
Map<Integer, String> map = new HashMap<>();
map.put(1, "First element."); 
System.out.println(map.get(1));

```

Output: `First element.`

1. Override

```java
Map<Integer, String> map = new HashMap<>();
map.put(1, "First element.");
map.put(1, "New element.");
System.out.println(map.get(1));

```

Output: `New element.`

`HashMap` is used as an example. Other implementations that implement the `Map` interface may be used as well.



## Clear the map


```java
Map<Integer, String> map = new HashMap<>();

map.put(1, "First element.");
map.put(2, "Second element.");
map.put(3, "Third element.");

map.clear();

System.out.println(map.size());   // => 0

```



## Check if key exists


```java
Map<String, String> num = new HashMap<>();
num.put("one", "first");

if (num.containsKey("one")) {
    System.out.println(num.get("one")); // => first
}

```

### Maps can contain null values

For maps, one has to be carrefull not to confuse "containing a key" with "having a value". For example, `HashMap`s can contain null which means the following is perfectly normal behavior :

```java
Map<String, String> map = new HashMap<>();
map.put("one", null);
if (map.containsKey("one")) {
    System.out.println("This prints !"); // This line is reached 
}
if (map.get("one") != null) {
    System.out.println("This is never reached !"); // This line is never reached 
}

```

More formally, there is no guarantee that `map.contains(key) <=> map.get(key)!=null`



## Use custom object as key


Before using your own object as key you must override hashCode() and equals() method of your object.

In simple case you would have something like:

```java
class MyKey {
    private String name;
    MyKey(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object obj) {            
        if(obj instanceof MyKey) {
            return this.name.equals(((MyKey)obj).name);
        }
        return false;
    }
    
    @Override
    public int hashCode() {
        return this.name.hashCode();
    }
}

```

`hashCode` will decide which hash bucket the key belongs to and `equals` will decide which object inside that hash bucket.

Without these method, the reference of your object will be used for above comparison which will not work unless you use the same object reference everytime.



## Creating and Initializing Maps


### **Introduction**

`Maps` stores key/value pairs, where each key has an associated value. Given a particular key, the map can look up the associated value very quickly.

`Maps`, also known as associate array, is an object that stores the data in form of keys and values. In Java, maps are represented using Map interface which is not an extension of the collection interface.

<li>
Way 1 :-

```java
 /*J2SE < 5.0*/
 Map map = new HashMap();
 map.put("name", "A");
 map.put("address", "Malviya-Nagar");
 map.put("city", "Jaipur");
 System.out.println(map);

```


</li>
<li>
Way 2 :-

```java
 /*J2SE 5.0+ style (use of generics):*/
 Map<String, Object> map = new HashMap<>();
 map.put("name", "A");
 map.put("address", "Malviya-Nagar");
 map.put("city", "Jaipur");
 System.out.println(map);

```


</li>
<li>
Way 3 :-

```java
  Map<String, Object> map = new HashMap<String, Object>(){{
      put("name", "A");
      put("address", "Malviya-Nagar");
      put("city", "Jaipur");
  }};
  System.out.println(map);

```


</li>
<li>
Way 4 :-

```java
  Map<String, Object> map = new TreeMap<String, Object>();
      map.put("name", "A");
      map.put("address", "Malviya-Nagar");
      map.put("city", "Jaipur");
  System.out.println(map);

```


</li>
<li>
Way 5 :-

```java
  //Java 8
  final Map<String, String> map =
      Arrays.stream(new String[][] {
          { "name", "A" }, 
          { "address", "Malviya-Nagar" }, 
          { "city", "jaipur" },
      }).collect(Collectors.toMap(m -> m[0], m -> m[1]));
  System.out.println(map);

```


</li>
<li>
Way 6 :-

```java
  //This way for initial a map in outside the function
  final static Map<String, String> map;
  static
  {
      map = new HashMap<String, String>();
      map.put("a", "b");
      map.put("c", "d");
  }

```


</li>
<li>
Way 7 :- Creating an immutable single key-value map.

```java
    //Immutable single key-value map
    Map<String, String> singletonMap = Collections.singletonMap("key", "value");

```


Please note, that **it is impossible to modify such map**.
Any attemts to modify the map will result in throwing the UnsupportedOperationException.

```java
    //Immutable single key-value pair
    Map<String, String> singletonMap = Collections.singletonMap("key", "value");
    singletonMap.put("newKey", "newValue"); //will throw UnsupportedOperationException
    singletonMap.putAll(new HashMap<>()); //will throw UnsupportedOperationException
    singletonMap.remove("key"); //will throw UnsupportedOperationException
    singletonMap.replace("key", "value", "newValue"); //will throw UnsupportedOperationException
    //and etc

```


</li>



#### Remarks


A [**map**](https://docs.oracle.com/javase/8/docs/api/java/util/Map.html) is an object which store **keys** with an associated **value** for each key. A key and its value are sometimes called a **key/value pair** or an **entry**. Maps typically provide these features:

- Data is stored into the map in key/value pairs.
- The map may contain only one entry for a particular key. If a map contains an entry with a particular key, and you try to store a second entry with the same key, then the second entry will replace the first. In other words, this will change the value associated with the key.
- Maps provide fast operations to test whether a key exists in the map, to fetch the value associated with a key, and to remove a key/value pair.

The most commonly used map implementation is [HashMap](https://docs.oracle.com/javase/8/docs/api/java/util/HashMap.html). It works well with keys that are strings or numbers.

Plain maps such as HashMap are unordered. Iterating through key/value pairs may return individual entries in any order. If you need to iterate through map entries in a controlled fashion, you should look at the following:

<li>
[Sorted maps](https://docs.oracle.com/javase/8/docs/api/java/util/SortedMap.html) such as [TreeMap](https://docs.oracle.com/javase/8/docs/api/java/util/TreeMap.html) will iterate through keys in their natural order (or in an order that you can specify, by providing a [Comparator](https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html)). For example, a sorted map using numbers as keys would be expected to iterate through its entries in numeric order.
</li>
<li>
[LinkedHashMap](https://docs.oracle.com/javase/8/docs/api/java/util/LinkedHashMap.html) permits iterating through entries in the same order that they were inserted into the map, or by the order of most recently accessed.
</li>

