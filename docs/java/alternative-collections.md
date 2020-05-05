---
metaTitle: "Java - Alternative Collections"
description: "Multimap in Guava, Apache and Eclipse Collections, Apache HashBag, Guava HashMultiset and Eclipse HashBag, Compare operation with collections - Create collections"
---

# Alternative Collections



## Multimap in Guava, Apache and Eclipse Collections


This multimap allows duplicate key-value pairs. JDK analogs are HashMap<K, List>, HashMap<K, Set> and so on.

|Key's order|Value's order|Duplicate|Analog key|Analog value|Guava|Apache|Eclipse (GS) Collections|JDK
|---|---|---|---|---|---|---|---|---|---
|not defined|Insertion-order|yes|[HashMap](https://docs.oracle.com/javase/8/docs/api/java/util/HashMap.html)|[ArrayList](https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html)|[ArrayListMultimap](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/ArrayListMultimap.html)|`MultiValueMap`|`FastListMultimap`|`HashMap<K, ArrayList<V>>`
|not defined|not defined|no|[HashMap](https://docs.oracle.com/javase/8/docs/api/java/util/HashMap.html)|[HashSet](https://docs.oracle.com/javase/8/docs/api/java/util/HashSet.html)|[HashMultimap](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/HashMultimap.html)|`MultiValueMap. multiValueMap( new HashMap<K, Set>(), HashSet.class);`|`UnifiedSetMultimap`|`HashMap<K, HashSet<V>>`
|not defined|sorted|no|[HashMap](https://docs.oracle.com/javase/8/docs/api/java/util/HashMap.html)|[TreeSet](https://docs.oracle.com/javase/8/docs/api/java/util/TreeSet.html)|`Multimaps. newMultimap( HashMap, Supplier <TreeSet>)`|`MultiValueMap.multiValueMap( new HashMap<K, Set>(), TreeSet.class)`|`TreeSortedSet- Multimap`|`HashMap<K, TreeSet<V>>`
|Insertion-order|Insertion-order|yes|[LinkedHashMap](https://docs.oracle.com/javase/8/docs/api/java/util/LinkedHashMap.html)|[ArrayList](https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html)|[LinkedListMultimap](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/LinkedListMultimap.html)|MultiValueMap. multiValueMap(new LinkedHashMap<K, List>(), ArrayList.class);||LinkedHashMap< K, ArrayList>
|Insertion-order|Insertion-order|no|[LinkedHashMap](https://docs.oracle.com/javase/8/docs/api/java/util/LinkedHashMap.html)|[LinkedHashSet](https://docs.oracle.com/javase/8/docs/api/java/util/LinkedHashSet.html)|[LinkedHashMultimap](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/LinkedHashMultimap.html)|`MultiValueMap. multiValueMap(new LinkedHashMap<K, Set>(), LinkedHashSet.class)`||`LinkedHashMap<K, LinkedHashSet<V>>`
|sorted|sorted|no|[TreeMap](https://docs.oracle.com/javase/8/docs/api/java/util/TreeMap.html)|[TreeSet](https://docs.oracle.com/javase/8/docs/api/java/util/TreeSet.html)|[TreeMultimap](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/TreeMultimap.html)|`MultiValueMap. multiValueMap( new TreeMap<K, Set>(),TreeSet.class)`||`TreeMap<K, TreeSet<V>>`

**Task**: Parse "Hello World! Hello All! Hi World!" string to separate words and print all indexes of every word using MultiMap (for example, Hello=[0, 2], World!=[1, 5] and so on)

**1. MultiValueMap from  Apache**

```

   String INPUT_TEXT = "Hello World! Hello All! Hi World!";
    // Parse text to words and index
    List<String> words = Arrays.asList(INPUT_TEXT.split(" "));
    // Create Multimap
    MultiMap<String, Integer> multiMap = new MultiValueMap<String, Integer>();


    // Fill Multimap
    int i = 0;
    for(String word: words) {
        multiMap.put(word, i);
        i++;
    }

    // Print all words
    System.out.println(multiMap); // print {Hi=[4], Hello=[0, 2], World!=[1, 5], All!=[3]} - in random orders
    // Print all unique words
    System.out.println(multiMap.keySet());    // print [Hi, Hello, World!, All!] - in random orders

    // Print all indexes
    System.out.println("Hello = " + multiMap.get("Hello"));    // print [0, 2]
    System.out.println("World = " + multiMap.get("World!"));    // print [1, 5]
    System.out.println("All = " + multiMap.get("All!"));    // print [3]
    System.out.println("Hi = " + multiMap.get("Hi"));    // print [4]
    System.out.println("Empty = " + multiMap.get("Empty"));    // print null

    // Print count unique words
    System.out.println(multiMap.keySet().size());    //print 4

```

**2. HashBiMap from GS / Eclipse Collection**

```

   String[] englishWords = {"one", "two", "three","ball","snow"};
    String[] russianWords = {"jeden", "dwa", "trzy", "kula", "snieg"};

    // Create Multiset
    MutableBiMap<String, String> biMap = new HashBiMap(englishWords.length);
    // Create English-Polish dictionary
    int i = 0;
    for(String englishWord: englishWords) {
        biMap.put(englishWord, russianWords[i]);
        i++;
    }

    // Print count words
    System.out.println(biMap); // print {two=dwa, ball=kula, one=jeden, snow=snieg, three=trzy} - in random orders
    // Print all unique words
    System.out.println(biMap.keySet());    // print [snow, two, one, three, ball] - in random orders
    System.out.println(biMap.values());    // print [dwa, kula, jeden, snieg, trzy] - in random orders

    // Print translate by words
    System.out.println("one = " + biMap.get("one"));    // print one = jeden
    System.out.println("two = " + biMap.get("two"));    // print two = dwa
    System.out.println("kula = " + biMap.inverse().get("kula"));    // print kula = ball
    System.out.println("snieg = " + biMap.inverse().get("snieg"));    // print snieg = snow
    System.out.println("empty = " + biMap.get("empty"));    // print empty = null

    // Print count word's pair
    System.out.println(biMap.size());    //print 5

```


<li>
HashMultiMap from Guava

```java
 String INPUT_TEXT = "Hello World! Hello All! Hi World!";
 // Parse text to words and index
 List<String> words = Arrays.asList(INPUT_TEXT.split(" "));
 // Create Multimap
 Multimap<String, Integer> multiMap = HashMultimap.create();

 // Fill Multimap
 int i = 0;
 for(String word: words) {
     multiMap.put(word, i);
     i++;
 }

 // Print all words
 System.out.println(multiMap); // print {Hi=[4], Hello=[0, 2], World!=[1, 5], All!=[3]} - keys and values in random orders
 // Print all unique words
 System.out.println(multiMap.keySet());    // print [Hi, Hello, World!, All!] - in random orders

 // Print all indexes
 System.out.println("Hello = " + multiMap.get("Hello"));    // print [0, 2]
 System.out.println("World = " + multiMap.get("World!"));    // print [1, 5]
 System.out.println("All = " + multiMap.get("All!"));    // print [3]
 System.out.println("Hi = " + multiMap.get("Hi"));    // print [4]
 System.out.println("Empty = " + multiMap.get("Empty"));    // print []

 // Print count all words
 System.out.println(multiMap.size());    //print 6

 // Print count unique words
 System.out.println(multiMap.keySet().size());    //print 4

```


</li>

### **Nore examples:**

**I. Apache Collection:**

1. [MultiValueMap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/ApacheMultiValueMapTest.java)
1. [MultiValueMapLinked](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/ApacheMultiValueMapLinkedTest.java)
1. [MultiValueMapTree](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/ApacheMultiValueMapTreeTest.java)

**II. GS / Eclipse Collection**

1. [FastListMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/gs-eclipse/src/GsFastListMultimapTest.java)
1. [HashBagMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/gs-eclipse/src/GsHashBiMapTest.java)
1. [TreeSortedSetMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/gs-eclipse/src/GsTreeSortedSetMultimapTest.java)
1. [UnifiedSetMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/gs-eclipse/src/GsUnifiedSetMultimapTest.java)

**III. Guava**

1. [HashMultiMap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/HashMultiMapTest.java)
1. [LinkedHashMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/LinkedHashMultimapTest.java)
1. [LinkedListMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/LinkedListMultimapTest.java)
1. [TreeMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/TreeMultimapTest.java)
1. [ArrayListMultimap](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/ArrayListMultimapTest.java)



## Apache HashBag, Guava HashMultiset and Eclipse HashBag


A Bag/ultiset stores each object in the collection together with a count of occurrences. Extra methods on the interface allow multiple copies of an object to be added or removed at once. JDK analog is HashMap<T, Integer>, when values is count of copies this key.

|**Type**|**Guava**|**Apache Commons Collections**|**GS Collections**|**JDK**
|---|---|---|---|---|---|---|---|---|---
|Order not defined|[HashMultiset](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/HashMultiset.html)|[HashBag](http://commons.apache.org/proper/commons-collections/javadocs/api-3.2.1/org/apache/commons/collections/bag/HashBag.html)|[HashBag](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/mutable/HashBag.html)|[HashMap](https://docs.oracle.com/javase/8/docs/api/java/util/HashMap.html)
|Sorted|[TreeMultiset](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/TreeMultiset.html)|[TreeBag](http://commons.apache.org/proper/commons-collections/javadocs/api-3.2.1/org/apache/commons/collections/TreeBag.html)|[TreeBag](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/sorted/mutable/TreeBag.html)|[TreeMap](https://docs.oracle.com/javase/8/docs/api/java/util/TreeMap.html)
|Insertion-order|[LinkedHashMultiset](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/LinkedHashMultiset.html)|-|-|[LinkedHashMap](https://docs.oracle.com/javase/8/docs/api/java/util/LinkedHashMap.html)
|Concurrent variant|[ConcurrentHashMultiset](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/ConcurrentHashMultiset.html)|[SynchronizedBag](http://commons.apache.org/proper/commons-collections/javadocs/api-3.2.1/org/apache/commons/collections/bag/SynchronizedBag.html)|[SynchronizedBag](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/mutable/SynchronizedBag.html)|`Collections.synchronizedMap(HashMap<String, Integer>)`
|Concurrent and sorted|-|[SynchronizedSortedBag](http://commons.apache.org/proper/commons-collections/javadocs/api-3.2.1/org/apache/commons/collections/bag/SynchronizedSortedBag.html)|[SynchronizedSortedBag](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/sorted/mutable/SynchronizedSortedBag.html)|`Collections.synchronizedSortedMap(TreeMap<String,Integer>)`
|Immutable collection|[ImmutableMultiset](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/ImmutableMultiset.html)|[UnmodifiableBag](http://commons.apache.org/proper/commons-collections/javadocs/api-3.2.1/org/apache/commons/collections/bag/UnmodifiableBag.html)|[UnmodifiableBag](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/mutable/UnmodifiableBag.html)|`Collections.unmodifiableMap(HashMap<String, Integer)]`
|Immutable and sorted|[ImmutableSortedMultiset](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/collect/ImmutableSortedMultiset.html)|[UnmodifiableSortedBag](http://commons.apache.org/proper/commons-collections/javadocs/api-3.2.1/org/apache/commons/collections/bag/UnmodifiableSortedBag.html)|UnmodifiableSortedBag|`Collections.unmodifiableSortedMap(TreeMap<String, Integer>`)

**Examples**:

### 1. [Using SynchronizedSortedBag from Apache](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/sorted/mutable/SynchronizedSortedBag.html):

```

   // Parse text to separate words
    String INPUT_TEXT = "Hello World! Hello All! Hi World!";
    // Create Multiset
    Bag bag = SynchronizedSortedBag.synchronizedBag(new TreeBag(Arrays.asList(INPUT_TEXT.split(" "))));

    // Print count words
    System.out.println(bag); // print [1:All!,2:Hello,1:Hi,2:World!]- in natural (alphabet) order
    // Print all unique words
    System.out.println(bag.uniqueSet());    // print [All!, Hello, Hi, World!]- in natural (alphabet) order


    // Print count occurrences of words
    System.out.println("Hello = " + bag.getCount("Hello"));    // print 2
    System.out.println("World = " + bag.getCount("World!"));    // print 2
    System.out.println("All = " + bag.getCount("All!"));    // print 1
    System.out.println("Hi = " + bag.getCount("Hi"));    // print 1
    System.out.println("Empty = " + bag.getCount("Empty"));    // print 0

    // Print count all words
    System.out.println(bag.size());    //print 6

    // Print count unique words
    System.out.println(bag.uniqueSet().size());    //print 4

```

### [2. Using TreeBag from Eclipse(GC)](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/sorted/mutable/TreeBag.html):

```

   // Parse text to separate words
    String INPUT_TEXT = "Hello World! Hello All! Hi World!";
    // Create Multiset
    MutableSortedBag<String> bag =  TreeBag.newBag(Arrays.asList(INPUT_TEXT.split(" ")));

    // Print count words
    System.out.println(bag); // print [All!, Hello, Hello, Hi, World!, World!]- in natural order
    // Print all unique words
    System.out.println(bag.toSortedSet());    // print [All!, Hello, Hi, World!]- in natural order

    // Print count occurrences of words
    System.out.println("Hello = " + bag.occurrencesOf("Hello"));    // print 2
    System.out.println("World = " + bag.occurrencesOf("World!"));    // print 2
    System.out.println("All = " + bag.occurrencesOf("All!"));    // print 1
    System.out.println("Hi = " + bag.occurrencesOf("Hi"));    // print 1
    System.out.println("Empty = " + bag.occurrencesOf("Empty"));    // print 0

    // Print count all words
    System.out.println(bag.size());    //print 6

    // Print count unique words
    System.out.println(bag.toSet().size());    //print 4

```

### 3. [Using LinkedHashMultiset from Guava](http://www.goldmansachs.com/gs-collections/javadoc/6.1.0/com/gs/collections/impl/bag/sorted/mutable/SynchronizedSortedBag.html):

```

   // Parse text to separate words
    String INPUT_TEXT = "Hello World! Hello All! Hi World!";
    // Create Multiset
    Multiset<String> multiset = LinkedHashMultiset.create(Arrays.asList(INPUT_TEXT.split(" ")));

    // Print count words
    System.out.println(multiset); // print [Hello x 2, World! x 2, All!, Hi]- in predictable iteration order
    // Print all unique words
    System.out.println(multiset.elementSet());    // print [Hello, World!, All!, Hi] - in predictable iteration order

    // Print count occurrences of words
    System.out.println("Hello = " + multiset.count("Hello"));    // print 2
    System.out.println("World = " + multiset.count("World!"));    // print 2
    System.out.println("All = " + multiset.count("All!"));    // print 1
    System.out.println("Hi = " + multiset.count("Hi"));    // print 1
    System.out.println("Empty = " + multiset.count("Empty"));    // print 0

    // Print count all words
    System.out.println(multiset.size());    //print 6

    // Print count unique words
    System.out.println(multiset.elementSet().size());    //print 4

```

### **More examples:**

**I. Apache Collection:**

1. [HashBag](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/ApacheHashBagTest.java) - order not defined
1. [SynchronizedBag](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/ApacheSynchronizedBagTest.java) - concurrent and order not defined
1. [SynchronizedSortedBag](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/ApacheSynchronizedSortedBagTest.java) - - concurrent and sorted order
1. [TreeBag](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/ApacheTreeBagTest.java) - sorted order

**II. GS / Eclipse Collection**

1. [MutableBag](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/gs-eclipse/src/GsMutableBagTest.java) - order not defined
1. [MutableSortedBag](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/gs-eclipse/src/GsMutableSortedBagTest.java) - sorted order

**III. Guava**

1. [HashMultiset](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/guava/src/GuavaHashMultisetTest.java) - order not defined
1. [TreeMultiset](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/TreeMultisetTest.java) - sorted order
1. [LinkedHashMultiset](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/LinkedHashMultisetTest.java) - insertion order
1. [ConcurrentHashMultiset](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections//src/ConcurrentHashMultisetTest.java) - concurrent and order not defined



## Compare operation with collections - Create collections


### Compare operation with collections - Create collections

|Description|JDK|guava|gs-collections
|---|---|---|---|---|---|---|---|---|---
|Create empty list|`new ArrayList<>`()|`Lists.newArrayList()`|`FastList.newList()`
|Create list from values|`Arrays.asList("1", "2", "3")`|`Lists.newArrayList("1", "2", "3")`|`FastList.newListWith("1", "2", "3")`
|Create list with capacity = 100|`new ArrayList<>(100)`|`Lists.newArrayListWithCapacity(100)`|`FastList.newList(100)`
|Create list from any collectin|`new ArrayList<>(collection)`|`Lists.newArrayList(collection)`|`FastList.newList(collection)`
|Create list from any Iterable|-|`Lists.newArrayList(iterable)`|`FastList.newList(iterable)`
|Create list from Iterator|-|`Lists.newArrayList(iterator)`|-
|Create list from array|`Arrays.asList(array)`|`Lists.newArrayList(array)`|`FastList.newListWith(array)`
|Create list using factory|-|-|`FastList.newWithNValues(10, () -> "1")`

**Examples:**

```

   System.out.println("createArrayList start");
    // Create empty list
    List<String> emptyGuava = Lists.newArrayList(); // using guava
    List<String> emptyJDK = new ArrayList<>(); // using JDK
    MutableList<String>  emptyGS = FastList.newList(); // using gs

    // Create list with 100 element
    List < String > exactly100 = Lists.newArrayListWithCapacity(100); // using guava
    List<String> exactly100JDK = new ArrayList<>(100); // using JDK
    MutableList<String>  empty100GS = FastList.newList(100); // using gs

    // Create list with about 100 element
    List<String> approx100 = Lists.newArrayListWithExpectedSize(100); // using guava
    List<String> approx100JDK = new ArrayList<>(115); // using JDK
    MutableList<String>  approx100GS = FastList.newList(115); // using gs

    // Create list with some elements
    List<String> withElements = Lists.newArrayList("alpha", "beta", "gamma"); // using guava
    List<String> withElementsJDK = Arrays.asList("alpha", "beta", "gamma"); // using JDK
    MutableList<String>  withElementsGS = FastList.newListWith("alpha", "beta", "gamma"); // using gs

    System.out.println(withElements);
    System.out.println(withElementsJDK);
    System.out.println(withElementsGS);

    // Create list from any Iterable interface (any collection)
    Collection<String> collection = new HashSet<>(3);
    collection.add("1");
    collection.add("2");
    collection.add("3");

    List<String> fromIterable = Lists.newArrayList(collection); // using guava
    List<String> fromIterableJDK = new ArrayList<>(collection); // using JDK
    MutableList<String>  fromIterableGS = FastList.newList(collection); // using gs

    System.out.println(fromIterable);
    System.out.println(fromIterableJDK);
    System.out.println(fromIterableGS);
    /* Attention: JDK create list only from Collection, but guava and gs can create list from Iterable and Collection */

    // Create list from any Iterator
    Iterator<String> iterator = collection.iterator();
    List<String> fromIterator = Lists.newArrayList(iterator); // using guava
    System.out.println(fromIterator);

    // Create list from any array
    String[] array = {"4", "5", "6"};
    List<String> fromArray = Lists.newArrayList(array); // using guava
    List<String> fromArrayJDK = Arrays.asList(array); // using JDK
    MutableList<String>  fromArrayGS = FastList.newListWith(array); // using gs
    System.out.println(fromArray);
    System.out.println(fromArrayJDK);
    System.out.println(fromArrayGS);

    // Create list using fabric
    MutableList<String>  fromFabricGS = FastList.newWithNValues(10, () -> String.valueOf(Math.random())); // using gs
    System.out.println(fromFabricGS);

    System.out.println("createArrayList end");

```

|Description|JDK|guava|gs-collections
|---|---|---|---|---|---|---|---|---|---
|Create empty set|`new HashSet<>()`|`Sets.newHashSet()`|`UnifiedSet.newSet()`
|Creatre set from values|`new HashSet<>(Arrays.asList("alpha", "beta", "gamma")`)|`Sets.newHashSet("alpha", "beta", "gamma")`|`UnifiedSet.newSetWith("alpha", "beta", "gamma")`
|Create set from any collections|`new HashSet<>(collection)`|`Sets.newHashSet(collection)`|`UnifiedSet.newSet(collection)`
|Create set from any Iterable|-|`Sets.newHashSet(iterable)`|`UnifiedSet.newSet(iterable)`
|Create set from any Iterator|-|`Sets.newHashSet(iterator)`|-
|Create set from Array|`new HashSet<>(Arrays.asList(array))`|`Sets.newHashSet(array)`|`UnifiedSet.newSetWith(array)`

**Examples:**

```

   System.out.println("createHashSet start");
    // Create empty set
    Set<String> emptyGuava = Sets.newHashSet(); // using guava
    Set<String> emptyJDK = new HashSet<>(); // using JDK
    Set<String> emptyGS = UnifiedSet.newSet(); // using gs

    // Create set with 100 element
    Set<String> approx100 = Sets.newHashSetWithExpectedSize(100); // using guava
    Set<String> approx100JDK = new HashSet<>(130); // using JDK
    Set<String> approx100GS = UnifiedSet.newSet(130); // using gs

    // Create set from some elements
    Set<String> withElements =  Sets.newHashSet("alpha", "beta", "gamma"); // using guava
    Set<String> withElementsJDK = new HashSet<>(Arrays.asList("alpha", "beta", "gamma")); // using JDK
    Set<String> withElementsGS  = UnifiedSet.newSetWith("alpha", "beta", "gamma"); // using gs

    System.out.println(withElements);
    System.out.println(withElementsJDK);
    System.out.println(withElementsGS);

    // Create set from any Iterable interface (any collection)
    Collection<String> collection = new ArrayList<>(3);
    collection.add("1");
    collection.add("2");
    collection.add("3");

    Set<String> fromIterable = Sets.newHashSet(collection); // using guava
    Set<String> fromIterableJDK = new HashSet<>(collection); // using JDK
    Set<String> fromIterableGS  = UnifiedSet.newSet(collection); // using gs

    System.out.println(fromIterable);
    System.out.println(fromIterableJDK);
    System.out.println(fromIterableGS);
    /* Attention: JDK create set only from Collection, but guava and gs can create set from Iterable and Collection */

    // Create set from any Iterator
    Iterator<String> iterator = collection.iterator();
    Set<String> fromIterator = Sets.newHashSet(iterator); // using guava
    System.out.println(fromIterator);

    // Create set from any array
    String[] array = {"4", "5", "6"};
    Set<String> fromArray = Sets.newHashSet(array); // using guava
    Set<String> fromArrayJDK = new HashSet<>(Arrays.asList(array)); // using JDK
    Set<String> fromArrayGS  = UnifiedSet.newSetWith(array); // using gs
    System.out.println(fromArray);
    System.out.println(fromArrayJDK);
    System.out.println(fromArrayGS);

    System.out.println("createHashSet end");

```

|Description|JDK|guava|gs-collections
|---|---|---|---|---|---|---|---|---|---
|Create empty map|`new HashMap<>()`|`Maps.newHashMap()`|`UnifiedMap.newMap()`
|Create map with capacity = 130|`new HashMap<>(130)`|`Maps.newHashMapWithExpectedSize(100)`|`UnifiedMap.newMap(130)`
|Create map from other map|`new HashMap<>(map)`|`Maps.newHashMap(map)`|`UnifiedMap.newMap(map)`
|Create map from keys|-|-|`UnifiedMap.newWithKeysValues("1", "a", "2", "b")`

**Examples:**

```

   System.out.println("createHashMap start");
    // Create empty map
    Map<String, String> emptyGuava = Maps.newHashMap(); // using guava
    Map<String, String> emptyJDK = new HashMap<>(); // using JDK
    Map<String, String> emptyGS = UnifiedMap.newMap(); // using gs

    // Create map with about 100 element
    Map<String, String> approx100 = Maps.newHashMapWithExpectedSize(100); // using guava
    Map<String, String> approx100JDK = new HashMap<>(130); // using JDK
    Map<String, String> approx100GS = UnifiedMap.newMap(130); // using gs

    // Create map from another map
    Map<String, String> map = new HashMap<>(3);
    map.put("k1","v1");
    map.put("k2","v2");
    Map<String, String> withMap =  Maps.newHashMap(map); // using guava
    Map<String, String> withMapJDK = new HashMap<>(map); // using JDK
    Map<String, String> withMapGS = UnifiedMap.newMap(map); // using gs

    System.out.println(withMap);
    System.out.println(withMapJDK);
    System.out.println(withMapGS);

    // Create map from keys
    Map<String, String> withKeys =  UnifiedMap.newWithKeysValues("1", "a", "2", "b");
    System.out.println(withKeys);

    System.out.println("createHashMap end");

```

More examples: [CreateCollectionTest](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/gs-eclipse/src/CreateCollectionTest.java)

1. [CollectionCompare](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/CollectionCompareTests.java)
1. [CollectionSearch](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/CollectionSearchTests.java)
1. [JavaTransform](https://github.com/Vedenin/useful-java-links/blob/master/helloworlds/1.6-usefull-libraries/collections/apache-commons/src/JavaTransformTest.java)



#### Remarks


This topic about Java collections from guava, apache, eclipse: Multiset, Bag, Multimap, utils function from this lib and so on.

