---
metaTitle: "Collection Factory Methods"
description: "List<E> Factory Method Examples, Set<E> Factory Method Examples, Map<K, V> Factory Method Examples"
---

# Collection Factory Methods


The arrival of Java 9 brings many new features to Java's Collections API, one of which being collection factory methods.  These methods allow for easy initialization of **immutable** collections, whether they be empty or nonempty.

Note that these factory methods are only available for the following interfaces: `List<E>`, `Set<E>`, and `Map<K, V>`



## List<E> Factory Method Examples


<li>`List<Integer> immutableEmptyList = List.of();`
<ul>
- Initializes an empty, immutable `List<Integer>`.

- Initializes an immutable `List<Integer>` with five initial elements.

- Initializes a mutable `List<Integer>` from an immutable `List<Integer>`.



## Set<E> Factory Method Examples


<li>`Set<Integer> immutableEmptySet = Set.of();`
<ul>
- Initializes an empty, immutable `Set<Integer>`.

- Initializes an immutable `Set<Integer>` with five initial elements.

- Initializes a mutable `Set<Integer>` from an immutable `Set<Integer>`.



## Map<K, V> Factory Method Examples


<li>`Map<Integer, Integer> immutableEmptyMap = Map.of();`
<ul>
- Initializes an empty, immutable `Map<Integer, Integer>`.

- Initializes an immutable `Map<Integer, Integer>` with two initial key-value entries.

- Initializes an immutable `Map<Integer, Integer>` with two initial key-value entries.

- Initializes a mutable `Map<Integer, Integer>` from an immutable `Map<Integer, Integer>`.



#### Syntax


- `static <E> List<E> of​()`
- `static <E> List<E> of​(E e1)`
- `static <E> List<E> of​(E e1, E e2)`
- `static <E> List<E> of​(E e1, E e2, ..., E e9, E e10)`
- `static <E> List<E> of​(E... elements)`
- `static <E> Set<E> of​()`
- `static <E> Set<E> of​(E e1)`
- `static <E> Set<E> of​(E e1, E e2)`
- `static <E> Set<E> of​(E e1, E e2, ..., E e9, E e10)`
- `static <E> Set<E> of​(E... elements)`
- `static <K,V> Map<K,V> of()`
- `static <K,V> Map<K,V> of(K k1, V v1)`
- `static <K,V> Map<K,V> of(K k1, V v1, K k2, V v2)`
- `static <K,V> Map<K,V> of(K k1, V v1, K k2, V v2, ..., K k9, V v9, K k10, V v10)`
- `static <K,V> Map<K,V> ofEntries​(Map.Entry<? extends K,? extends V>... entries)`



#### Parameters


|Method w/ Parameter|Description
|------
|`List.of(E e)`|A generic type that can be a class or interface.
|`Set.of(E e)`|A generic type that can be a class or interface.
|`Map.of(K k, V v)`|A key-value pair of generic types that can each be a class or interface.
|`Map.of(Map.Entry<? extends K, ? extends V> entry)`|A `Map.Entry` instance where its key can be `K` or one of its children, and its value can be `V` or any of its children.

