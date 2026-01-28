---
metaTitle: "Java - SortedMap"
description: "Introduction to sorted Map."
---

# SortedMap


Introduction to sorted Map.



## Introduction to sorted Map.


**Keypoint :-**

- SortedMap interface extends Map.
- entries are maintained in an ascending key order.

**Methods of sorted Map :**

- Comparator comparator( ).
- Object firstKey( ).
- SortedMap headMap(Object end).
- Object lastKey( ).
- SortedMap subMap(Object start, Object end).
- SortedMap tailMap(Object start).

**Example**

```java
public static void main(String args[]) {
      // Create a hash map
      TreeMap tm = new TreeMap();
      
      // Put elements to the map
      tm.put("Zara", new Double(3434.34));
      tm.put("Mahnaz", new Double(123.22));
      tm.put("Ayan", new Double(1378.00));
      tm.put("Daisy", new Double(99.22));
      tm.put("Qadir", new Double(-19.08));
      
      // Get a set of the entries
      Set set = tm.entrySet();
      
      // Get an iterator
      Iterator i = set.iterator();
      
      // Display elements
      while(i.hasNext()) {
         Map.Entry me = (Map.Entry)i.next();
         System.out.print(me.getKey() + ": ");
         System.out.println(me.getValue());
      }
      System.out.println();
      
      // Deposit 1000 into Zara's account
      double balance = ((Double)tm.get("Zara")).doubleValue();
      tm.put("Zara", new Double(balance + 1000));
      System.out.println("Zara's new balance: " + tm.get("Zara"));
   }

```

