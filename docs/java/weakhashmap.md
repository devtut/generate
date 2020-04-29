---
metaTitle: "WeakHashMap"
description: "Concepts of WeakHashmap"
---

# WeakHashMap


Concepts of weak Hashmap



## Concepts of WeakHashmap


**Key Points:-**

- Implementation of Map.
- stores only weak references to its keys.

**Weak References** :
The objects that are referenced only by weak references are garbage collected eagerly; the GC won’t wait until it needs memory in that case.

**Diffrence between Hashmap and WeakHashMap:-**

If the Java memory manager no longer has a strong reference to the object specified as a key, then the entry in the map will be removed in WeakHashMap.

**Example :-**

```java
public class WeakHashMapTest {
    public static void main(String[] args) {
        Map hashMap= new HashMap();
        
        Map weakHashMap = new WeakHashMap();
        
        String keyHashMap = new String("keyHashMap");
        String keyWeakHashMap = new String("keyWeakHashMap");
        
        hashMap.put(keyHashMap, "Ankita");
        weakHashMap.put(keyWeakHashMap, "Atul");
        System.gc();
        System.out.println("Before: hash map value:"+hashMap.get("keyHashMap")+" and weak hash map value:"+weakHashMap.get("keyWeakHashMap"));
        
        keyHashMap = null;
        keyWeakHashMap = null;
        
        System.gc();  
        
        System.out.println("After: hash map value:"+hashMap.get("keyHashMap")+" and weak hash map value:"+weakHashMap.get("keyWeakHashMap"));
    }

```

****Size differences (HashMap vs WeakHashMap):****

Calling size()  method on HashMap object will return the same number of key-value pairs. size will decrease only if remove() method is called explicitly on the HashMap object.

Because the garbage collector may discard keys at anytime, a WeakHashMap may behave as though an unknown thread is silently removing entries. So it is possible for the size method to return smaller values over time.So, in **WeakHashMap  size decrease happens automatically**.

