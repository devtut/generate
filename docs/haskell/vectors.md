---
metaTitle: "Haskell - Vectors"
description: "Filtering a Vector, Mapping (`map`) and Reducing (`fold`) a Vector, Working on Multiple Vectors, The Data.Vector Module"
---

# Vectors



## Filtering a Vector


Filter odd elements:

```hs
Prelude Data.Vector> Data.Vector.filter odd y
fromList [1,3,5,7,9,11] :: Data.Vector.Vector

```



## Mapping (`map`) and Reducing (`fold`) a Vector


Vectors can be `map`'d and `fold'd,`filter`'d and`zip`'d:

```hs
Prelude Data.Vector> Data.Vector.map (^2) y
fromList [0,1,4,9,16,25,36,49,64,81,100,121] :: Data.Vector.Vector

```

Reduce to a single value:

```hs
Prelude Data.Vector> Data.Vector.foldl (+) 0 y
66

```



## Working on Multiple Vectors


Zip two arrays into an array of pairs:

```hs
Prelude Data.Vector> Data.Vector.zip y y
fromList [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9),(10,10),(11,11)] :: Data.Vector.Vector

```



## The Data.Vector Module


The [Data.Vector](https://hackage.haskell.org/package/vector-0.11.0.0/docs/Data-Vector.html) module provided by the [vector](https://hackage.haskell.org/package/vector) is a high performance library for working with arrays.

Once you've imported `Data.Vector`, it's easy to start using a `Vector`:

You can even have a multi-dimensional array:



#### Remarks


> 
<p>It [Data.Vector] has an emphasis on very high performance through loop fusion, whilst
retaining a rich interface. The main data types are boxed and unboxed
arrays, and arrays may be immutable (pure), or mutable. Arrays may
hold Storable elements, suitable for passing to and from C, and you
can convert between the array types. Arrays are indexed by
non-negative Int values.</p>


The Haskell Wiki has [these recommendations](https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial#Array_Types):

> 
In general:
<ul>
- End users should use Data.Vector.Unboxed for most cases
- If you need to store more complex structures, use Data.Vector
- If you need to pass to C, use Data.Vector.Storable
</ul>
For library writers;
<ul>
- Use the generic interface, to ensure your library is maximally flexible: Data.Vector.Generic
</ul>


