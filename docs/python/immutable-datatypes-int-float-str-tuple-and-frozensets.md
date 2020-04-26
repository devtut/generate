# Immutable datatypes(int, float, str, tuple and frozensets)




## Individual characters of strings are not assignable


```
foo = &quot;bar&quot;
foo[0] = &quot;c&quot; # Error 

```

Immutable variable value can not be changed once they are created.



## Tuple's individual members aren't assignable


```
foo = (&quot;bar&quot;, 1, &quot;Hello!&quot;,)
foo[1] = 2 # ERROR!! 

```

Second line would return an error since tuple members once created aren't assignable.
Because of tuple's immutability.<br>



## Frozenset's are immutable and not assignable


```
foo = frozenset([&quot;bar&quot;, 1, &quot;Hello!&quot;])
foo[2] = 7 # ERROR
foo.add(3) # ERROR

```

Second line would return an error since frozenset members once created aren't assignable.
Third line would return error as frozensets do not support functions that can manipulate members.

