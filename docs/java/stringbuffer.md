---
metaTitle: "Java - StringBuffer"
description: "String Buffer class"
---

# StringBuffer


**Introduction to Java StringBuffer class.**



## String Buffer class


**Key Points :-**

<li>
used to created mutable (modifiable) string.
</li>
<li>
**Mutable** :- Which can be changed.
</li>
<li>
is thread-safe i.e. multiple threads cannot access it simultaneously.
</li>

**Methods :-**

<li>
public synchronized StringBuffer append(String s)
</li>
<li>
public synchronized StringBuffer insert(int offset, String s)
</li>
<li>
public synchronized StringBuffer replace(int startIndex, int endIndex, String str)
</li>
<li>
public synchronized StringBuffer delete(int startIndex, int endIndex)
</li>
<li>
public synchronized StringBuffer reverse()
</li>
<li>
public int capacity()
</li>
<li>
public void ensureCapacity(int minimumCapacity)
</li>
<li>
public char charAt(int index)
</li>
<li>
public int length()
</li>
<li>
public String substring(int beginIndex)
</li>
<li>
public String substring(int beginIndex, int endIndex)
</li>

**Example Showing diffrence between String and String Buffer implementation :-**

```java
class Test {
 public static void main(String args[])
 {
  String str = "study";
  str.concat("tonight");
  System.out.println(str);      // Output: study

  StringBuffer strB = new StringBuffer("study");
  strB.append("tonight");
  System.out.println(strB);    // Output: studytonight
 }
}

```

