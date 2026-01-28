---
metaTitle: "Java - StringBuffer"
description: "String Buffer class"
---

# StringBuffer


**Introduction to Java StringBuffer class.**



## String Buffer class


**Key Points :-**

&lt;li>
used to created mutable (modifiable) string.
&lt;/li>
&lt;li>
**Mutable** :- Which can be changed.
&lt;/li>
&lt;li>
is thread-safe i.e. multiple threads cannot access it simultaneously.
&lt;/li>

**Methods :-**

&lt;li>
public synchronized StringBuffer append(String s)
&lt;/li>
&lt;li>
public synchronized StringBuffer insert(int offset, String s)
&lt;/li>
&lt;li>
public synchronized StringBuffer replace(int startIndex, int endIndex, String str)
&lt;/li>
&lt;li>
public synchronized StringBuffer delete(int startIndex, int endIndex)
&lt;/li>
&lt;li>
public synchronized StringBuffer reverse()
&lt;/li>
&lt;li>
public int capacity()
&lt;/li>
&lt;li>
public void ensureCapacity(int minimumCapacity)
&lt;/li>
&lt;li>
public char charAt(int index)
&lt;/li>
&lt;li>
public int length()
&lt;/li>
&lt;li>
public String substring(int beginIndex)
&lt;/li>
&lt;li>
public String substring(int beginIndex, int endIndex)
&lt;/li>

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

