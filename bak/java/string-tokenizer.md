---
metaTitle: "Java - String Tokenizer"
description: "StringTokenizer Split by space, StringTokenizer Split by comma ','"
---

# String Tokenizer


The java.util.StringTokenizer class allows you to break a string into tokens. It is simple way to break string.

The set of delimiters (the characters that separate tokens) may be specified either at creation time or on a per-token basis.



## StringTokenizer Split by space


```java
import java.util.StringTokenizer;  
public class Simple{  
 public static void main(String args[]){  
   StringTokenizer st = new StringTokenizer("apple ball cat dog"," ");  
     while (st.hasMoreTokens()) {  
         System.out.println(st.nextToken());  
     }  
   }  
}  

```

**Output:**

apple

ball

cat

dog



## StringTokenizer Split by comma ','


```java
public static void main(String args[]) {
        StringTokenizer st = new StringTokenizer("apple,ball cat,dog", ",");
        while (st.hasMoreTokens()) {
            System.out.println(st.nextToken());
        }
    }

```

**Output:**

apple

ball cat

dog

