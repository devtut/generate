---
metaTitle: "Java - List vs SET"
description: "List vs Set"
---

# List vs SET


What are differences between List and Set collection at the top level and How to choose when to use List in java and when to use Set in Java



## List vs Set


```java
import java.util.ArrayList;

```

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class SetAndListExample
{
public static void main( String[] args )
{
System.out.println("List example .....");
List list = new ArrayList();
list.add("1");
list.add("2");
list.add("3");
list.add("4");
list.add("1");

```

   for (String temp : list){
        System.out.println(temp);
    }

    System.out.println("Set example .....");
    Set<String> set = new HashSet<String>();
    set.add("1");
    set.add("2");
    set.add("3");
    set.add("4");
    set.add("1");
    set.add("2");
    set.add("5");

    for (String temp : set){
        System.out.println(temp);
    }
}

```

}

Output
List example .....
1
2
3
4
1
Set example .....
3
2
10
5
4

