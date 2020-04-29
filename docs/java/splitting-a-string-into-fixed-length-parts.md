---
metaTitle: "Splitting a string into fixed length parts"
description: "Break a string up into substrings all of a known length, Break a string up into substrings all of variable length"
---

# Splitting a string into fixed length parts



## Break a string up into substrings all of a known length


The trick is to use a look-behind with the regex `\G`, which means "end of previous match":

```java
String[] parts = str.split("(?<=\\G.{8})");

```

The regex matches 8 characters after the end of the last match. Since in this case the match is zero-width, we could more simply say "8 characters after the last match".

Conveniently, `\G` is initialized to start of input, so it works for the first part of the input too.



## Break a string up into substrings all of variable length


Same as the known length example, but insert the length into regex:

```java
int length = 5;
String[] parts = str.split("(?<=\\G.{" + length + "})");

```



#### Remarks


The goal here is to not lose content, so the regex must not consume (match) any input. Rather it must match **between** the last character of the previous target input and the first character of the next target input. eg for 8-character substrings, we need to break the input up (ie match) at the places marked below:

```java
a b c d e f g h i j k l m n o p q r s t u v w x y z
               ^               ^               ^

```

Ignore the spaces in the input which were required to show **between** character positions.

