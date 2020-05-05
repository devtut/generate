---
metaTitle: "Java - The java.util.Objects Class"
description: "Basic use for object null check, Objects.nonNull() method reference use in stream api"
---

# The java.util.Objects Class



## Basic use for object null check


### For null check in method

```java
Object nullableObject = methodReturnObject();
if (Objects.isNull(nullableObject)) {
    return;
}

```

### For not null check in method

```java
Object nullableObject = methodReturnObject();
if (Objects.nonNull(nullableObject)) {
    return;
}

```



## Objects.nonNull() method reference use in stream api


In the old fashion way for collection null check

```java
List<Object> someObjects = methodGetList();
for (Object obj : someObjects) {
    if (obj == null) {
        continue;
    }
    doSomething(obj);
}

```

With the `Objects.nonNull` method and Java8 Stream API, we can do the above in this way:

```java
List<Object> someObjects = methodGetList();
someObjects.stream()
           .filter(Objects::nonNull)
           .forEach(this::doSomething);

```

