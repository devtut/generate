---
metaTitle: "Runtime Commands"
description: "Adding shutdown hooks"
---

# Runtime Commands



## Adding shutdown hooks


Sometimes you need a piece of code to execute when the program stops, such as releasing system resources that you open. You can make a thread run when the program stops with the `addShutdownHook` method:

```java
Runtime.getRuntime().addShutdownHook(new Thread(() -> {
    ImportantStuff.someImportantIOStream.close();
}));

```

