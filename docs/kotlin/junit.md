---
metaTitle: "Kotlin - JUnit"
description: "Rules"
---

# JUnit



## Rules


To add a JUnit [rule](https://github.com/junit-team/junit4/wiki/rules) to a test fixture:

```kotlin
@Rule @JvmField val myRule = TemporaryFolder()

```

The `@JvmField` annotation is necessary to expose the backing field with the same visibility (public) as the `myRule` property (see [answer](http://stackoverflow.com/questions/32899947/kotlin-junit-rules)). JUnit rules require the annotated rule field to be public.

