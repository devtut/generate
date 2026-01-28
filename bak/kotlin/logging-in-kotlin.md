---
metaTitle: "Kotlin - logging in kotlin"
description: "kotlin.logging"
---

# logging in kotlin




## kotlin.logging


```kotlin
class FooWithLogging {
  companion object: KLogging()

  fun bar() {
    logger.info { "hello $name" }
  }

  fun logException(e: Exception) {
    logger.error(e) { "Error occured" }
  }
}

```

Using [kotlin.logging](https://github.com/MicroUtils/kotlin.logging) framework



#### Remarks


Related question: [Idiomatic way of logging in Kotlin](http://stackoverflow.com/q/34416869/986533)

