---
metaTitle: "Kotlin - Exceptions"
description: "Catching exception with try-catch-finally"
---

# Exceptions



## Catching exception with try-catch-finally


Catching exceptions in Kotlin looks very similar to Java

```kotlin
try {
    doSomething()
} 
catch(e: MyException) {
    handle(e)
} 
finally {
    cleanup()
}

```

You can also catch multiple exceptions

```kotlin
try {
    doSomething()
} 
catch(e: FileSystemException) {
    handle(e)
}
catch(e: NetworkException) {
    handle(e)
}
catch(e: MemoryException) {
    handle(e)
}
finally {
    cleanup()
}    

```

`try` is also an expression and may return value

```kotlin
val s: String? = try { getString() } catch (e: Exception) { null }

```

Kotlin doesn't have checked exceptions, so you don't have to catch any exceptions.

```kotlin
fun fileToString(file: File) : String {
    //readAllBytes throws IOException, but we can omit catching it
    fileContent = Files.readAllBytes(file)
    return String(fileContent)
}

```

