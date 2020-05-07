---
metaTitle: "Kotlin - Kotlin Caveats"
description: "Calling a toString() on a nullable type"
---

# Kotlin Caveats




## Calling a toString() on a nullable type


A thing to look out for when using the `toString` method in Kotlin is the handling of null in combination with the `String?`.

For example you want to get text from an `EditText` in Android.

You would have a piece of code like:

```kotlin
// Correct:
val text = view.textField?.text?.toString() ?: ""

```

