---
metaTitle: "Kotlin - Kotlin Android Extensions"
description: "Using Views, Configuration, Painfull listener for getting notice, when the view is completely drawn now is so simple and awesome with Kotlin's extension, Product flavors"
---

# Kotlin Android Extensions


Kotlin has a built-in view injection for Android, allowing to skip manual binding or need for frameworks such as ButterKnife. Some of the advantages are a nicer syntax, better static typing and thus being less error-prone.



## Using Views


Assuming we have an activity with an example layout called `activity_main.xml`:

```kotlin
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <Button
        android:id="@+id/my_button"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text="My button"/>
</LinearLayout>

```

We can use Kotlin extensions to call the button without any additional binding like so:

```kotlin
import kotlinx.android.synthetic.main.activity_main.my_button

class MainActivity: Activity() {
    override fun onCreate(savedInstanceBundle: Bundle?) {
        super.onCreate(savedInstanceBundle)
        setContentView(R.layout.activity_main)
        // my_button is already casted to a proper type of "Button"
        // instead of being a "View"
        my_button.setText("Kotlin rocks!")
    }
}

```

You can also import all ids appearing in layout with a `*` notation

```kotlin
// my_button can be used the same way as before
import kotlinx.android.synthetic.main.activity_main.*

```

Synthetic views can't be used outside of Activities/Fragments/Views with that layout inflated:

```kotlin
import kotlinx.android.synthetic.main.activity_main.my_button

class NotAView {
    init {
        // This sample won't compile!
        my_button.setText("Kotlin rocks!")
    }
}

```



## Configuration


Start with a [properly configured gradle project](http://stackoverflow.com/documentation/kotlin/2501/configuring-kotlin-build/8269/gradle-configuration#t=201703191349559200788).

In your **project-local** (not top-level) `build.gradle` append extensions plugin declaration below your Kotlin plugin, on top-level indentation level.

```kotlin
buildscript {
    ...
}

apply plugin: "com.android.application"
...
apply plugin: "kotlin-android"
apply plugin: "kotlin-android-extensions"
...

```



## Painfull listener for getting notice, when the view is completely drawn now is so simple and awesome with Kotlin's extension


```kotlin
mView.afterMeasured {
  // inside this block the view is completely drawn
  // you can get view's height/width, it.height / it.width
}

```

Under the hood

```kotlin
inline fun View.afterMeasured(crossinline f: View.() -> Unit) {
viewTreeObserver.addOnGlobalLayoutListener(object : ViewTreeObserver.OnGlobalLayoutListener {
    override fun onGlobalLayout() {
        if (measuredHeight > 0 && measuredWidth > 0) {
            viewTreeObserver.removeOnGlobalLayoutListener(this)
            f()
        }
    }
})
}

```



## Product flavors


Android extensions also work with multiple Android Product Flavors. For example if we have flavors in `build.gradle` like so:

```kotlin
android {
    productFlavors {
        paid {
            ...
        }
        free {
            ...
        }
    }
}

```

And for example, only the free flavor has a buy button:

```kotlin
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <Button
        android:id="@+id/buy_button"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text="Buy full version"/>
</LinearLayout>

```

We can bind to the flavor specifically:

```kotlin
import kotlinx.android.synthetic.free.main_activity.buy_button

```

