---
metaTitle: "Android - Tools Attributes"
description: "Designtime Layout Attributes"
---

# Tools Attributes



## Designtime Layout Attributes


These attributes are used when the layout is rendered in Android Studio, but have no impact on the runtime.

In general you can use any Android framework attribute, just using the `tools:` namespace rather than the `android:` namespace for layout preview.
You can add both the `android:` namespace attribute (which is used at runtime) and the matching `tools:` attribute (which overrides the runtime attribute in the layout preview only).

Just define the tools namespace as described in the remarks section.

For example the `text` attribute:

```java
<EditText 
      tools:text="My Text"
      android:layout_width="wrap_content"
      android:layout_height="wrap_content" />

```

Or the `visibility` attribute to unset a view for preview:

```java
<LinearLayout
        android:id="@+id/ll1"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        tools:visibility="gone" />

```

Or the `context` attribute to associate the layout with activity or fragment

```java
<LinearLayout
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    tools:context=".MainActivity" >

```

Or the `showIn` attribute to see and included layout preview in another layout

```java
<EditText xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:text="@string/text"
    tools:showIn="@layout/activity_main" />

```



#### Remarks


Android has a dedicated XML namespace intended for tools to be able to record information in XML file.

The namespace URI is :

`http://schemas.android.com/tools` and is usually bound to the `tools:` prefix.

