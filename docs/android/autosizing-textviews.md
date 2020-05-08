---
metaTitle: "Android - Autosizing TextViews"
description: "Granularity, Preset Sizes"
---

# Autosizing TextViews


A TextView that automatically resizes text to fit perfectly within its bounds.

Android O allows you to instruct a TextView to let the size of the text expand or contract automatically to fill its layout based on the TextView’s characteristics and boundaries.

You can set up the TextView autosizing in either code or XML.

There are two ways to set autosizing TextView: **Granularity** and **Preset Sizes**



## Granularity


**In Java:**

Call the [`setAutoSizeTextTypeUniformWithConfiguration()`](https://developer.android.com/reference/android/widget/TextView.html#setAutoSizeTextTypeUniformWithConfiguration%28int,%20int,%20int,%20int%29) method:

```java
setAutoSizeTextTypeUniformWithConfiguration(int autoSizeMinTextSize, int autoSizeMaxTextSize, int autoSizeStepGranularity, int unit)

```

**In XML:**

Use the `autoSizeMinTextSize`, `autoSizeMaxTextSize`, and `autoSizeStepGranularity` attributes to set the auto-sizing dimensions in the layout XML file:

```java
<TextView android:id=”@+id/autosizing_textview_presetsize” 
        android:layout_width=”wrap_content” 
        android:layout_height=”250dp” 
        android:layout_marginLeft=”0dp” 
        android:layout_marginTop=”0dp” 
        android:autoSizeMaxTextSize=”100sp” 
        android:autoSizeMinTextSize=”12sp” 
        android:autoSizeStepGranularity=”2sp” 
        android:autoSizeText=”uniform” 
        android:text=”Hello World!” 
        android:textSize=”100sp” 
        app:layout_constraintLeft_toLeftOf=”parent” 
        app:layout_constraintTop_toTopOf=”parent” />

```

Check out the [AutosizingTextViews-Demo](https://github.com/1priyank1/AutosizingTextViews-Demo) at GitHub for more details.



## Preset Sizes


**In Java:**

Call the [`setAutoSizeTextTypeUniformWithPresetSizes()`](https://developer.android.com/reference/android/widget/TextView.html#setAutoSizeTextTypeUniformWithPresetSizes%28int%5B%5D,%20int%29) method:

```java
setAutoSizeTextTypeUniformWithPresetSizes(int[] presetSizes, int unit)

```

**In XML:**

Use the `autoSizePresetSizes` attribute in the layout XML file:

```java
<TextView android:id=”@+id/autosizing_textview_presetsize” 
        android:layout_width=”wrap_content” 
        android:layout_height=”250dp” 
        android:layout_marginLeft=”0dp” 
        android:layout_marginTop=”0dp” 
        android:autoSizeText=”uniform” 
        android:autoSizePresetSizes=”@array/autosize_text_sizes” 
        android:text=”Hello World!” 
        android:textSize=”100sp” 
        app:layout_constraintLeft_toLeftOf=”parent” 
        app:layout_constraintTop_toTopOf=”parent” />

```

To access the array as a resource, define the array in the **res/values/arrays.xml** file:

```java
<array name=”autosize_text_sizes”>
    <item>10sp</item>
    <item>12sp</item>
    <item>20sp</item>
    <item>40sp</item>
    <item>100sp</item>
</array>

```

Check out the [AutosizingTextViews-Demo](https://github.com/1priyank1/AutosizingTextViews-Demo) at GitHub for more details.

