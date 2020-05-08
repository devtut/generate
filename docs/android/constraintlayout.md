---
metaTitle: "Android - ConstraintLayout"
description: "Adding ConstraintLayout to your project, Chains"
---

# ConstraintLayout


`ConstraintLayout` is a `ViewGroup` which allows you to position and size widgets in a flexible way. It is compatible with Android 2.3 (API level 9) and higher.

It allows you to create large and complex layouts with a flat view hierarchy. It is similar to `RelativeLayout` in that all views are laid out according to relationships between sibling views and the parent layout, but it's more flexible than `RelativeLayout` and easier to use with Android Studio's Layout Editor.



## Adding ConstraintLayout to your project


To work with ConstraintLayout, you need Android Studio Version 2.2 or newer and have at least version 32 (or higher) of Android Support Repository.

1. Add the Constraint Layout library as a dependency in your `build.gradle` file:

```java
dependencies {
   compile 'com.android.support.constraint:constraint-layout:1.0.2'
}

```


1. Sync project

To add a new constraint layout to your project:

1. **Right-click** on your module's layout directory, then click `New > XML > Layout XML.`
1. Enter a **name** for the layout and enter `"android.support.constraint.ConstraintLayout"` for the Root Tag.
1. Click **Finish**.

Otherwise just add in a layout file:

```java
<?xml version="1.0" encoding="utf-8"?>
<android.support.constraint.ConstraintLayout
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

</android.support.constraint.ConstraintLayout>

```



## Chains


Since `ConstraintLayout` alpha 9, **Chains** are available. A **Chain** is a set of views inside a `ConstraintLayout` that are connected in a bi-directional way between them, i.e **A** connected to **B** with a constraint, and **B** connected to **A** with another constraint.

Example:

```java
<android.support.constraint.ConstraintLayout
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <!-- this view is linked to the bottomTextView --> 
    <TextView
        android:id="@+id/topTextView"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text="TextView"
        app:layout_constraintBottom_toTopOf="@+id/bottomTextView"
        app:layout_constraintTop_toTopOf="parent"
        app:layout_constraintVertical_chainPacked="true"/>

    <!-- this view is linked to the topTextView at the same time --> 
    <TextView
        android:id="@+id/bottomTextView"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text="Bottom\nMkay"
        app:layout_constraintBottom_toBottomOf="parent"
        app:layout_constraintTop_toBottomOf="@+id/topTextView"/>

</android.support.constraint.ConstraintLayout>

```

In this example, the two views are positioned one under another and both of them are centered vertically. You may change the vertical position of these views by adjusting the chain's **bias**. Add the following code to the first element of a chain:

```java
app:layout_constraintVertical_bias="0.2"

```

In a vertical chain, the first element is a top-most view, and in a horizontal chain it is the left-most view. The first element defines the whole chain's behavior.

Chains are a new feature and are updated frequently. [Here](https://developer.android.com/reference/android/support/constraint/ConstraintLayout.html) is an official Android Documentation on Chains.



#### Syntax


<li>
**ConstraintLayout**
<ul>
<li>
public void addView(View child, int index, ViewGroup.LayoutParams params)
</li>
<li>
public ConstraintLayout.LayoutParams generateLayoutParams(AttributeSet attrs)
</li>
<li>
public void onViewAdded(View view)
</li>
<li>
public void onViewRemoved(View view)
</li>
<li>
public void removeView(View view)
</li>
<li>
public void requestLayout()
</li>
<li>
protected boolean checkLayoutParams(ViewGroup.LayoutParams params)
</li>
<li>
protected ConstraintLayout.LayoutParams generateDefaultLayoutParams()
</li>
<li>
protected ViewGroup.LayoutParams generateLayoutParams(ViewGroup.LayoutParams params)
</li>
<li>
protected void onLayout(boolean changed, int left, int top, int right, int bottom)
</li>
<li>
protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
</li>

**ConstraintLayout.LayoutParams**

<li>
public void resolveLayoutDirection(int layoutDirection)
</li>
<li>
public void validate()
</li>
<li>
protected void setBaseAttributes(TypedArray a, int widthAttr, int heightAttr)
</li>



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|child|The `View` to be added to the layout
|index|The index of the `View` in the layout hierarchy
|params|The `LayoutParams` of the `View`
|attrs|The `AttributeSet` that defines the `LayoutParams`
|view|The `View` that has been added or removed
|changed|Indicates if this `View` has changed size or position
|left|The left position, relative to the parent `View`
|top|The top position, relative to the parent `View`
|right|The right position, relative to the parent `View`
|bottom|The bottom position, relative to the parent `View`
|widthMeasureSpec|The horizontal space requirements imposed by the parent `View`
|heightMeasureSpec|The vertical space requirements imposed by the parent `View`
|layoutDirection|-
|a|-
|widthAttr|-
|heightAttr|-



#### Remarks


At Google IO 2016 Google announced a new Android layout named ConstraintLayout.<br />
Pay attention because currently, this layout is a **Beta release**.

### Fore More About Constraint Layout:

**[https://codelabs.developers.google.com/codelabs/constraint-layout/index.html](https://codelabs.developers.google.com/codelabs/constraint-layout/index.html)**

