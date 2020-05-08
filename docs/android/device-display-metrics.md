---
metaTitle: "Android - Device Display Metrics"
description: "Get the screens pixel dimensions, Get screen density, Formula px to dp, dp to px conversation "
---

# Device Display Metrics



## Get the screens pixel dimensions


To retreive the screens width and height in pixels, we can make use of the [WindowManagers](https://developer.android.com/reference/android/view/WindowManager.html) display metrics.

```java
// Get display metrics
DisplayMetrics metrics = new DisplayMetrics();
context.getWindowManager().getDefaultDisplay().getMetrics(metrics);

```

These [DisplayMetrics ](https://developer.android.com/reference/android/util/DisplayMetrics.html) hold a series of information about the devices screen, like its density or size:

```java
// Get width and height in pixel
Integer heightPixels = metrics.heightPixels;
Integer widthPixels = metrics.widthPixels;

```



## Get screen density


To get the screens density, we also can make use of the [Windowmanagers](https://developer.android.com/reference/android/view/WindowManager.html) [DisplayMetrics](https://developer.android.com/reference/android/util/DisplayMetrics.html). This is a quick example:

```java
// Get density in dpi
DisplayMetrics metrics = new DisplayMetrics();
context.getWindowManager().getDefaultDisplay().getMetrics(metrics);
int densityInDpi =  metrics.densityDpi;

```



## Formula px to dp, dp to px conversation 


**DP to Pixel:**

```java
private int dpToPx(int dp)
{
    return (int) (dp * Resources.getSystem().getDisplayMetrics().density);
}

```

**Pixel to DP:**

```java
private int pxToDp(int px)
{
    return (int) (px / Resources.getSystem().getDisplayMetrics().density);
}

```

