---
metaTitle: "Android - CardView"
description: "Getting Started with CardView, Adding Ripple animation, Customizing the CardView, Using Images as Background in CardView (Pre-Lollipop device issues), Animate CardView  background color with TransitionDrawable"
---

# CardView


A FrameLayout with a rounded corner background and shadow.

CardView uses elevation property on Lollipop for shadows and falls back to a custom emulated shadow implementation on older platforms.

Due to expensive nature of rounded corner clipping, on platforms before Lollipop, CardView does not clip its children that intersect with rounded corners. Instead, it adds padding to avoid such intersection (See setPreventCornerOverlap(boolean) to change this behavior).



## Getting Started with CardView


`CardView` is a member of the Android Support Library, and provides a layout for cards.

To add `CardView` to your project, add the following line to your `build.gradle` dependencies.

```java
compile 'com.android.support:cardview-v7:25.1.1'

```

**A number of the latest version may be found [here](https://developer.android.com/topic/libraries/support-library/features.html)**

In your layout you can then add the following to get a card.

```java
<android.support.v7.widget.CardView
        xmlns:card_view="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="wrap_content">

        <!-- one child layout containing other layouts or views -->     

</android.support.v7.widget.CardView>

```

You can then add other layouts inside this and they will be encompassed in a card.

Also, CardView can be populated with any UI element and manipulated from [code](https://stackoverflow.com/a/36223222/6013830).

```java
<?xml version="1.0" encoding="utf-8"?>
<android.support.v7.widget.CardView
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:card_view="http://schemas.android.com/apk/res-auto"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:id="@+id/card_view"
    android:layout_margin="5dp"
    card_view:cardBackgroundColor="#81C784"
    card_view:cardCornerRadius="12dp"
    card_view:cardElevation="3dp"
    card_view:contentPadding="4dp" >

    <RelativeLayout
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:padding="16dp" >

        <ImageView
            android:layout_width="100dp"
            android:layout_height="100dp"
            android:id="@+id/item_image"
            android:layout_alignParentLeft="true"
            android:layout_alignParentTop="true"
            android:layout_marginRight="16dp"
            />

        <TextView
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:id="@+id/item_title"
            android:layout_toRightOf="@+id/item_image"
            android:layout_alignParentTop="true"
            android:textSize="30sp"
            />

        <TextView
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:id="@+id/item_detail"
            android:layout_toRightOf="@+id/item_image"
            android:layout_below="@+id/item_title"
            />

    </RelativeLayout>
</android.support.v7.widget.CardView>

```



## Adding Ripple animation


To enable the ripple animation in a CardView, add the following attributes:

```java
<android.support.v7.widget.CardView
  ...
  android:clickable="true"
  android:foreground="?android:attr/selectableItemBackground">
  ...
</android.support.v7.widget.CardView>

```



## Customizing the CardView


CardView provides a default elevation and corner radius so that cards have a consistent appearance across the platforms.

You can customize these default values using these attributes in the xml file:

1. `card_view:cardElevation` attribute add elevation in CardView.
<li>`card_view:cardBackgroundColor` attribute is used to
customize background color of CardView's background(you can give any color).</li>
<li>`card_view:cardCornerRadius` attribute is used to curve 4 edges of
CardView</li>
<li>`card_view:contentPadding` attribute add padding between card and
children of card</li>

Note: card_view is a namespace defined in topmost parent layout view.
xmlns:card_view="[http://schemas.android.com/apk/res-auto](http://schemas.android.com/apk/res-auto)"

Here an example:

```java
<android.support.v7.widget.CardView
        xmlns:card_view="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        card_view:cardElevation="4dp"
        card_view:cardBackgroundColor="@android:color/white"
        card_view:cardCornerRadius="8dp"
        card_view:contentPadding="16dp">

        <!-- one child layout containing other layouts or views -->     

</android.support.v7.widget.CardView>

```

You can also do it programmatically using:

```java
card.setCardBackgroundColor(....);
card.setCardElevation(...);
card.setRadius(....);
card.setContentPadding();

```

Check the [official javadoc](https://developer.android.com/reference/android/support/v7/widget/CardView.html) for additional properties.



## Using Images as Background in CardView (Pre-Lollipop device issues)


While using Image/Colour as an background in a CardView, You might end up with slight white paddings (If default Card colour is white) on the edges. This occurs due to the default rounded corners in the Card View. Here is how to avoid those margins in Pre-lollipop devices.

We need to use an attribute `card_view:cardPreventCornerOverlap="false"` in the CardView.
1). In XML use the following snippet.

```java
<android.support.v7.widget.CardView
    xmlns:card_view="http://schemas.android.com/apk/res-auto"
    android:layout_width="match_parent"
    card_view:cardPreventCornerOverlap="false"
    android:layout_height="wrap_content"> 
      <ImageView
            android:id="@+id/row_wallet_redeem_img"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:adjustViewBounds="true"
            android:scaleType="centerCrop"
            android:src="@drawable/bg_image" />

</android.support.v7.widget.CardView>

```


1. In Java like this `cardView.setPreventCornerOverlap(false)`.

Doing so removes an unwanted padding on the Card's edges.
Here are some visual examples related to this implementation.

**1 Card with image background in API 21** (perfectly fine)
[<img src="https://i.stack.imgur.com/LU77o.png" alt="Card with image background in API 21" />](https://i.stack.imgur.com/LU77o.png)

**2 Card with image background in API 19 without attribute** (notice the paddings around image)
[<img src="https://i.stack.imgur.com/mz69j.png" alt="Card with image background in API 19 without attribute" />](https://i.stack.imgur.com/mz69j.png)

**3 FIXED Card with image background in API 19 with attribute** `cardView.setPreventCornerOverlap(false)` (Issue now fixed)
[<img src="https://i.stack.imgur.com/9Tfry.png" alt="FIXED Card with image background in API 19 with attribute" />](https://i.stack.imgur.com/9Tfry.png)

Also read about this on [Documentation here](https://developer.android.com/reference/android/support/v7/widget/CardView.html#setPreventCornerOverlap(boolean))<br>
Original SOF post [here](http://stackoverflow.com/a/38115371/1149398)



## Animate CardView  background color with TransitionDrawable


```java
public void setCardColorTran(CardView card) {
    ColorDrawable[] color = {new ColorDrawable(Color.BLUE), new ColorDrawable(Color.RED)};
    TransitionDrawable trans = new TransitionDrawable(color);
    if(Build.VERSION.SDK_INT > Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1) {
        card.setBackground(trans);
    } else {
        card.setBackgroundDrawable(trans);
    }
    trans.startTransition(5000);
}

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|cardBackgroundColor|Background color for CardView.
|cardCornerRadius|Corner radius for CardView.
|cardElevation|Elevation for CardView.
|cardMaxElevation|Maximum Elevation for CardView.
|cardPreventCornerOverlap|Add padding to CardView on v20 and before to prevent intersections between the Card content and rounded corners.
|cardUseCompatPadding|Add padding in API v21+ as well to have the same measurements with previous versions. May be a boolean value, such as "true" or "false".
|contentPadding|Inner padding between the edges of the Card and children of the CardView.
|contentPaddingBottom|Inner padding between the bottom edge of the Card and children of the CardView.
|contentPaddingLeft|Inner padding between the left edge of the Card and children of the CardView.
|contentPaddingRight|Elevation for CardView.
|cardElevation|Inner padding between the right edge of the Card and children of the CardView.
|contentPaddingTop|Inner padding between the top edge of the Card and children of the CardView.



#### Remarks


`CardView` uses real elevation and dynamic shadows on Lollipop (API 21) and above. However, before Lollipop `CardView` falls back to a programmatic shadow implementation.

If trying to make an `ImageView` fit within the rounded corners of a `CardView`, you may notice it does not look correct pre-Lollipop (API 21). To fix this you should call `setPreventCornerOverlap(false)` on your `CardView`, or add `app:cardPreventCornerOverlap="false"` to your layout.

Before using the `CardView` you have to add the support library dependency in the `build.gradle` file:

```java
dependencies{
    compile 'com.android.support:cardview-v7:25.2.0'
}

```

**A number of the latest version may be found [here](https://developer.android.com/topic/libraries/support-library/features.html)**

### Official Documentation:

[https://developer.android.com/reference/android/support/v7/widget/CardView.html](https://developer.android.com/reference/android/support/v7/widget/CardView.html)
[https://developer.android.com/training/material/lists-cards.html](https://developer.android.com/training/material/lists-cards.html)

