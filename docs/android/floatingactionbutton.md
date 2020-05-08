---
metaTitle: "Android - FloatingActionButton"
description: "Show and Hide FloatingActionButton on Swipe, Show and Hide FloatingActionButton on Scroll, How to add the FAB to the layout, Setting behaviour of FloatingActionButton"
---

# FloatingActionButton


Floating action button is used for a special type of promoted action,it animates onto the screen as an expanding piece of material, by default. The icon within it may be animated,also FAB may move differently than other UI elements because of their relative importance.
A floating action button represents the primary action in an application which can simply trigger an action or navigate somewhere.



## Show and Hide FloatingActionButton on Swipe


To show and hide a `FloatingActionButton` with the default animation, just call the methods `show()` and `hide()`.  It's good practice to keep a `FloatingActionButton` in the Activity layout instead of putting it in a Fragment, this allows the default animations to work when showing and hiding.

Here is an example with a `ViewPager`:

- Three Tabs
- Show `FloatingActionButton` for the first and third Tab
- Hide the `FloatingActionButton` on the middle Tab

```java
public class MainActivity extends AppCompatActivity {

    FloatingActionButton fab;
    ViewPager viewPager;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        fab = (FloatingActionButton) findViewById(R.id.fab);
        viewPager = (ViewPager) findViewById(R.id.viewpager);

        // ...... set up ViewPager ............

        viewPager.addOnPageChangeListener(new ViewPager.OnPageChangeListener() {

            @Override
            public void onPageSelected(int position) {
                if (position == 0) {
                    fab.setImageResource(android.R.drawable.ic_dialog_email);
                    fab.show();
                } else if (position == 2) {
                    fab.setImageResource(android.R.drawable.ic_dialog_map);
                    fab.show();
                } else {
                    fab.hide();
                }
            }

            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {}

            @Override
            public void onPageScrollStateChanged(int state) {}
        });

        // Handle the FloatingActionButton click event:
        fab.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                int position = viewPager.getCurrentItem();
                if (position == 0) {
                    openSend();
                } else if (position == 2) {
                    openMap();
                }
            }
        });

    }
}

```

Result:

[<img src="https://i.stack.imgur.com/XrJ3N.gif" alt="enter image description here" />](https://i.stack.imgur.com/XrJ3N.gif)



## Show and Hide FloatingActionButton on Scroll


Starting with the Support Library version 22.2.1, it's possible to show and hide a [FloatingActionButton](https://developer.android.com/reference/android/support/design/widget/FloatingActionButton.html) from scrolling behavior using a [`FloatingActionButton.Behavior`](https://developer.android.com/reference/android/support/design/widget/FloatingActionButton.Behavior.html) sublclass that takes advantage of the [`show()`](https://developer.android.com/reference/android/support/design/widget/FloatingActionButton.html#show()) and [`hide()`](https://developer.android.com/reference/android/support/design/widget/FloatingActionButton.html#hide()) methods.

Note that this only works with a [CoordinatorLayout](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.html) in conjunction with inner Views that support Nested Scrolling, such as [RecyclerView](https://developer.android.com/reference/android/support/v7/widget/RecyclerView.html) and [NestedScrollView](https://developer.android.com/reference/android/support/v4/widget/NestedScrollView.html).

This `ScrollAwareFABBehavior` class comes from the [Android Guides on Codepath](https://github.com/codepath/android_guides/wiki/Floating-Action-Buttons) <sub><sup>(cc-wiki with attribution required)</sup></sub>

```java
public class ScrollAwareFABBehavior extends FloatingActionButton.Behavior {
    public ScrollAwareFABBehavior(Context context, AttributeSet attrs) {
        super();
    }

    @Override
    public boolean onStartNestedScroll(final CoordinatorLayout coordinatorLayout, final FloatingActionButton child,
                                       final View directTargetChild, final View target, final int nestedScrollAxes) {
        // Ensure we react to vertical scrolling
        return nestedScrollAxes == ViewCompat.SCROLL_AXIS_VERTICAL
                || super.onStartNestedScroll(coordinatorLayout, child, directTargetChild, target, nestedScrollAxes);
    }

    @Override
    public void onNestedScroll(final CoordinatorLayout coordinatorLayout, final FloatingActionButton child,
                               final View target, final int dxConsumed, final int dyConsumed,
                               final int dxUnconsumed, final int dyUnconsumed) {
        super.onNestedScroll(coordinatorLayout, child, target, dxConsumed, dyConsumed, dxUnconsumed, dyUnconsumed);
        if (dyConsumed > 0 && child.getVisibility() == View.VISIBLE) {
            // User scrolled down and the FAB is currently visible -> hide the FAB
            child.hide();
        } else if (dyConsumed < 0 && child.getVisibility() != View.VISIBLE) {
            // User scrolled up and the FAB is currently not visible -> show the FAB
            child.show();
        }
    }
}

```

In the FloatingActionButton layout xml, specify the `app:layout_behavior` with the fully-qualified-class-name of `ScrollAwareFABBehavior`:

```java
app:layout_behavior="com.example.app.ScrollAwareFABBehavior"

```

For example with this layout:

```java
<android.support.design.widget.CoordinatorLayout
    android:id="@+id/main_layout"
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    tools:context=".MainActivity">

    <android.support.design.widget.AppBarLayout
        android:id="@+id/appBarLayout"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        app:elevation="6dp">
        <android.support.v7.widget.Toolbar
            android:id="@+id/toolbar"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_alignParentTop="true"
            android:background="?attr/colorPrimary"
            android:minHeight="?attr/actionBarSize"
            android:theme="@style/ThemeOverlay.AppCompat.Dark.ActionBar"
            app:popupTheme="@style/ThemeOverlay.AppCompat.Light"
            app:elevation="0dp"
            app:layout_scrollFlags="scroll|enterAlways"
            />

        <android.support.design.widget.TabLayout
            android:id="@+id/tab_layout"
            app:tabMode="fixed"
            android:layout_below="@+id/toolbar"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:background="?attr/colorPrimary"
            app:elevation="0dp"
            app:tabTextColor="#d3d3d3"
            android:minHeight="?attr/actionBarSize"
            />

    </android.support.design.widget.AppBarLayout>

    <android.support.v4.view.ViewPager
        android:id="@+id/viewpager"
        android:layout_below="@+id/tab_layout"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        app:layout_behavior="@string/appbar_scrolling_view_behavior"
        />

    <android.support.design.widget.FloatingActionButton
        android:id="@+id/fab"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_gravity="bottom|end"
        app:layout_behavior="com.example.app.ScrollAwareFABBehavior"
        android:layout_margin="@dimen/fab_margin"
        android:src="@android:drawable/ic_dialog_email" />

</android.support.design.widget.CoordinatorLayout>

```

Here is the result:

[<img src="https://i.stack.imgur.com/ijLDS.gif" alt="enter image description here" />](https://i.stack.imgur.com/ijLDS.gif)



## How to add the FAB to the layout


To use a FloatingActionButton just add the dependency in the `build.gradle` file as described in the remarks section.

Then add to the layout:

```

<android.support.design.widget.FloatingActionButton
        android:id="@+id/fab"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_gravity="bottom|end"
        android:layout_margin="@dimen/fab_margin"
        android:src="@drawable/my_icon" />

```

An example:

[<img src="https://i.stack.imgur.com/k84iu.png" alt="enter image description here" />](https://i.stack.imgur.com/k84iu.png)

**Color**

The background color of this view defaults to the your theme's colorAccent.

In the above image if the `src` only points to + icon (by default 24x24 dp),to get the **background color** of full circle you can use `app:backgroundTint="@color/your_colour"`

If you wish to change the color in code you can use,

`myFab.setBackgroundTintList(ColorStateList.valueOf(your color in int));`

If you want to change FAB's color in pressed state use

```java
mFab.setRippleColor(your color in int);

```

**Positioning**

It is recommended to place 16dp minimum from the edge on mobile,and 24dp minimum on tablet/desktop.

**Note** : Once you set an src excepting to cover the full area of `FloatingActionButton` make sure you have the right size of that image to get the best result.

Default circle size is 56 x 56dp

[<img src="https://i.stack.imgur.com/M3R1u.png" alt="enter image description here" />](https://i.stack.imgur.com/M3R1u.png)

Mini circle size : 40 x 40dp

If you only want to change only the Interior icon use a 24 x 24dp icon for default size



## Setting behaviour of FloatingActionButton


You can set the behavior of the FAB in XML.

For example:

```java
<android.support.design.widget.FloatingActionButton    
   app:layout_behavior=".MyBehavior" />

```

Or you can set programmatically using:

```java
CoordinatorLayout.LayoutParams p = (CoordinatorLayout.LayoutParams) fab.getLayoutParams();
p.setBehavior(xxxx);
fab.setLayoutParams(p);

```



#### Parameters


|Parameter|Detail
|---|---|---|---|---|---|---|---|---|---
|`android.support.design:elevation`|Elevation value for the FAB. May be a reference to another resource, in the form "@[+][package:]type/name" or a theme attribute in the form "?[package:]type/name".
|`android.support.design:fabSize`|Size for the FAB.
|`android.support.design:rippleColor`|Ripple color for the FAB.
|`android.support.design:useCompatPadding`|Enable compat padding.



#### Remarks


Floating action buttons are used for a special type of promoted action. They are distinguished by a circled icon floating above the UI and have special motion behaviors related to morphing, launching, and the transferring anchor point.

Only one floating action button is recommended per screen to represent the most common action.

Before using the `FloatingActionButton` you must add the design support library dependency in the `build.gradle` file:

```java
dependencies {
    compile 'com.android.support:design:25.1.0'
}

```

### Official Documentation:

[https://developer.android.com/reference/android/support/design/widget/FloatingActionButton.html](https://developer.android.com/reference/android/support/design/widget/FloatingActionButton.html)

### Material Design Specifications:

[https://material.google.com/components/buttons-floating-action-button.html](https://material.google.com/components/buttons-floating-action-button.html)

