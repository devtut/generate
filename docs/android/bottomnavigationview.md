---
metaTitle: "Android - BottomNavigationView"
description: "Basic implemetation, Customization of BottomNavigationView, Handling Enabled / Disabled states, Allowing more than 3 menus"
---

# BottomNavigationView


The Bottom Navigation View has been in the material design [guidelines](https://material.io/guidelines/components/bottom-navigation.html) for some time, but it hasn’t been easy for us to implement it into our apps.

Some applications have built their own solutions, whilst others have relied on third-party open-source libraries to get the job done.

Now the design support library is seeing the addition of this bottom navigation bar, let’s take a dive into how we can use it!



## Basic implemetation


To add the `BottomNavigationView` follow these steps:

1. Add in your `build.gradle` the **dependency**:

```java
compile 'com.android.support:design:25.1.0'

```


1. Add the `BottomNavigationView` in **your layout**:

```java
<android.support.design.widget.BottomNavigationView
        xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        android:id="@+id/bottom_navigation"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        app:menu="@menu/bottom_navigation_menu"/>

```


1. Create the **menu** to populate the view:

```java
<!-- res/menu/bottom_navigation_menu.xml -->

<?xml version="1.0" encoding="utf-8"?>
<menu xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto">
    <item
        android:id="@+id/my_action1"
        android:enabled="true"
        android:icon="@drawable/my_drawable"
        android:title="@string/text"
        app:showAsAction="ifRoom" />
    ....
</menu>

```


1. Attach **a listener** for the click events:

```java
//Get the view
BottomNavigationView bottomNavigationView = (BottomNavigationView)
                findViewById(R.id.bottom_navigation);
//Attach the listener
bottomNavigationView.setOnNavigationItemSelectedListener(
        new BottomNavigationView.OnNavigationItemSelectedListener() {
            @Override
            public boolean onNavigationItemSelected(@NonNull MenuItem item) {
                switch (item.getItemId()) {

                    case R.id.my_action1:
                        //Do something...
                        break;

                    //...
                }
                return true;//returning false disables the Navigation bar animations
            }
        });

```

Checkout demo code at [BottomNavigation-Demo](https://github.com/1priyank1/BottomNavigation-Demo)



## Customization of BottomNavigationView


**Note : I am assuming that you know about how to use **`BottomNavigationView`**.**

This example I will explain how to add selector for **`BottomNavigationView`**. So you can state on UI for icons and texts.

Create drawable `bottom_navigation_view_selector.xml` as

```java
<?xml version="1.0" encoding="utf-8"?>
<selector xmlns:android="http://schemas.android.com/apk/res/android">
    <item  android:color="@color/bottom_nv_menu_selected" android:state_checked="true" />
    <item android:color="@color/bottom_nv_menu_default" />
</selector>

```

And use below attributes into **`BottomNavigationView`** in layout file

```java
app:itemIconTint="@drawable/bottom_navigation_view_selector" 
app:itemTextColor="@drawable/bottom_navigation_view_selector"

```

> 
<p>In above example, I have used same selector
`bottom_navigation_view_selector` for `app:itemIconTint` and
`app:itemTextColor` both to keep text and icon colors same. But if
your design has different color for text and icon, you can define 2
different selectors and use them.</p>


Output will be similar to below

[<img src="https://i.stack.imgur.com/HT7wB.png" alt="Option 1" />](https://i.stack.imgur.com/HT7wB.png)

[<img src="https://i.stack.imgur.com/pScpu.png" alt="Option 2" />](https://i.stack.imgur.com/pScpu.png)



## Handling Enabled / Disabled states


Create Selector for Enable/Disable Menu Item.

**selector.xml**

```java
<?xml version="1.0" encoding="utf-8"?>
<selector xmlns:android="http://schemas.android.com/apk/res/android">
    <item android:color="@color/white" android:state_enabled="true" />
    <item android:color="@color/colorPrimaryDark" android:state_enabled="false" />
</selector>

```

**design.xml**

```java
<android.support.design.widget.BottomNavigationView
    android:id="@+id/bottom_navigation"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:layout_alignParentBottom="true"
    app:itemBackground="@color/colorPrimary"
    app:itemIconTint="@drawable/nav_item_color_state"
    app:itemTextColor="@drawable/nav_item_color_state"
    app:menu="@menu/bottom_navigation_main" />

```



## Allowing more than 3 menus


This example is strictly a workaround since, currently there is no way to disable a behaviour known as ShiftMode.

Create a function as such.

```java
public static void disableMenuShiftMode(BottomNavigationView view) {
    BottomNavigationMenuView menuView = (BottomNavigationMenuView) view.getChildAt(0);
    try {
        Field shiftingMode = menuView.getClass().getDeclaredField("mShiftingMode");
        shiftingMode.setAccessible(true);
        shiftingMode.setBoolean(menuView, false);
        shiftingMode.setAccessible(false);
        for (int i = 0; i < menuView.getChildCount(); i++) {
            BottomNavigationItemView item = (BottomNavigationItemView) menuView.getChildAt(i);
            //noinspection RestrictedApi
            item.setShiftingMode(false);
            // set once again checked value, so view will be updated
            //noinspection RestrictedApi
            item.setChecked(item.getItemData().isChecked());
        }
    } catch (NoSuchFieldException e) {
        Log.e("BNVHelper", "Unable to get shift mode field", e);
    } catch (IllegalAccessException e) {
        Log.e("BNVHelper", "Unable to change value of shift mode", e);
    }
}

```

This disables the Shifting behaviour of the menu when item count exceeds 3 nos.

**USAGE**

```java
BottomNavigationView navView = (BottomNavigationView) findViewById(R.id.bottom_navigation_bar);
disableMenuShiftMode(navView);

```

**Proguard Issue** : Add following line proguard configuration file as well else, this wouldn't work.

```java
-keepclassmembers class android.support.design.internal.BottomNavigationMenuView { 
boolean mShiftingMode; 
}

```

Alternatively, you can create a Class and access this method from there. [See Original Reply Here](http://stackoverflow.com/a/40189977/1149398)

**NOTE** : This is a Reflection based **HOTFIX**, please update this once Google's support library is updated with a direct function call.



#### Remarks


Represents a standard bottom navigation bar for application. It is an implementation of material design bottom navigation.

### Links:

- [Official Javadoc](https://developer.android.com/reference/android/support/design/widget/BottomNavigationView.html)

