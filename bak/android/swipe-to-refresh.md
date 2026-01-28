---
metaTitle: "Android - Swipe to Refresh"
description: "How to add Swipe-to-Refresh To your app, Swipe To Refresh with RecyclerView"
---

# Swipe to Refresh



## How to add Swipe-to-Refresh To your app


Make sure the following dependency is added to your app's `build.gradle` file under dependencies:

```java
compile 'com.android.support:support-core-ui:24.2.0'

```

Then add the `SwipeRefreshLayout` in your layout:

```java
<android.support.v4.widget.SwipeRefreshLayout
        android:id="@+id/swipe_refresh_layout"
        android:layout_width="match_parent"
        android:layout_height="wrap_content">

     <!-- place your view here -->       

</android.support.v4.widget.SwipeRefreshLayout>

```

Finally implement the `SwipeRefreshLayout.OnRefreshListener` listener.

```java
mSwipeRefreshLayout = (SwipeRefreshLayout) findViewById(R.id.swipe_refresh_layout);
mSwipeRefreshLayout.setOnRefreshListener(new OnRefreshListener() {
    @Override
    public void onRefresh() {
         // your code
    }
});

```



## Swipe To Refresh with RecyclerView


To add a **Swipe To Refresh** layout with a **RecyclerView** add the following to your Activity/Fragment layout file:

```java
<android.support.v4.widget.SwipeRefreshLayout
    android:id="@+id/refresh_layout"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    app:layout_behavior="@string/appbar_scrolling_view_behavior">

    <android.support.v7.widget.RecyclerView
        android:id="@+id/recycler_view"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:orientation="vertical"
        android:scrollbars="vertical" />

</android.support.v4.widget.SwipeRefreshLayout>

```

In your Activity/Fragment add the following to initialize the **SwipeToRefreshLayout**:

```

   SwipeRefreshLayout mSwipeRefreshLayout = (SwipeRefreshLayout) findViewById(R.id.refresh_layout);
    mSwipeRefreshLayout.setColorSchemeResources(R.color.green_bg,
            android.R.color.holo_green_light,
            android.R.color.holo_orange_light,
            android.R.color.holo_red_light);

    mSwipeRefreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
        @Override
        public void onRefresh() {
            // Execute code when refresh layout swiped
        }
    });

```



#### Syntax


1. **setColorSchemeResources** sets the colors of the SwipeToRefreshLayout **indicator**
1. **setOnRefreshListener** sets what to do when layout is swiped
1. **app:layout_behavior="@string/appbar_scrolling_view_behavior"** if you have a Toolbar with your layout, add this with the scrollFlags in Toolbar and the Toolbar will slide up while scrolling down and slide in again while scrolling up.

