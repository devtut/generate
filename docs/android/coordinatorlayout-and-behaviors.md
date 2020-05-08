---
metaTitle: "Android - CoordinatorLayout and Behaviors"
description: "Creating a simple Behavior, Using the SwipeDismissBehavior, Create dependencies between Views"
---

# CoordinatorLayout and Behaviors


The CoordinatorLayout is a super-powered FrameLayout and goal of this ViewGroup is to coordinate the views that are inside it.

The main appeal of the CoordinatorLayout is its ability to coordinate the animations and transitions of the views within the XML file itself.

CoordinatorLayout is intended for two primary use cases:

:As a top-level application decor or chrome layout

:As a container for a specific interaction with one or more child views



## Creating a simple Behavior


To create a `Behavior` just extend the [`CoordinatorLayout.Behavior`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html) class.

### Extend the CoordinatorLayout.Behavior

Example:

```java
public class MyBehavior<V extends View> extends CoordinatorLayout.Behavior<V> {

      /**
       * Default constructor.
       */
      public MyBehavior() {
      }
    
      /**
       * Default constructor for inflating a MyBehavior from layout.
       *
       * @param context The {@link Context}.
       * @param attrs The {@link AttributeSet}.
       */
      public MyBehavior(Context context, AttributeSet attrs) {
        super(context, attrs);
      }
}

```

This behavior need to be attached to a child View of a `CoordinatorLayout` to be called.

### Attach a Behavior programmatically

```java
MyBehavior myBehavior = new MyBehavior();
CoordinatorLayout.LayoutParams params = (CoordinatorLayout.LayoutParams) view.getLayoutParams();
params.setBehavior(myBehavior);

```

### Attach a Behavior in XML

You can use the `layout_behavior` attribute to attach the behavior in XML:

```java
<View
  android:layout_height="...."
  android:layout_width="...."
  app:layout_behavior=".MyBehavior" />

```

### Attach a Behavior automatically

If you are working with a custom view you can attach the behavior using the `@CoordinatorLayout.DefaultBehavior` annotation:

```java
@CoordinatorLayout.DefaultBehavior(MyBehavior.class)
public class MyView extends ..... {

}

```



## Using the SwipeDismissBehavior


The [`SwipeDismissBehavior`](https://developer.android.com/reference/android/support/design/widget/SwipeDismissBehavior.html) works on any View and implements the functionality of swipe to dismiss in our layouts with a `CoordinatorLayout`.

Just use:

```

       final SwipeDismissBehavior<MyView> swipe = new SwipeDismissBehavior();

        //Sets the swipe direction for this behavior.
        swipe.setSwipeDirection(
            SwipeDismissBehavior.SWIPE_DIRECTION_ANY);

        //Set the listener to be used when a dismiss event occurs
        swipe.setListener(
            new SwipeDismissBehavior.OnDismissListener() {
            @Override public void onDismiss(View view) {
                //......
            }

            @Override 
            public void onDragStateChanged(int state) {
                //......
            }
        });

        //Attach the SwipeDismissBehavior to a view
        LayoutParams coordinatorParams = 
            (LayoutParams) mView.getLayoutParams();    
        coordinatorParams.setBehavior(swipe);

```



## Create dependencies between Views


You can use the [`CoordinatorLayout.Behavior`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html) to create dependencies between views. You can anchor a `View` to another `View` by:

- using the [`layout_anchor`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.LayoutParams.html#getAnchorId()) attribute.
- creating a custom `Behavior` and implementing the [`layoutDependsOn`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html#layoutDependsOn(android.support.design.widget.CoordinatorLayout,%20V,%20android.view.View)) method returning `true`.

For example, in order to create a `Behavior` for moving an `ImageView` when another one is moved (example Toolbar), perform the following steps:

<li>
[Create the custom Behavior](http://stackoverflow.com/documentation/android/5714/coordinatorlayout-and-behaviors/20239/creating-a-simple-behavior#t=201608310823294520346):

```java
public class MyBehavior extends CoordinatorLayout.Behavior<ImageView> {...}

```


</li>
<li>
Override the [`layoutDependsOn`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html#layoutDependsOn(android.support.design.widget.CoordinatorLayout,%20V,%20android.view.View)) method returning `true`. This method is called every time a change occurs to the layout:

```java
@Override
public boolean layoutDependsOn(CoordinatorLayout parent, 
        ImageView child, View dependency) {
    // Returns true to add a dependency.
    return dependency instanceof Toolbar;
}

```


</li>
<li>
Whenever the method [`layoutDependsOn`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html#layoutDependsOn(android.support.design.widget.CoordinatorLayout,%20V,%20android.view.View)) returns `true` the method [`onDependentViewChanged`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html#onDependentViewChanged(android.support.design.widget.CoordinatorLayout,%20V,%20android.view.View)) is called:

```java
@Override
public boolean onDependentViewChanged(CoordinatorLayout parent, ImageView child, View dependency) {
    // Implement here animations, translations, or movements; always related to the provided dependency.
    float translationY = Math.min(0, dependency.getTranslationY() - dependency.getHeight()); 
    child.setTranslationY(translationY);
}

```


</li>



#### Remarks


The [`CoordinatorLayout`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.html) is a container that extends the `FrameLayout`.<br />
By attaching a [`CoordinatorLayout.Behavior`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html) to a direct child of `CoordinatorLayout`, you’ll be able to intercept touch events, window insets, measurement, layout, and nested scrolling.

By specifying [`Behaviors`](https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.Behavior.html) for child views of a `CoordinatorLayout` you can provide many different interactions within a single parent and those views can also interact with one another. View classes can specify a default behavior when used as a child of a `CoordinatorLayout` using the `DefaultBehavior` annotation.

