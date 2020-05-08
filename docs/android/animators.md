---
metaTitle: "Android - Animators"
description: "TransitionDrawable animation, Fade in/out animation, ValueAnimator, Expand and Collapse animation of View, Shake animation of an ImageView, ObjectAnimator, ViewPropertyAnimator"
---

# Animators



## TransitionDrawable animation


This example displays a transaction for an image view with only two images.(can use more images as well one after the other for the first and second layer positions after each transaction as a loop)

- add a image array to `res/values/arrays.xml`

```java
<resources>

    <array
        name="splash_images">
        <item>@drawable/spash_imge_first</item>
        <item>@drawable/spash_img_second</item>
    </array>

</resources>

```

```java
    private Drawable[] backgroundsDrawableArrayForTransition;
    private TransitionDrawable transitionDrawable;

private void backgroundAnimTransAction() {

        // set res image array
        Resources resources = getResources();
        TypedArray icons = resources.obtainTypedArray(R.array.splash_images);

        @SuppressWarnings("ResourceType")
        Drawable drawable = icons.getDrawable(0);    // ending image

        @SuppressWarnings("ResourceType")
        Drawable drawableTwo = icons.getDrawable(1);   // starting image


        backgroundsDrawableArrayForTransition = new Drawable[2];
        backgroundsDrawableArrayForTransition[0] = drawable;
        backgroundsDrawableArrayForTransition[1] = drawableTwo;
        transitionDrawable = new TransitionDrawable(backgroundsDrawableArrayForTransition);

        // your image view here - backgroundImageView
        backgroundImageView.setImageDrawable(transitionDrawable);
        transitionDrawable.startTransition(4000);

        transitionDrawable.setCrossFadeEnabled(false); // call public methods  

      
    }

```



## Fade in/out animation


In order to get a view to slowly fade in or out of view, use an `ObjectAnimator`. As seen in the code below, set a duration using `.setDuration(millis)` where the `millis` parameter is the duration (in milliseconds) of the animation. In the below code, the views will fade in / out over 500 milliseconds, or 1/2 second. To start the `ObjectAnimator`'s animation, call `.start()`. Once the animation is complete, `onAnimationEnd(Animator animation)` is called. Here is a good place to set your view's visibility to `View.GONE` or `View.VISIBLE`.

```java
import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;

void fadeOutAnimation(View viewToFadeOut) {
    ObjectAnimator fadeOut = ObjectAnimator.ofFloat(viewToFadeOut, "alpha", 1f, 0f);

    fadeOut.setDuration(500);
    fadeOut.addListener(new AnimatorListenerAdapter() {
        @Override
        public void onAnimationEnd(Animator animation) {
            // We wanna set the view to GONE, after it's fade out. so it actually disappear from the layout & don't take up space.
            viewToFadeOut.setVisibility(View.GONE);
        }
    });

    fadeOut.start();
}

void fadeInAnimation(View viewToFadeIn) {
    ObjectAnimator fadeIn = ObjectAnimator.ofFloat(viewToFadeIn, "alpha", 0f, 1f);
    fadeIn.setDuration(500);

    fadeIn.addListener(new AnimatorListenerAdapter() {
        @Override
        public void onAnimationStar(Animator animation) {
            // We wanna set the view to VISIBLE, but with alpha 0. So it appear invisible in the layout.
            viewToFadeIn.setVisibility(View.VISIBLE);
            viewToFadeIn.setAlpha(0);
        }
    });

    fadeIn.start();
}

```



## ValueAnimator


`ValueAnimator` introduces a simple way to animate a value (of a particular type, e.g. `int`, `float`, etc.).

The usual way of using it is:

1. Create a `ValueAnimator` that will animate a value from `min` to `max`
1. Add an [`UpdateListener`](https://developer.android.com/reference/android/animation/ValueAnimator.AnimatorUpdateListener.html) in which you will use the calculated animated value (which you can obtain with [`getAnimatedValue()`](https://developer.android.com/reference/android/animation/ValueAnimator.html#getAnimatedValue()))

There are two ways you can create the `ValueAnimator`:

**(the example code animates a `float` from `20f` to `40f` in `250ms`)**

1. From `xml` (put it in the `/res/animator/`):

```java
<animator xmlns:android="http://schemas.android.com/apk/res/android"
    android:duration="250"
    android:valueFrom="20"
    android:valueTo="40"
    android:valueType="floatType"/>

```

```java
ValueAnimator animator = (ValueAnimator) AnimatorInflater.loadAnimator(context, 
        R.animator.example_animator);
animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
    @Override
    public void onAnimationUpdate(ValueAnimator anim) {
        // ... use the anim.getAnimatedValue()
    }
});
// set all the other animation-related stuff you want (interpolator etc.)
animator.start();

```


1. From the code:

```java
ValueAnimator animator = ValueAnimator.ofFloat(20f, 40f);
animator.setDuration(250);
animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
    @Override
    public void onAnimationUpdate(ValueAnimator anim) {
        // use the anim.getAnimatedValue()
    }
});
// set all the other animation-related stuff you want (interpolator etc.)
animator.start();

```



## Expand and Collapse animation of View


```java
public class ViewAnimationUtils {
        
        public static void expand(final View v) {
            v.measure(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
            final int targtetHeight = v.getMeasuredHeight();
    
            v.getLayoutParams().height = 0;
            v.setVisibility(View.VISIBLE);
            Animation a = new Animation()
            {
                @Override
                protected void applyTransformation(float interpolatedTime, Transformation t) {
                    v.getLayoutParams().height = interpolatedTime == 1
                            ? LayoutParams.WRAP_CONTENT
                            : (int)(targtetHeight * interpolatedTime);
                    v.requestLayout();
                }
    
                @Override
                public boolean willChangeBounds() {
                    return true;
                }
            };
    
            a.setDuration((int)(targtetHeight / v.getContext().getResources().getDisplayMetrics().density));
            v.startAnimation(a);
        }
    
        public static void collapse(final View v) {
            final int initialHeight = v.getMeasuredHeight();
    
            Animation a = new Animation()
            {
                @Override
                protected void applyTransformation(float interpolatedTime, Transformation t) {
                    if(interpolatedTime == 1){
                        v.setVisibility(View.GONE);
                    }else{
                        v.getLayoutParams().height = initialHeight - (int)(initialHeight * interpolatedTime);
                        v.requestLayout();
                    }
                }
    
                @Override
                public boolean willChangeBounds() {
                    return true;
                }
            };
    
            a.setDuration((int)(initialHeight / v.getContext().getResources().getDisplayMetrics().density));
            v.startAnimation(a);
        }
    }

```



## Shake animation of an ImageView


Under res folder, create a new folder called "anim" to store your animation resources and put this on that folder.

shakeanimation.xml

```java
<?xml version="1.0" encoding="utf-8"?>
<rotate xmlns:android="http://schemas.android.com/apk/res/android"
    android:duration="100"
    android:fromDegrees="-15"
    android:pivotX="50%"
    android:pivotY="50%"
    android:repeatCount="infinite"
    android:repeatMode="reverse"
    android:toDegrees="15" />

```

Create a blank activity called Landing

activity_landing.xml

```

 <RelativeLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <ImageView
            android:id="@+id/imgBell"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_centerInParent="true"
            android:src="@mipmap/ic_notifications_white_48dp"/>

    </RelativeLayout>

```

And the method for animate the imageview on Landing.java

```java
Context mContext;


 @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            mContext=this;
                setContentView(R.layout.activity_landing);

                AnimateBell();
        }

public void AnimateBell() {
        Animation shake = AnimationUtils.loadAnimation(mContext, R.anim.shakeanimation);
        ImageView imgBell= (ImageView) findViewById(R.id.imgBell);
        imgBell.setImageResource(R.mipmap.ic_notifications_active_white_48dp);
        imgBell.setAnimation(shake);
    }

```



## ObjectAnimator


`ObjectAnimator` is a subclass of `ValueAnimator` with the added ability to set the calculated value to the property of a `target` `View`.

Just like in the `ValueAnimator`, there are two ways you can create the `ObjectAnimator`:

(the example code animates an `alpha` of a `View` from `0.4f` to `0.2f` in `250ms`)

1. From `xml` (put it in the `/res/animator`)

```java
<objectAnimator xmlns:android="http://schemas.android.com/apk/res/android"
                android:duration="250"
                android:propertyName="alpha"
                android:valueFrom="0.4"
                android:valueTo="0.2"
                android:valueType="floatType"/>

```

```java
ObjectAnimator animator = (ObjectAnimator) AnimatorInflater.loadAnimator(context,
        R.animator.example_animator);
animator.setTarget(exampleView);
// set all the animation-related stuff you want (interpolator etc.)
animator.start();

```


1. From code:

```java
ObjectAnimator animator = ObjectAnimator.ofFloat(exampleView, View.ALPHA, 0.4f, 0.2f);
animator.setDuration(250);
// set all the animation-related stuff you want (interpolator etc.)
animator.start();

```



## ViewPropertyAnimator


[`ViewPropertyAnimator`](https://developer.android.com/reference/android/view/ViewPropertyAnimator.html) is a simplified and optimized way to animate properties of a `View`.

Every single `View` has a `ViewPropertyAnimator` object available through the [`animate()`](https://developer.android.com/reference/android/view/View.html#animate()) method. You can use that to animate multiple properties at once with a simple call. Every single method of a `ViewPropertyAnimator` specifies the **target** value of a specific parameter that the `ViewPropertyAnimator` should animate to.

```java
View exampleView = ...;
exampleView.animate()
        .alpha(0.6f)
        .translationY(200)
        .translationXBy(10)
        .scaleX(1.5f)
        .setDuration(250)
        .setInterpolator(new FastOutLinearInInterpolator());

```

**Note:** Calling `start()` on a `ViewPropertyAnimator` object is **NOT** mandatory. If you don't do that you're just letting the platform to handle the starting of the animation in the appropriate time (next animation handling pass). If you actually do that (call `start()`) you're making sure the animation is started immediately.

