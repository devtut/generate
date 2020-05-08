---
metaTitle: "Android - Creating Custom Views"
description: "Creating Custom Views, Adding attributes to views, CustomView performance tips, Creating a compound view, Compound view for SVG/VectorDrawable as drawableRight, Responding to Touch Events"
---

# Creating Custom Views




## Creating Custom Views


If you need a completely customized view,  you'll need to subclass `View` (the superclass of all Android views) and provide your custom sizing (`onMeasure(...)`) and drawing (`onDraw(...)`) methods:

<li>
**Create your custom view skeleton:** this is basically the same for every custom view. Here we create the skeleton for a custom view that can draw a smiley, called `SmileyView`:

```java
public class SmileyView extends View {
    private Paint mCirclePaint;
    private Paint mEyeAndMouthPaint;

    private float mCenterX;
    private float mCenterY;
    private float mRadius;
    private RectF mArcBounds = new RectF();

    public SmileyView(Context context) {
        this(context, null, 0);
    }

    public SmileyView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SmileyView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initPaints();
    }

    private void initPaints() {/* ... */}

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {/* ... */}

    @Override
    protected void onDraw(Canvas canvas) {/* ... */}
}

```


</li>
<li>
**Initialize your paints:** the `Paint` objects are the brushes of your virtual canvas defining how your geometric objects are rendered (e.g. color, fill and stroke style, etc.). Here we create two `Paint`s, one yellow filled paint for the circle and one black stroke paint for the eyes and the mouth:

```java
private void initPaints() {
    mCirclePaint = new Paint(Paint.ANTI_ALIAS_FLAG);
    mCirclePaint.setStyle(Paint.Style.FILL);
    mCirclePaint.setColor(Color.YELLOW);
    mEyeAndMouthPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
    mEyeAndMouthPaint.setStyle(Paint.Style.STROKE);
    mEyeAndMouthPaint.setStrokeWidth(16 * getResources().getDisplayMetrics().density);
    mEyeAndMouthPaint.setStrokeCap(Paint.Cap.ROUND);
    mEyeAndMouthPaint.setColor(Color.BLACK);
}

```


</li>
<li>
**Implement your own `onMeasure(...)` method:** this is required so that the parent layouts (e.g. `FrameLayout`) can properly align your custom view. It provides a set of `measureSpecs` that you can use to determine your view's height and width. Here we create a square by making sure that the height and width are the same:

```java
@Override
protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
    int w = MeasureSpec.getSize(widthMeasureSpec);
    int h = MeasureSpec.getSize(heightMeasureSpec);

    int size = Math.min(w, h);
    setMeasuredDimension(size, size);
}

```


Note that `onMeasure(...)` must contain at least one call to `setMeasuredDimension(..)` or else your custom view will crash with an `IllegalStateException`.
</li>
<li>
**Implement your own `onSizeChanged(...)` method:** this allows you to catch the current height and width of your custom view to properly adjust your rendering code. Here we just calculate our center and our radius:

```java
@Override
protected void onSizeChanged(int w, int h, int oldw, int oldh) {
    mCenterX = w / 2f;
    mCenterY = h / 2f;
    mRadius = Math.min(w, h) / 2f;
}

```


</li>
<li>
**Implement your own `onDraw(...)` method:** this is where you implement the actual rendering of your view. It provides a `Canvas` object that you can draw on (see the official [`Canvas`](https://developer.android.com/reference/android/graphics/Canvas.html) documentation for all drawing methods available).

```java
@Override
protected void onDraw(Canvas canvas) {
    // draw face
    canvas.drawCircle(mCenterX, mCenterY, mRadius, mCirclePaint);
    // draw eyes
    float eyeRadius = mRadius / 5f;
    float eyeOffsetX = mRadius / 3f;
    float eyeOffsetY = mRadius / 3f;
    canvas.drawCircle(mCenterX - eyeOffsetX, mCenterY - eyeOffsetY, eyeRadius, mEyeAndMouthPaint);
    canvas.drawCircle(mCenterX + eyeOffsetX, mCenterY - eyeOffsetY, eyeRadius, mEyeAndMouthPaint);
    // draw mouth
    float mouthInset = mRadius /3f;
    mArcBounds.set(mouthInset, mouthInset, mRadius * 2 - mouthInset, mRadius * 2 - mouthInset);
    canvas.drawArc(mArcBounds, 45f, 90f, false, mEyeAndMouthPaint);
}

```


</li>
<li>
**Add your custom view to a layout:** the custom view can now be included in any layout files that you have. Here we just wrap it inside a `FrameLayout`:

```java
<FrameLayout
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <com.example.app.SmileyView
        android:layout_width="match_parent"
        android:layout_height="match_parent" />
</FrameLayout>

```


</li>

Note that it is recommended to build your project after the view code is finished. Without building it you won't be able to see the view on a preview screen in Android Studio.

After putting everything together, you should be greeted with the following screen after launching the activity containing the above layout:

[<img src="https://i.stack.imgur.com/0yhjv.jpg" alt="SmileyView used inside activity" />](https://i.stack.imgur.com/0yhjv.jpg)



## Adding attributes to views


Custom views can also take custom attributes which can be used in Android layout resource files. To add attributes to your custom view you need to do the following:

<li>
**Define the name and type of your attributes:** this is done inside `res/values/attrs.xml` (create it if necessary). The following file defines a color attribute for our smiley's face color and an enum attribute for the smiley's expression:

```java
<resources>
    <declare-styleable name="SmileyView">
        <attr name="smileyColor" format="color" />
        <attr name="smileyExpression" format="enum">
            <enum name="happy" value="0"/>
            <enum name="sad" value="1"/>
        </attr>
    </declare-styleable>
    <!-- attributes for other views -->
</resources>

```


</li>
<li>
**Use your attributes inside your layout:** this can be done inside any layout files that use your custom view. The following layout file creates a screen with a happy yellow smiley:

```java
<FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    android:layout_height="match_parent"
    android:layout_width="match_parent">
    
    <com.example.app.SmileyView
        android:layout_height="56dp"
        android:layout_width="56dp"
        app:smileyColor="#ffff00"
        app:smileyExpression="happy" />
</FrameLayout>

```


Tip: Custom attributes do not work with the `tools:` prefix in Android Studio 2.1 and older (and possibly in future versions). In this example, replacing `app:smileyColor` with `tools:smileyColor` would result in `smileyColor` neither being set during runtime nor at design time.
</li>
<li>
**Read your attributes:** this is done inside your custom view source code. The following snippet of `SmileyView` demonstrates how the attributes can be extracted:

```java
public class SmileyView extends View {
    // ...

    public SmileyView(Context context) {
        this(context, null);
    }

    public SmileyView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SmileyView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        
        TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.SmileyView, defStyleAttr, 0);
        mFaceColor = a.getColor(R.styleable.SmileyView_smileyColor, Color.TRANSPARENT);
        mFaceExpression = a.getInteger(R.styleable.SmileyView_smileyExpression, Expression.HAPPY);
        // Important: always recycle the TypedArray
        a.recycle();

        // initPaints(); ...
    }
}

```


</li>
<li>
**(Optional) Add default style:** this is done by adding a style with the default values and loading it inside your custom view. The following default smiley style represents a happy yellow one:

```java
<!-- styles.xml -->
<style name="DefaultSmileyStyle">
    <item name="smileyColor">#ffff00</item>
    <item name="smileyExpression">happy</item>
</style>

```


Which gets applied in our `SmileyView` by adding it as the last parameter of the call to `obtainStyledAttributes` (see code in step 3):

```java
TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.SmileyView, defStyleAttr, R.style.DefaultSmileyViewStyle);

```


Note that any attribute values set in the inflated layout file (see code in step 2) will override the corresponding values of the default style.
</li>
<li>
**(Optional) Provide styles inside themes:** this is done by adding a new style reference attribute which can be used inside your themes and providing a style for that attribute. Here we simply name our reference attribute `smileyStyle`:

```java
<!-- attrs.xml -->
<attr name="smileyStyle" format="reference" />

```


Which we then provide a style for in our app theme (here we just reuse the default style from step 4):

```java
<!-- themes.xml -->
<style name="AppTheme" parent="AppBaseTheme">
    <item name="smileyStyle">@style/DefaultSmileyStyle</item>
</style>

```


</li>



## CustomView performance tips


Do not allocate new objects in **onDraw**

```java
@Override
protected void onDraw(Canvas canvas) {
    super.onDraw(canvas);
    Paint paint = new Paint(); //Do not allocate here
}

```

Instead of drawing drawables in canvas...

```java
drawable.setBounds(boundsRect);

drawable.draw(canvas);

```

Use a Bitmap for faster drawing:

```java
canvas.drawBitmap(bitmap, srcRect, boundsRect, paint);

```

Do not redraw the entire view to update just a small part of it. Instead redraw the specific part of view.

```java
invalidate(boundToBeRefreshed);

```

If your view is doing some continuous animation, for instance a watch-face showing each and every second, at least stop the animation at `onStop()` of the activity and start it back on `onStart()` of the activity.

Do not do any calculations inside the `onDraw` method of a view, you should instead finish drawing before calling `invalidate()`. By using this technique you can avoid frame dropping in your view.

**Rotations**

The basic operations of a view are translate, rotate, etc... Almost every developer has faced this problem when they use bitmap or gradients in their custom view. If the view is going to show a rotated view and the bitmap has to be rotated in that custom view, many of us will think that it will be expensive. Many think that rotating a bitmap is very expensive because in order to do that, you need to translate the bitmap's pixel matrix. But the truth is that it is not that tough! Instead of rotating the bitmap, just rotate the canvas itself!

```java
// Save the canvas state
int save = canvas.save();
// Rotate the canvas by providing the  center point as pivot and angle
canvas.rotate(pivotX, pivotY, angle);
// Draw whatever you want
// Basically whatever you draw here will be drawn as per the angle you rotated the canvas
canvas.drawBitmap(...);
// Now restore your your canvas to its original state
canvas.restore(save);
// Unless canvas is restored to its original state, further draw will also be rotated.

```



## Creating a compound view


A **compound view** is a custom `ViewGroup` that's treated as a single view by the surrounding program code. Such a ViewGroup can be really useful in [DDD](https://en.wikipedia.org/wiki/Domain-driven_design)-like design, because it can correspond to an aggregate, in this example, a Contact. It can be reused everywhere that contact is displayed.

This means that the surrounding controller code, an Activity, Fragment or Adapter, can simply pass the data object to the view without picking it apart into a number of different UI widgets.

This facilitates code reuse and makes for a better design according to [SOLID priciples](https://en.wikipedia.org/wiki/SOLID_(object-oriented_design)).

**The layout XML**

This is usually where you start. You have an existing bit of XML that you find yourself reusing, perhaps as an `<include/>`. Extract it into a separate XML file and wrap the root tag in a `<merge>` element:

```java
<?xml version="1.0" encoding="utf-8"?>
<merge xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

        <ImageView
            android:id="@+id/photo"
            android:layout_width="48dp"
            android:layout_height="48dp"
            android:layout_alignParentRight="true" />

        <TextView
            android:id="@+id/name"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_toLeftOf="@id/photo" />

        <TextView
            android:id="@+id/phone_number"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_below="@id/name"
            android:layout_toLeftOf="@id/photo" />
</merge>

```

This XML file keeps working in the Layout Editor in Android Studio perfectly fine. You can treat it like any other layout.

**The compound ViewGroup**

Once you have the XML file, create the custom view group.

```java
import android.annotation.TargetApi;
import android.content.Context;
import android.os.Build;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.ImageView;
import android.widget.TextView;

import myapp.R;

/**
 * A compound view to show contacts.
 *
 * This class can be put into an XML layout or instantiated programmatically, it
 * will work correctly either way.
 */
public class ContactView extends RelativeLayout {

    // This class extends RelativeLayout because that comes with an automatic
    // (MATCH_PARENT, MATCH_PARENT) layout for its child item. You can extend
    // the raw android.view.ViewGroup class if you want more control. See the
    // note in the layout XML why you wouldn't want to extend a complex view
    // such as RelativeLayout.

    // 1. Implement superclass constructors.
    public ContactView(Context context) {
        super(context);
        init(context, null);
    }

    // two extra constructors left out to keep the example shorter

    @TargetApi(Build.VERSION_CODES.LOLLIPOP)
    public ContactView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init(context, attrs);
    }

    // 2. Initialize the view by inflating an XML using `this` as parent
    private TextView mName;
    private TextView mPhoneNumber;
    private ImageView mPhoto;

    private void init(Context context, AttributeSet attrs) {
        LayoutInflater.from(context).inflate(R.layout.contact_view, this, true);
        mName = (TextView) findViewById(R.id.name);
        mPhoneNumber = (TextView) findViewById(R.id.phone_number);
        mPhoto = (ImageView) findViewById(R.id.photo);
    }

    // 3. Define a setter that's expressed in your domain model. This is what the example is
    //    all about. All controller code can just invoke this setter instead of fiddling with
    //    lots of strings, visibility options, colors, animations, etc. If you don't use a
    //    custom view, this code will usually end up in a static helper method (bad) or copies 
    //    of this code will be copy-pasted all over the place (worse).
    public void setContact(Contact contact) {
        mName.setText(contact.getName());
        mPhoneNumber.setText(contact.getPhoneNumber());
        if (contact.hasPhoto()) {
            mPhoto.setVisibility(View.VISIBLE);
            mPhoto.setImageBitmap(contact.getPhoto());
        } else {
            mPhoto.setVisibility(View.GONE);
        }
    }
}

```

The `init(Context, AttributeSet)` method is where you would read any custom XML attributes as explained in [Adding Attributes to Views](https://stackoverflow.com/documentation/android/1446/creating-custom-views/4726/adding-attributes-to-views#t=201607212055553085582).

With these pieces in place, you can use it in your app.

**Usage in XML**

Here's an example `fragment_contact_info.xml` that illustrates how you'd put a single ContactView on top of a list of messages:

```java
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:orientation="vertical">

    <!-- The compound view becomes like any other view XML element -->
    <myapp.ContactView
        android:id="@+id/contact"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"/>

    <android.support.v7.widget.RecyclerView
        android:id="@+id/message_list"
        android:layout_width="match_parent"
        android:layout_height="0dp"
        android:layout_weight="1"/>

</LinearLayout>

```

**Usage in Code**

Here's an example `RecyclerView.Adapter` that shows a list of contacts. This example illustrates just how much cleaner the controller code gets when it's completely free of View manipulation.

```java
package myapp;

import android.content.Context;
import android.support.v7.widget.RecyclerView;
import android.view.ViewGroup;

public class ContactsAdapter extends RecyclerView.Adapter<ContactsViewHolder> {

    private final Context context;

    public ContactsAdapter(final Context context) {
        this.context = context;
    }

    @Override
    public ContactsViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        ContactView v = new ContactView(context); // <--- this
        return new ContactsViewHolder(v);
    }

    @Override
    public void onBindViewHolder(ContactsViewHolder holder, int position) {
        Contact contact = this.getItem(position);
        holder.setContact(contact);  // <--- this
    }

    static class ContactsViewHolder extends RecyclerView.ViewHolder {

        public ContactsViewHolder(ContactView itemView) {
            super(itemView);
        }

        public void setContact(Contact contact) {
            ((ContactView) itemView).setContact(contact); // <--- this
        }
    }
}

```



## Compound view for SVG/VectorDrawable as drawableRight


Main motive to develop this compound view is, below 5.0 devices does not support svg in drawable inside TextView/EditText. One more pros is, we can set `height` and `width` of `drawableRight` inside `EditText`.
I have separated it from my project and created in separate module.
<h3>Module Name : custom_edit_drawable (short name for prefix- c_d_e)</h3>

"c_d_e_" prefix to use so that app module resources should not override them by mistake. Example : "abc" prefix is used by google in support library.

<h3>build.gradle</h3>

```java
dependencies {
   compile 'com.android.support:appcompat-v7:25.3.1'
}

```

use AppCompat >= 23

<h3>Layout file : c_e_d_compound_view.xml</h3>

```java
<?xml version="1.0" encoding="utf-8"?>
<FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="wrap_content">

    <EditText
        android:id="@+id/edt_search"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:inputType="text"
        android:maxLines="1"
        android:paddingEnd="40dp"
        android:paddingLeft="5dp"
        android:paddingRight="40dp"
        android:paddingStart="5dp" />

    <!--make sure you are not using ImageView instead of this-->
    <android.support.v7.widget.AppCompatImageView
        android:id="@+id/drawbleRight_search"
        android:layout_width="30dp"
        android:layout_height="30dp"
        android:layout_gravity="right|center_vertical"
        android:layout_marginLeft="8dp"
        android:layout_marginRight="8dp" />
</FrameLayout>

```

<h3>Custom Attributes : attrs.xml</h3>

```java
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <declare-styleable name="EditTextWithDrawable">
        <attr name="c_e_d_drawableRightSVG" format="reference" />
        <attr name="c_e_d_hint" format="string" />
        <attr name="c_e_d_textSize" format="dimension" />
        <attr name="c_e_d_textColor" format="color" />
    </declare-styleable>
</resources>

```

<h3>Code : EditTextWithDrawable.java</h3>

```java
public class EditTextWithDrawable extends FrameLayout {
    public AppCompatImageView mDrawableRight;
    public EditText mEditText;

    public EditTextWithDrawable(Context context) {
        super(context);
        init(null);
    }

    public EditTextWithDrawable(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(attrs);
    }

    public EditTextWithDrawable(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(attrs);
    }

    @TargetApi(Build.VERSION_CODES.LOLLIPOP)
    public EditTextWithDrawable(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init(attrs);
    }

    private void init(AttributeSet attrs) {
        if (attrs != null && !isInEditMode()) {
            LayoutInflater inflater = (LayoutInflater) getContext()
                    .getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            inflater.inflate(R.layout.c_e_d_compound_view, this, true);
            mDrawableRight = (AppCompatImageView) ((FrameLayout) getChildAt(0)).getChildAt(1);
            mEditText = (EditText) ((FrameLayout) getChildAt(0)).getChildAt(0);

            TypedArray attributeArray = getContext().obtainStyledAttributes(
                    attrs,
                    R.styleable.EditTextWithDrawable);

            int drawableRes =
                    attributeArray.getResourceId(
                            R.styleable.EditTextWithDrawable_c_e_d_drawableRightSVG, -1);
            if (drawableRes != -1) {
                mDrawableRight.setImageResource(drawableRes);
            }

            mEditText.setHint(attributeArray.getString(
                    R.styleable.EditTextWithDrawable_c_e_d_hint));
            mEditText.setTextColor(attributeArray.getColor(
                    R.styleable.EditTextWithDrawable_c_e_d_textColor, Color.BLACK));
            int textSize = attributeArray.getDimensionPixelSize(R.styleable.EditTextWithDrawable_c_e_d_textSize, 15);
            mEditText.setTextSize(TypedValue.COMPLEX_UNIT_PX, textSize);
            android.view.ViewGroup.LayoutParams layoutParams = mDrawableRight.getLayoutParams();
            layoutParams.width = (textSize * 3) / 2;
            layoutParams.height = (textSize * 3) / 2;
            mDrawableRight.setLayoutParams(layoutParams);

            attributeArray.recycle();
        }
    }
}

```

<h3>Example : How to use above view</h3>
<h3>Layout : activity_main.xml</h3>

### Layout : activity_main.xml

```java
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:orientation="vertical">

    <com.customeditdrawable.AppEditTextWithDrawable
        android:id="@+id/edt_search_emp"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        app:c_e_d_drawableRightSVG="@drawable/ic_svg_search"
        app:c_e_d_hint="@string/hint_search_here"
        app:c_e_d_textColor="@color/text_color_dark_on_light_bg"
        app:c_e_d_textSize="@dimen/text_size_small" />
</LinearLayout> 

```

<h3>Activity : MainActivity.java</h3>

```java
public class MainActivity extends AppCompatActivity {
    EditTextWithDrawable mEditTextWithDrawable;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        mEditTextWithDrawable= (EditTextWithDrawable) findViewById(R.id.edt_search_emp);
    }
}

```



## Responding to Touch Events


Many custom views need to accept user interaction in the form of touch events. You can get access to touch events by overriding `onTouchEvent`. There are a number of actions you can filter out. The main ones are

- `ACTION_DOWN`: This is triggered once when your finger first touches the view.
- `ACTION_MOVE`: This is called every time your finger moves a little across the view. It gets called many times.
- `ACTION_UP`: This is the last action to be called as you lift your finger off the screen.

You can add the following method to your view and then observe the log output when you touch and move your finger around your view.

```java
@Override
public boolean onTouchEvent(MotionEvent event) {

    int x = (int) event.getX();
    int y = (int) event.getY();
    int action = event.getAction();

    switch (action) {
        case MotionEvent.ACTION_DOWN:
            Log.i("CustomView", "onTouchEvent: ACTION_DOWN: x = " + x + ", y = " + y);
            break;

        case MotionEvent.ACTION_MOVE:
            Log.i("CustomView", "onTouchEvent: ACTION_MOVE: x = " + x + ", y = " + y);
            break;

        case MotionEvent.ACTION_UP:
            Log.i("CustomView", "onTouchEvent: ACTION_UP: x = " + x + ", y = " + y);
            break;
    }
    return true;
}

```

Further reading:

- [Android official documentation: Responding to Touch Events](https://developer.android.com/training/graphics/opengl/touch.html)

