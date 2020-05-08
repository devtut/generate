---
metaTitle: "Android - Button"
description: "Using the same click event for one or more Views in the XML, inline onClickListener, Defining external Listener, Customizing Button style, Using the layout to define a click action, Listening to the long click events, Custom Click Listener to prevent multiple fast clicks"
---

# Button



## Using the same click event for one or more Views in the XML


When we create any View in layout, we can use the android:onClick attribute to reference a method in the associated activity or fragment to handle the click events.

**XML Layout**

```java
<Button android:id="@+id/button"
    ...
    // onClick should reference the method in your activity or fragment
    android:onClick="doSomething" />

// Note that this works with any class which is a subclass of View, not just Button
<ImageView android:id="@+id/image"
    ...
    android:onClick="doSomething" />

```

**Activity/fragment code**

In your **code**, create the method you named, where v will be the view that was touched, and do something for each view that calls this method.

```java
public void doSomething(View v) {
    switch(v.getId()) {
        case R.id.button:
            // Button was clicked, do something.
            break;
        case R.id.image:
            // Image was clicked, do something else.
            break;
    }
}

```

If you want, you can also use different method for each View (in this case, of course, you don't have to check for the ID).



## inline onClickListener


Say we have a button (we can create it programmatically, or bind it from a view using findViewbyId(), etc...)

```java
Button btnOK = (...) 

```

Now, create an anonymous class and set it inline.

```java
btnOk.setOnClickListener(new View.OnClickListener() {
    @Override
    public void onClick(View v) {
        // Do stuff here...
    }
});

```



## Defining external Listener


### **When should I use it**

- When the code inside an inline listener is too big and your method / class becomes ugly and hard to read
- You want to perform same action in various elements (view) of your app

To achieve this you need to create a class implementing one of the listeners in the [View API](https://developer.android.com/reference/android/view/View.html).

For example, give help when long click on any element:

```java
public class HelpLongClickListener implements View.OnLongClickListener 
{
    public HelpLongClickListener() {
    }

    @Override 
    public void onLongClick(View v) {
        // show help toast or popup
    }
}

```

Then you just need to have an attribute or variable in your `Activity` to use it:

```java
HelpLongClickListener helpListener = new HelpLongClickListener(...);

button1.setOnClickListener(helpListener);
button2.setOnClickListener(helpListener);
label.setOnClickListener(helpListener);
button1.setOnClickListener(helpListener);

```

**NOTE:** defining listeners in separated class has one disadvantage, it cannot access class fields directly, so you need to pass data (context, view) through constructor unless you make attributes public or define geters.



## Customizing Button style


There are many possible ways of customizing the look of a Button. This example presents several options:

**Option 0: Use ThemeOverlay (currently the easiest/quickest way)**

Create a new style in your styles file:

**styles.xml**

```java
<resources>
    <style name=“mybutton” parent=”ThemeOverlay.AppCompat.Ligth”>
        <!-- customize colorButtonNormal for the disable color -->
        <item name="colorButtonNormal">@color/colorbuttonnormal</item>
        <!-- customize colorAccent for the enabled color -->
        <item name="colorButtonNormal">@color/coloraccent</item>
    </style>
</resources>

```

Then in the layout where you place your button (e.g. MainActivity):

**activity_main.xml**

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:layout_gravity="center_horizontal"
    android:gravity="center_horizontal"
    android:orientation="vertical"
    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context=".MainActivity">

            <Button
                android:id="@+id/mybutton"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:text="Hello"
                android:theme="@style/mybutton"
                style="@style/Widget.AppCompat.Button.Colored"/>

</LinearLayout>

```

**Option 1: Create your own button style**

In values/styles.xml, create a new style for your button:

**styles.xml**

```java
<resources>
        <style name="mybuttonstyle" parent="@android:style/Widget.Button">
            <item name="android:gravity">center_vertical|center_horizontal</item>
            <item name="android:textColor">#FFFFFFFF</item>
            <item name="android:shadowColor">#FF000000</item>
            <item name="android:shadowDx">0</item>
            <item name="android:shadowDy">-1</item>
            <item name="android:shadowRadius">0.2</item>
            <item name="android:textSize">16dip</item>
            <item name="android:textStyle">bold</item>
            <item name="android:background">@drawable/button</item>
        </style>
    </resources>

```

Then in the layout where you place your button (e.g. in MainActivity):

**activity_main.xml**

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:layout_gravity="center_horizontal"
    android:gravity="center_horizontal"
    android:orientation="vertical"
    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context=".MainActivity">

            <Button
                android:id="@+id/mybutton"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:text="Hello"
                android:theme="@style/mybuttonstyle"/>

</LinearLayout>

```

**Option 2: Assign a drawable for each of your button states**

Create an xml file into drawable folder called 'mybuttondrawable.xml' to define the drawable resource of each of your button states:

**drawable/mybutton.xml**

```java
<selector xmlns:android="http://schemas.android.com/apk/res/android">
    <item
        android:state_enabled="false"
        android:drawable="@drawable/mybutton_disabled" />
    <item
        android:state_pressed="true"
        android:state_enabled="true"
        android:drawable="@drawable/mybutton_pressed" />
    <item
        android:state_focused="true"
        android:state_enabled="true"
        android:drawable="@drawable/mybutton_focused" />
    <item
        android:state_enabled="true"
        android:drawable="@drawable/mybutton_enabled" />
</selector>

```

Each of those drawables may be images (e.g. mybutton_disabled.png) or xml files defined by you and stored in the drawables folder. For instance:

**drawable/mybutton_disabled.xml**

```java
<?xml version="1.0" encoding="utf-8"?>

    <shape xmlns:android="http://schemas.android.com/apk/res/android" android:shape="rectangle">
        <gradient
            android:startColor="#F2F2F2"
            android:centerColor="#A4A4A4"
            android:endColor="#F2F2F2"
            android:angle="90"/>
        <padding android:left="7dp"
            android:top="7dp"
            android:right="7dp"
            android:bottom="7dp" />
        <stroke
            android:width="2dip"
            android:color="#FFFFFF" />
        <corners android:radius= "8dp" />
    </shape>

```

Then in the layout where you place your button (e.g. MainActivity):

**activity_main.xml**

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:layout_gravity="center_horizontal"
    android:gravity="center_horizontal"
    android:orientation="vertical"
    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context=".MainActivity">

            <Button
                android:id="@+id/mybutton"
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:text="Hello"
                android:background="@drawable/mybuttondrawable"/>

</LinearLayout>

```

**Option 3: Add your button style to your App theme**

You can override the default android button style in the definition of your app theme (in values/styles.xml).

**styles.xml**

```java
<resources>
     <style name="AppTheme" parent="android:Theme">
            <item name="colorPrimary">@color/colorPrimary</item>
            <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
            <item name="colorAccent">@color/colorAccent</item>
              <item name="android:button">@style/mybutton</item>
     </style>
    
     <style name="mybutton" parent="android:style/Widget.Button">
             <item name="android:gravity">center_vertical|center_horizontal</item>
                <item name="android:textColor">#FFFFFFFF</item>
                <item name="android:shadowColor">#FF000000</item>
                <item name="android:shadowDx">0</item>
                <item name="android:shadowDy">-1</item>
                <item name="android:shadowRadius">0.2</item>
                <item name="android:textSize">16dip</item>
                <item name="android:textStyle">bold</item>
                <item name="android:background">@drawable/anydrawable</item>
     </style>
</resources>

```

**Option 4: Overlay a color on the default button style programatically**

Just find you button in your activity and apply a color filter:

```java
Button mybutton = (Button) findViewById(R.id.mybutton);
mybutton.getBackground().setColorFilter(anycolor, PorterDuff.Mode.MULTIPLY)

```

You can check different blending modes [here](https://developer.android.com/reference/android/graphics/PorterDuff.Mode.html) and nice examples [here](http://ssp.impulsetrain.com/porterduff.html).



## Using the layout to define a click action


When we create a button in layout, we can use the `android:onClick` attribute to reference a method in code to handle clicks.

**Button**

```java
<Button
    android:width="120dp"
    android:height="wrap_content"
    android:text="Click me"
    android:onClick="handleClick" />

```

Then in your activity, create the `handleClick` method.

```java
public void handleClick(View v) {
    // Do whatever.
}

```



## Listening to the long click events


To catch a long click and use it you need to provide appropriate listener to button:

```java
View.OnLongClickListener listener = new View.OnLongClickListener() {
    public boolean onLongClick(View v) {
        Button clickedButton = (Button) v;
        String buttonText = clickedButton.getText().toString();
        Log.v(TAG, "button long pressed --> " + buttonText);
        return true;
    }
};

button.setOnLongClickListener(listener);

```



## Custom Click Listener to prevent multiple fast clicks


In order to **prevent a button from firing multiple times within a short period of time** (let's say 2 clicks within 1 second, which may cause serious problems if the flow is not controlled), one can implement a custom ****SingleClickListener****.

This ClickListener sets a specific time interval as threshold (for instance, 1000ms).<br />
When the button is clicked, a check will be ran to see if the trigger was executed in the past amount of time you defined, and if not it will trigger it.

```java
public class SingleClickListener implements View.OnClickListener {

    protected int defaultInterval;
    private long lastTimeClicked = 0;

    public SingleClickListener() {
        this(1000);
    }

    public SingleClickListener(int minInterval) {
        this.defaultInterval = minInterval;
    }

    @Override
    public void onClick(View v) {
        if (SystemClock.elapsedRealtime() - lastTimeClicked < defaultInterval) {
            return;
        }
        lastTimeClicked = SystemClock.elapsedRealtime();
        performClick(v);
    }

    public abstract void performClick(View v);

}

```

And in the class, the SingleClickListener is associated to the Button at stake

```java
myButton = (Button) findViewById(R.id.my_button);
myButton.setOnClickListener(new SingleClickListener() {
    @Override
    public void performClick(View view) {
        // do stuff
    }
});

```



#### Syntax


- <Button ... />
- android:onClick="methodname"
- button.setOnClickListener(new OnClickListener(){...});
- public class classname implements View.OnLongClickListener

