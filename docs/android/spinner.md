---
metaTitle: "Android - Spinner"
description: "Adding a spinner to your activity, Basic Spinner Example"
---

# Spinner



## Adding a spinner to your activity


In /res/values/strings.xml:

```java
<string-array name="spinner_options">
    <item>Option 1</item>
    <item>Option 2</item>
    <item>Option 3</item>
</string-array>

```

In layout XML:

```java
<Spinner
    android:id="@+id/spinnerName"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:entries="@array/spinner_options" />

```

In Activity:

```java
Spinner spinnerName = (Spinner) findViewById(R.id.spinnerName);
spinnerName.setOnItemSelectedListener(new OnItemSelectedListener() {
    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
         String chosenOption = (String) parent.getItemAtPosition(position);
    }
    @Override
    public void onNothingSelected(AdapterView<?> parent) {}
});

```



## Basic Spinner Example


**Spinner**
It is a type of dropdown input.
Firstly in layout

```

 <Spinner
    android:id="@+id/spinner"     <!-- id to refer this spinner from JAVA-->
    android:layout_width="match_parent"
    android:layout_height="wrap_content">
    
</Spinner>

```

Now Secondly populate values in spinner
There are mainly two ways to populate values in `spinner`.

<li>From XML itself
create a **array.xml** in **values** directory under **res**.
Create this `array`</li>

```

 <string-array name="defaultValue">
    <item>--Select City Area--</item>
    <item>--Select City Area--</item>
    <item>--Select City Area--</item>
</string-array>

```

Now add this line in sppiner XML

```

           android:entries="@array/defaultValue"

```


1. You can also add values via JAVA

if you are using in `activity`
cityArea = (Spinner) findViewById(R.id.cityArea);
else if you are using in `fragment`

```java
cityArea = (Spinner) findViewById(R.id.cityArea);

```

Now create a `arrayList` of `Strings`

```java
ArrayList<String> area = new ArrayList<>();
//add values in area arrayList
cityArea.setAdapter(new ArrayAdapter<String>(context
                            , android.R.layout.simple_list_item_1, area));

```

This will look like

[<img src="http://i.stack.imgur.com/BuLI2.jpg" alt="enter image description here" />](http://i.stack.imgur.com/BuLI2.jpg)
[<img src="http://i.stack.imgur.com/3rFYB.jpg" alt="enter image description here" />](http://i.stack.imgur.com/3rFYB.jpg)

According to the device Android version it will render style

Following are some of the default themes

If an app does not explicitly request a theme in its manifest, Android System will determine the default theme based on the app’s targetSdkVersion to maintain the app’s original expectations:

|Android SDK Version|Default Theme
|---|---|---|---|---|---|---|---|---|---
|Version < 11|@android:style/Theme
|Version between 11 and 13|@android:style/Theme.Holo
|14 and higher|@android:style/Theme.DeviceDefault

`Spinner` can be easily customized with the help of xml
eg

```

android:background="@drawable/spinner_background"

 android:layout_margin="16dp"

 android:padding="16dp"

```

Create a custom background in XML and use it.

easily get the position and other details of the selected item in spinner

```java
cityArea.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            areaNo = position;
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    });

```

Change the text color of the selected item in spinner

This can be done in two ways in XML

```java
<item android:state_activated="true" android:color="@color/red"/>

```

This will change the selected item color in the popup.

and from JAVA do this (in the setOnItemSelectedListener(...))

```

@Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                 ((TextView) parent.getChildAt(0)).setTextColor(0x00000000);
// similarly change `background color` etc.
            }

```

