---
metaTitle: "Android - Menu"
description: "Options menu with dividers, Apply custom font to Menu, Creating a Menu in an Activity"
---

# Menu




## Options menu with dividers


In Android there is a default options menu, which can take a number of options. If a larger number of options needs to be displayed, then it makes sense to group those options in order to maintain clarity. Options can be grouped by putting dividers (i.e. horizontal lines) between them. In order to allow for dividers, the following theme can be used:

```java
<style name="AppTheme" parent="Theme.AppCompat.Light.DarkActionBar">
    <!-- Customize your theme here. -->
    <item name="colorPrimary">@color/colorPrimary</item>
    <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
    <item name="colorAccent">@color/colorAccent</item>
    <item name="android:dropDownListViewStyle">@style/PopupMenuListView</item>
</style>
<style name="PopupMenuListView" parent="@style/Widget.AppCompat.ListView.DropDown">
    <item name="android:divider">@color/black</item>
    <item name="android:dividerHeight">1dp</item>
</style>

```

By changing the theme, dividers can be added to a menu.



## Apply custom font to Menu


```java
public static void applyFontToMenu(Menu m, Context mContext){
    for(int i=0;i<m.size();i++) {
        applyFontToMenuItem(m.getItem(i),mContext);
    }
}
public static void applyFontToMenuItem(MenuItem mi, Context mContext) {
    if(mi.hasSubMenu())
        for(int i=0;i<mi.getSubMenu().size();i++) {
            applyFontToMenuItem(mi.getSubMenu().getItem(i),mContext);
        }
    Typeface font = Typeface.createFromAsset(mContext.getAssets(), "fonts/yourCustomFont.ttf");
    SpannableString mNewTitle = new SpannableString(mi.getTitle());
    mNewTitle.setSpan(new CustomTypefaceSpan("", font, mContext), 0, mNewTitle.length(), Spannable.SPAN_INCLUSIVE_INCLUSIVE);
    mi.setTitle(mNewTitle);
}

```

and then in the Activity:

```java
@Override
public boolean onCreateOptionsMenu(Menu menu) {
    getMenuInflater().inflate(R.menu.main, menu);
    applyFontToMenu(menu,this);
    return true;
}

```



## Creating a Menu in an Activity


<br>To define your own menu, create an XML file inside your project's `res/menu/` directory and build the menu with the following elements:

- `<menu>` : Defines a Menu, which holds all the menu items.
- `<item>` : Creates a MenuItem, which represents a single item in a menu. We can also create a nested  element in order to create a submenu.

<h3>**Step 1:**</h3>
Create your own xml file as the following:

In `res/menu/main_menu.xml`:

```java
<?xml version="1.0" encoding="utf-8"?>

<menu xmlns:android="http://schemas.android.com/apk/res/android">
    <item
        android:id="@+id/aboutMenu"
        android:title="About" />
    <item
        android:id="@+id/helpMenu"
        android:title="Help" />
    <item
        android:id="@+id/signOutMenu"
        android:title="Sign Out" />
</menu>

```

<h3>**Step 2:**</h3>

To specify the options menu, override `onCreateOptionsMenu()` in your **activity**.

In this method, you can inflate your menu resource (defined in your XML file i.e., `res/menu/main_menu.xml`)

```

@Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.main_menu, menu);
        return true;
    }

```

When the user selects an item from the options menu, the system calls your **activity's** overridden `onOptionsItemSelected()` method.

- This method passes the MenuItem selected.
- You can identify the item by calling `getItemId()`, which returns the unique ID for the menu item (defined by the `android:id attribute` in the menu resource - `res/menu/main_menu.xml`)*/

```

@Override
public boolean onOptionsItemSelected(MenuItem item) {
    switch (item.getItemId()) {
        case R.id.aboutMenu:
            Log.d(TAG, "Clicked on About!");
            // Code for About goes here
            return true;
        case R.id.helpMenu:
            Log.d(TAG, "Clicked on Help!");
            // Code for Help goes here
            return true;
        case R.id.signOutMenu:
            Log.d(TAG, "Clicked on Sign Out!");
            // SignOut method call goes here
            return true;
        default:
            return super.onOptionsItemSelected(item);
    }
}

```

###  Wrapping up! 

Your `Activity` code should look like below:

```java
public class MainActivity extends AppCompatActivity {

    private static final String TAG = "mytag";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.main_menu, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.aboutMenu:
                Log.d(TAG, "Clicked on About!");
                // Code for About goes here
                return true;
            case R.id.helpMenu:
                Log.d(TAG, "Clicked on Help!");
                // Code for Help goes here
                return true;
            case R.id.signOutMenu:
                Log.d(TAG, "User signed out");
                // SignOut method call goes here
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
}

```

<h3> **Screenshot of how your own Menu looks:** </h3>

[<img src="https://i.stack.imgur.com/HE2J8m.png" alt="SampleMenuApp" />](https://i.stack.imgur.com/HE2J8m.png)



#### Syntax


- inflater.inflate(R.menu.your_xml_file, menu);



#### Parameters


|**Parameter**|**Description**
|---|---|---|---|---|---|---|---|---|---
|`inflate(int menuRes, Menu menu)`|Inflate a menu hierarchy from the specified XML resource.
|`getMenuInflater ()`|Returns a `MenuInflater` with this context.
|`onCreateOptionsMenu (Menu menu)`|Initialize the contents of the Activity's standard options menu. You should place your menu items in to menu.
|`onOptionsItemSelected (MenuItem item)`|This method is called whenever an item in your options menu is selected



#### Remarks


To know more about **Menus**, read [this](https://developer.android.com/guide/topics/ui/menus.html). Hope it helps!

