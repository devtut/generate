---
metaTitle: "Android - Custom Fonts"
description: "Custom font in canvas text, Working with fonts in Android O, Custom font to whole activity, Putting a custom font in your app, Initializing a font, Using a custom font in a TextView, Apply font on TextView by xml (Not required Java code), Efficient Typeface loading"
---

# Custom Fonts



## Custom font in canvas text


Drawing text in canvas with your font from assets.

```java
Typeface typeface = Typeface.createFromAsset(getAssets(), "fonts/SomeFont.ttf");
Paint textPaint = new Paint();
textPaint.setTypeface(typeface);
canvas.drawText("Your text here", x, y, textPaint);

```



## Working with fonts in Android O


Android O changes the way to work with fonts.

Android O introduces a new feature, called **Fonts in XML**, which allows you to use fonts as resources. This means, that there is no need to bundle fonts as assets. Fonts are now compiled in an **R** file and are automatically available in the system as a resource.

In order to add a new **font**, you have to do the following:

- Create a new resource directory: `res/font`.
- Add your font files into this font folder. For example, by adding `myfont.ttf`, you will be able to use this font via `R.font.myfont`.

You can also create your own **font family** by adding the following XML file into the `res/font` directory:

```java
<?xml version="1.0" encoding="utf-8"?>
<font-family xmlns:android="http://schemas.android.com/apk/res/android">
    <font
        android:fontStyle="normal"
        android:fontWeight="400"
        android:font="@font/lobster_regular" />
    <font
        android:fontStyle="italic"
        android:fontWeight="400"
        android:font="@font/lobster_italic" />
</font-family>

```

You can use both the **font** file and the **font family** file in the same way:

<li>
**In an XML file,** by using the `android:fontFamily` attribute, for example like this:

```java
<TextView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:fontFamily="@font/myfont"/>

```


Or like this:

```java
<style name="customfontstyle" parent="@android:style/TextAppearance.Small">
    <item name="android:fontFamily">@font/myfont</item>
</style>

```


</li>
<li>
**In your code**, by using the following lines of code:

```java
Typeface typeface = getResources().getFont(R.font.myfont);
textView.setTypeface(typeface);

```


</li>



## Custom font to whole activity


```java
public class ReplaceFont {

public static void changeDefaultFont(Context context, String oldFont, String assetsFont) {
    Typeface typeface = Typeface.createFromAsset(context.getAssets(), assetsFont);
    replaceFont(oldFont, typeface);
}

private static void replaceFont(String oldFont, Typeface typeface) {
    try {
        Field myField = Typeface.class.getDeclaredField(oldFont);
        myField.setAccessible(true);
        myField.set(null, typeface);
    } catch (NoSuchFieldException e) {
        e.printStackTrace();
    } catch (IllegalAccessException e) {
        e.printStackTrace();
    }
}

```

Then in your activity, in `onCreate()` method:

```java
// Put your font to assets folder...

ReplaceFont.changeDefaultFont(getApplication(), "DEFAULT", "LinLibertine.ttf");

```



## Putting a custom font in your app


1. Go to the (project folder)
1. Then app -> src -> main.
1. Create folder 'assets -> fonts' into the main folder.
1. Put your 'fontfile.ttf' into the fonts folder.



## Initializing a font


```java
private Typeface myFont;

// A good practice might be to call this in onCreate() of a custom
// Application class and pass 'this' as Context. Your font will be ready to use
// as long as your app lives
public void initFont(Context context) {
    myFont = Typeface.createFromAsset(context.getAssets(), "fonts/Roboto-Light.ttf");
}

```



## Using a custom font in a TextView


```java
public void setFont(TextView textView) {
    textView.setTypeface(myFont);    
}

```



## Apply font on TextView by xml (Not required Java code)


**TextViewPlus.java:**

```java
public class TextViewPlus extends TextView {
    private static final String TAG = "TextView";

    public TextViewPlus(Context context) {
        super(context);
    }

    public TextViewPlus(Context context, AttributeSet attrs) {
        super(context, attrs);
        setCustomFont(context, attrs);
    }

    public TextViewPlus(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        setCustomFont(context, attrs);
    }

    private void setCustomFont(Context ctx, AttributeSet attrs) {
        TypedArray a = ctx.obtainStyledAttributes(attrs, R.styleable.TextViewPlus);
        String customFont = a.getString(R.styleable.TextViewPlus_customFont);
        setCustomFont(ctx, customFont);
        a.recycle();
    }

    public boolean setCustomFont(Context ctx, String asset) {
        Typeface typeface = null;
        try {
            typeface = Typeface.createFromAsset(ctx.getAssets(), asset);
        } catch (Exception e) {
            Log.e(TAG, "Unable to load typeface: "+e.getMessage());
            return false;
        }

        setTypeface(typeface);
        return true;
    }
}

```

**attrs.xml:** (Where to place **res/values**)

```java
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <declare-styleable name="TextViewPlus">
        <attr name="customFont" format="string"/>
    </declare-styleable>
</resources>

```

**How to use:**

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout 
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:foo="http://schemas.android.com/apk/res-auto"
    android:orientation="vertical" android:layout_width="fill_parent"
    android:layout_height="fill_parent">

    <com.mypackage.TextViewPlus
        android:id="@+id/textViewPlus1"
        android:layout_height="match_parent"
        android:layout_width="match_parent"
        android:text="@string/showingOffTheNewTypeface"
        foo:customFont="my_font_name_regular.otf">
    </com.mypackage.TextViewPlus>
</LinearLayout>

```



## Efficient Typeface loading


Loading custom fonts can be lead to a bad performance. I highly recommend to use this little helper which saves/loads your already used fonts into a Hashtable.

```java
public class TypefaceUtils {

private static final Hashtable<String, Typeface> sTypeFaces = new Hashtable<>();

/**
 * Get typeface by filename from assets main directory
 *
 * @param context
 * @param fileName the name of the font file in the asset main directory
 * @return
 */
public static Typeface getTypeFace(final Context context, final String fileName) {
    Typeface tempTypeface = sTypeFaces.get(fileName);

    if (tempTypeface == null) {
        tempTypeface = Typeface.createFromAsset(context.getAssets(), fileName);
        sTypeFaces.put(fileName, tempTypeface);
    }

    return tempTypeface;
}

```

}

Usage:

```java
Typeface typeface = TypefaceUtils.getTypeface(context, "RobotoSlab-Bold.ttf");
setTypeface(typeface);

```

