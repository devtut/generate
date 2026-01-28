---
metaTitle: "Android - Localization with resources in Android"
description: "Configuration types and qualifier names for each folder under the res directory, Currency, Adding translation to your Android app, Type of resource directories under the res folder, Change locale of android application programatically"
---

# Localization with resources in Android




## Configuration types and qualifier names for each folder under the "res" directory


Each resource directory under the `res` folder (listed in the example above) can have different variations of the contained resources in similarly named directory suffixed with different `qualifier-values` for each `configuration-type`.

Example of variations of `` directory with different qualifier values suffixed which are often seen in our android projects:

- drawable/
- drawable-en/
- drawable-fr-rCA/
- drawable-en-port/
- drawable-en-notouch-12key/
- drawable-port-ldpi/
- drawable-port-notouch-12key/

### **Exhaustive list of all different configuration types and their qualifier values for android resources:**

|Configuration|Qualifier Values
|---|---|---|---|---|---|---|---|---|---
|**MCC and MNC**|Examples:
||mcc310
||mcc310-mnc004
||mcc208-mnc00
||etc.
|**Language and region**|Examples:
||en
||fr
||en-rUS
||fr-rFR
||fr-rCA
|**Layout Direction**|ldrtl
||ldltr
|**smallestWidth**|swdp
||Examples:
||sw320dp
||sw600dp
||sw720dp
|**Available width**|wdp
||w720dp
||w1024dp
|**Available height**|hdp
||h720dp
||h1024dp
|**Screen size**|small
||normal
||large
||xlarge
|**Screen aspect**|long
||notlong
|**Round screen**|round
||notround
|**Screen orientation**|port
||land
|**UI mode**|car
||desk
||television
||appliancewatch
|**Night mode**|night
||notnight
|**Screen pixel density (dpi)**|ldpi
||mdpi
||hdpi
||xhdpi
||xxhdpi
||xxxhdpi
||nodpi
||tvdpi
||anydpi
|**Touchscreen type**|notouch
||finger
|**Keyboard availability**|keysexposed
||keyshidden
||keyssoft
|**Primary text input method**|nokeys
||qwerty
||12key
|**Navigation key availability**|navexposed
||navhidden
|**Primary non-touch navigation method**|nonav
||dpad
||trackball
||wheel
|**Platform Version (API level)**|Examples:
||v3
||v4
||v7



## Currency


```java
Currency currency = Currency.getInstance("USD");
NumberFormat format = NumberFormat.getCurrencyInstance();
format.setCurrency(currency);
format.format(10.00);

```



## Adding translation to your Android app


You have to create a different `strings.xml` file for every new language.

1. Right-click on the **res** folder
1. Choose **New** → **Values resource file**
1. Select a locale from the available qualifiers
1. Click on the **Next** button (>>)
1. Select a language
1. Name the file **strings.xml**

**strings.xml**

```java
<resources>
    <string name="app_name">Testing Application</string>
    <string name="hello">Hello World</string>
</resources>

```

**strings.xml(hi)**

```java
<resources>
    <string name="app_name">परीक्षण आवेदन</string>
    <string name="hello">नमस्ते दुनिया</string>
</resources>

```

**Setting the language programmatically:**

```java
public void setLocale(String locale) // Pass "en","hi", etc.
{
    myLocale = new Locale(locale);
    // Saving selected locale to session - SharedPreferences.
    saveLocale(locale);
    // Changing locale.
    Locale.setDefault(myLocale);
    android.content.res.Configuration config = new android.content.res.Configuration();
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
        config.setLocale(myLocale);
    } else {
        config.locale = myLocale;
    }
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
        getBaseContext().createConfigurationContext(config);
    } else {
        getBaseContext().getResources().updateConfiguration(config, getBaseContext().getResources().getDisplayMetrics());
    }
}

```

The function above will change the text fields which are referenced from **strings.xml**. For example, assume that you have the following two text views:

```java
<TextView
    android:layout_width="wrap_content"
    android:layout_height="wrap_content" 
    android:text="@string/app_name"/>
<TextView
    android:layout_width="wrap_content"
    android:layout_height="wrap_content" 
    android:text="@string/hello"/>

```

Then, after changing the locale, the language strings having the ids `app_name` and `hello` will be changed accordingly.



## Type of resource directories under the "res" folder


When localizing different types of resources are required, each of which has its own home in the android project structure.
Following are the different directories that we can place under the `\res` directory. The resource types placed in each of these directories are explained in the table below:

|Directory|Resource Type
|---|---|---|---|---|---|---|---|---|---
|animator/|XML files that define property animations.
|anim/|XML files that define tween animations. (Property animations can also be saved in this directory, but the animator/ directory is preferred for property animations to distinguish between the two types.)
|color/|XML files that define a state list of colors. See Color State List Resource
|drawable/|"Bitmap files (.png, .9.png, .jpg, .gif) or XML files that are compiled into the following drawable resource subtypes: : `Bitmap files - Nine-Patches (re-sizable bitmaps) - State lists - Shapes - Animation drawables - Other drawables - "`
|mipmap/|Drawable files for different launcher icon densities. For more information on managing launcher icons with mipmap/ folders, see Managing Projects Overview.
|layout/|XML files that define a user interface layout. See Layout Resource.
|menu/|XML files that define application menus, such as an Options Menu, Context Menu, or Sub Menu. See Menu Resource.
|raw/|Arbitrary files to save in their raw form. To open these resources with a raw InputStream, call Resources.openRawResource() with the resource ID, which is R.raw.filename.
||However, if you need access to original file names and file hierarchy, you might consider saving some resources in the assets/ directory (instead ofres/raw/). Files in assets/ are not given a resource ID, so you can read them only using AssetManager.
|values/|XML files that contain simple values, such as strings, integers, and colors, as well as styles and themes
|xml/|Arbitrary XML files that can be read at runtime by calling Resources.getXML(). Various XML configuration files must be saved here, such as a searchable configuration.



## Change locale of android application programatically


In above examples you understand how to localize resources of application. Following example explain how to change the application locale within application, not from device. In order to change Application locale only, you can use below locale util.

```java
import android.app.Application;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.os.Build;
import android.preference.PreferenceManager;
import android.view.ContextThemeWrapper;

import java.util.Locale;

/**
 * Created by Umesh on 10/10/16.
 */
public class LocaleUtils {

    private static Locale mLocale;

    public static void setLocale(Locale locale){
        mLocale = locale;
        if(mLocale != null){
            Locale.setDefault(mLocale);
        }
    }

    public static void updateConfiguration(ContextThemeWrapper wrapper){
        if(mLocale != null && Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1){
            Configuration configuration = new Configuration();
            configuration.setLocale(mLocale);
            wrapper.applyOverrideConfiguration(configuration);
        }
    }

    public static void updateConfiguration(Application application, Configuration configuration){
        if(mLocale != null && Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1){
            Configuration config = new Configuration(configuration);
            config.locale = mLocale;
            Resources res = application.getBaseContext().getResources();
            res.updateConfiguration(configuration, res.getDisplayMetrics());
        }
    }

    public static void updateConfiguration(Context context, String language, String country){
        Locale locale = new Locale(language,country);
        setLocale(locale);
        if(mLocale != null){
            Resources res = context.getResources();
            Configuration configuration = res.getConfiguration();
            configuration.locale = mLocale;
            res.updateConfiguration(configuration,res.getDisplayMetrics());
        }
    }




    public static String getPrefLangCode(Context context) {
        return PreferenceManager.getDefaultSharedPreferences(context).getString("lang_code","en");
    }

    public static void setPrefLangCode(Context context, String mPrefLangCode) {

        SharedPreferences.Editor editor = PreferenceManager.getDefaultSharedPreferences(context).edit();
        editor.putString("lang_code",mPrefLangCode);
        editor.commit();
    }

    public static  String getPrefCountryCode(Context context) {
        return PreferenceManager.getDefaultSharedPreferences(context).getString("country_code","US");
    }

    public static void setPrefCountryCode(Context context,String mPrefCountryCode) {

        SharedPreferences.Editor editor = PreferenceManager.getDefaultSharedPreferences(context).edit();
        editor.putString("country_code",mPrefCountryCode);
        editor.commit();
    }
}

```

Initialize locale that user preferred, from Application class.

```java
public class LocaleApp extends Application{

    @Override
    public void onCreate() {
        super.onCreate();

        LocaleUtils.setLocale(new Locale(LocaleUtils.getPrefLangCode(this), LocaleUtils.getPrefCountryCode(this)));
        LocaleUtils.updateConfiguration(this, getResources().getConfiguration());
    }
}

```

You also need to create a base activity and extend this activity to all other activity so that you can change locale of application only one place as follows :

```java
public abstract class LocalizationActivity extends AppCompatActivity {

    public LocalizationActivity() {
        LocaleUtils.updateConfiguration(this);
    }

    // We only override onCreate
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

}

```

> 
Note : Always initialize locale in constructor.


Now you can use LocalizationActivity as follow.

```java
public class MainActivity extends LocalizationActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

    }
}

```

> 
<p>Note: When you change locale of application programmatically, need to
restart your activity to take the effect of locale change
In order to work properly for this solution you and use locale from shared preferences on app startup you `android:name=".LocaleApp"` in you Manifest.xml.</p>


Sometimes Lint checker prompt to create the release build. To solve such issue follow below options.

**First:**

If you want to disable translation for some strings only then add following      attribute to default string.xml

```java
<string name="developer" translatable="false">Developer Name</string>

```

**Second:**

Ignore all missing translation from resource file add following attribute
It's the ignore attribute of the tools namespace in your strings file, as follows:

```java
<?xml version="1.0" encoding="utf-8"?>
<resources
  xmlns:tools="http://schemas.android.com/tools"
  tools:ignore="MissingTranslation" >
http://stackoverflow.com/documentation/android/3345/localization-with-resources-in-android#
  <!-- your strings here; no need now for the translatable attribute -->

</resources>

```

**Third:**

Another way to disable non-translatable string

[http://tools.android.com/recent/non-translatablestrings](http://tools.android.com/recent/non-translatablestrings)

If you have a lot of resources that should not be translated, you can place them in a file named donottranslate.xml and lint will consider all of them non-translatable resources.

**Fourth:**

You can also add locale in resource file

```java
<resources
xmlns:tools="http://schemas.android.com/tools"
    tools:locale="en" tools:ignore="MissingTranslation">

```

You can also disable missing translation check for lint from app/build.gradle

```java
lintOptions {
        
        disable 'MissingTranslation'
    }

```

