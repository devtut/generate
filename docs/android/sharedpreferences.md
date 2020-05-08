---
metaTitle: "Android - SharedPreferences"
description: "Implementing a Settings screen using SharedPreferences, Commit vs. Apply, Read and write values to SharedPreferences, Retrieve all stored entries from a particular SharedPreferences file, Reading and writing data to SharedPreferences with Singleton, Listening for SharedPreferences changes, getPreferences(int) VS getSharedPreferences(String, int), Store, Retrieve, Remove and Clear Data from SharedPreferences, Removing keys, Different ways of instantiating an object of SharedPreferences, Supported data types in SharedPreferences, Add filter for EditTextPreference, Support pre-Honeycomb with StringSet"
---

# SharedPreferences


SharedPreferences provide a way to save data to disk in the form of **key-value** pairs.



## Implementing a Settings screen using SharedPreferences


One use of `SharedPreferences` is to implement a "Settings" screen in your app, where the user can set their preferences / options. Like this:

[<img src="https://i.stack.imgur.com/B7eaU.png" alt="Screenshot" />](https://i.stack.imgur.com/B7eaU.png)

A PreferenceScreen saves user preferences in `SharedPreferences`. To create a PreferenceScreen, you need a few things:

**An XML file to define the available options:**

This goes in `/res/xml/preferences.xml`, and for the above settings screen, it looks like this:

```java
<PreferenceScreen
    xmlns:android="http://schemas.android.com/apk/res/android">
    <PreferenceCategory
        android:title="General options">
        <CheckBoxPreference
            android:key = "silent_mode"
            android:defaultValue="false"
            android:title="Silent Mode"
            android:summary="Mute all sounds from this app" />

        <SwitchPreference
            android:key="awesome_mode"
            android:defaultValue="false"
            android:switchTextOn="Yes"
            android:switchTextOff="No"
            android:title="Awesome mode™"
            android:summary="Enable the Awesome Mode™ feature"/>

        <EditTextPreference
            android:key="custom_storage"
            android:defaultValue="/sdcard/data/"
            android:title="Custom storage location"
            android:summary="Enter the directory path where you want data to be saved. If it does not exist, it will be created."
            android:dialogTitle="Enter directory path (eg. /sdcard/data/ )"/>
    </PreferenceCategory>
</PreferenceScreen>

```

This defines the available options in the settings screen. There are many other types of `Preference` listed in the Android Developers documentation on the [Preference Class](https://developer.android.com/reference/android/preference/Preference.html).

Next, we need **an Activity to host our Preferences** user interface. In this case, it's quite short, and looks like this:

```java
package com.example.preferences;

import android.preference.PreferenceActivity;
import android.os.Bundle;

public class PreferencesActivity extends PreferenceActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);
    }
}

```

It extends `PreferenceActivity`, and provides the user interface for the preferences screen. It can be started just like a normal activity, in this case, with something like:

```java
Intent i = new Intent(this, PreferencesActivity.class);
startActivity(i);

```

Don't forget to add `PreferencesActivity` to your `AndroidManifest.xml`.

**Getting the values of the preferences inside your app** is quite simple, just call `setDefaultValues()` first, in order to set the default values defined in your XML, and then get the default `SharedPreferences`. An example:

```java
//set the default values we defined in the XML
PreferenceManager.setDefaultValues(this, R.xml.preferences, false);
SharedPreferences preferences = PreferenceManager.getDefaultSharedPreferences(this);
    
//get the values of the settings options
boolean silentMode = preferences.getBoolean("silent_mode", false);
boolean awesomeMode = preferences.getBoolean("awesome_mode", false);
    
String customStorage = preferences.getString("custom_storage", "");

```



## Commit vs. Apply


The [`editor.apply()`](https://developer.android.com/reference/android/content/SharedPreferences.Editor.html#apply()) method is **asynchronous**, while [`editor.commit()`](https://developer.android.com/reference/android/content/SharedPreferences.Editor.html#commit()) is **synchronous**.

Obviously, you should call either `apply()` or `commit()`.

```java
SharedPreferences settings = getSharedPreferences(PREFS_FILE, MODE_PRIVATE);
SharedPreferences.Editor editor = settings.edit();
editor.putBoolean(PREF_CONST, true);
// This will asynchronously save the shared preferences without holding the current thread.
editor.apply();

```

```java
SharedPreferences settings = getSharedPreferences(PREFS_FILE, MODE_PRIVATE);
SharedPreferences.Editor editor = settings.edit();
editor.putBoolean(PREF_CONST, true);
// This will synchronously save the shared preferences while holding the current thread until done and returning a success flag.
boolean result = editor.commit();

```

`apply()` was added in 2.3 (API 9), it commits without returning a boolean indicating success or failure.

`commit()` returns true if the save works, false otherwise.

`apply()` was added as the Android dev team noticed that almost no one took notice of the return value, so apply is faster as it is asynchronous.

Unlike `commit()`, which writes its preferences out to persistent storage synchronously, `apply()` commits its changes to the in-memory `SharedPreferences` immediately but starts an asynchronous commit to disk and you won't be notified of any failures. If another editor on this `SharedPreferences` does a regular `commit()` while a `apply()` is still outstanding, the `commit()` will block until all async commits(apply) are completed as well as any other sync commits that may be pending.



## Read and write values to SharedPreferences


```java
public class MyActivity extends Activity {

    private static final String PREFS_FILE = "NameOfYourPrefrenceFile";
    // PREFS_MODE defines which apps can access the file
    private static final int PREFS_MODE = Context.MODE_PRIVATE;
    // you can use live template "key" for quickly creating keys
    private static final String KEY_BOOLEAN = "KEY_FOR_YOUR_BOOLEAN";
    private static final String KEY_STRING = "KEY_FOR_YOUR_STRING";
    private static final String KEY_FLOAT = "KEY_FOR_YOUR_FLOAT";
    private static final String KEY_INT = "KEY_FOR_YOUR_INT";
    private static final String KEY_LONG = "KEY_FOR_YOUR_LONG";

    @Override
    protected void onStart() {
        super.onStart();
    
        // Get the saved flag (or default value if it hasn't been saved yet)
        SharedPreferences settings = getSharedPreferences(PREFS_FILE, PREFS_MODE);
        // read a boolean value (default false)
        boolean booleanVal = settings.getBoolean(KEY_BOOLEAN, false);
        // read an int value (Default 0)
        int intVal = settings.getInt(KEY_INT, 0);
        // read a string value (default "my string")
        String str = settings.getString(KEY_STRING, "my string");
        // read a long value (default 123456)
        long longVal = settings.getLong(KEY_LONG, 123456);
        // read a float value (default 3.14f)
        float floatVal = settings.getFloat(KEY_FLOAT, 3.14f);
    }

    @Override
    protected void onStop() {
        super.onStop();
        
        // Save the flag
        SharedPreferences settings = getSharedPreferences(PREFS_FILE, PREFS_MODE);
        SharedPreferences.Editor editor = settings.edit();
        // write a boolean value
        editor.putBoolean(KEY_BOOLEAN, true);
        // write an integer value
        editor.putInt(KEY_INT, 123);
        // write a string
        editor.putString(KEY_STRING, "string value");
        // write a long value
        editor.putLong(KEY_LONG, 456876451);
        // write a float value
        editor.putFloat(KEY_FLOAT, 1.51f);
        editor.apply();
    }
}

```

[`getSharedPreferences()`](https://developer.android.com/reference/android/content/Context.html#getSharedPreferences(java.lang.String,%20int)) is a method from the [`Context`](https://developer.android.com/reference/android/content/Context.html) class — which [`Activity`](https://developer.android.com/reference/android/app/Activity.html) extends.  If you need to access the `getSharedPreferences()` method from other classes, you can use `context.getSharedPreferences()` with a `Context` Object reference from an `Activity`, `View`, or `Application`.



## Retrieve all stored entries from a particular SharedPreferences file


The [`getAll()`](https://developer.android.com/reference/android/content/SharedPreferences.html#getAll()) method retrieves all values from the preferences. We can use it, for instance, to log the current content of the `SharedPreferences`:

```java
private static final String PREFS_FILE = "MyPrefs";

public static void logSharedPreferences(final Context context) {
    SharedPreferences sharedPreferences = context.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE);
    Map<String, ?> allEntries = sharedPreferences.getAll();
    for (Map.Entry<String, ?> entry : allEntries.entrySet()) {
        final String key = entry.getKey();
        final Object value = entry.getValue();
        Log.d("map values", key + ": " + value);
    } 
}

```

The documentation warns you about modifying the `Collection` returned by `getAll`:

> 
<p>Note that you must not modify the collection returned by this method,
or alter any of its contents. The consistency of your stored data is
not guaranteed if you do.</p>




## Reading and writing data to SharedPreferences with Singleton


SharedPreferences Manager (Singleton) class to read and write all types of data.

```java
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

import com.google.gson.Gson;

import java.lang.reflect.Type;

/**
 * Singleton Class for accessing SharedPreferences,
 * should be initialized once in the beginning by any application component using static
 * method initialize(applicationContext)
 */
public class SharedPrefsManager {
    
    private static final String TAG = SharedPrefsManager.class.getName();
    private SharedPreferences prefs;
    private static SharedPrefsManager uniqueInstance;
    public static final String PREF_NAME = "com.example.app";

    private SharedPrefsManager(Context appContext) {
        prefs = appContext.getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
    }

    /**
     * Throws IllegalStateException if this class is not initialized
     *
     * @return unique SharedPrefsManager instance
     */
    public static SharedPrefsManager getInstance() {
        if (uniqueInstance == null) {
            throw new IllegalStateException(
                    "SharedPrefsManager is not initialized, call initialize(applicationContext) " +
                            "static method first");
        }
        return uniqueInstance;
    }

    /**
     * Initialize this class using application Context,
     * should be called once in the beginning by any application Component
     *
     * @param appContext application context
     */
    public static void initialize(Context appContext) {
        if (appContext == null) {
            throw new NullPointerException("Provided application context is null");
        }
        if (uniqueInstance == null) {
            synchronized (SharedPrefsManager.class) {
                if (uniqueInstance == null) {
                    uniqueInstance = new SharedPrefsManager(appContext);
                }
            }
        }
    }

    private SharedPreferences getPrefs() {
        return prefs;
    }

    /**
     * Clears all data in SharedPreferences
     */
    public void clearPrefs() {
        SharedPreferences.Editor editor = getPrefs().edit();
        editor.clear();
        editor.commit();
    }

    public void removeKey(String key) {
        getPrefs().edit().remove(key).commit();
    }

    public boolean containsKey(String key) {
        return getPrefs().contains(key);
    }

    public String getString(String key, String defValue) {
        return getPrefs().getString(key, defValue);
    }

    public String getString(String key) {
        return getString(key, null);
    }

    public void setString(String key, String value) {
        SharedPreferences.Editor editor = getPrefs().edit();
        editor.putString(key, value);
        editor.apply();
    }

    public int getInt(String key, int defValue) {
        return getPrefs().getInt(key, defValue);
    }

    public int getInt(String key) {
        return getInt(key, 0);
    }

    public void setInt(String key, int value) {
        SharedPreferences.Editor editor = getPrefs().edit();
        editor.putInt(key, value);
        editor.apply();
    }

    public long getLong(String key, long defValue) {
        return getPrefs().getLong(key, defValue);
    }

    public long getLong(String key) {
        return getLong(key, 0L);
    }

    public void setLong(String key, long value) {
        SharedPreferences.Editor editor = getPrefs().edit();
        editor.putLong(key, value);
        editor.apply();
    }

    public boolean getBoolean(String key, boolean defValue) {
        return getPrefs().getBoolean(key, defValue);
    }

    public boolean getBoolean(String key) {
        return getBoolean(key, false);
    }

    public void setBoolean(String key, boolean value) {
        SharedPreferences.Editor editor = getPrefs().edit();
        editor.putBoolean(key, value);
        editor.apply();
    }

    public boolean getFloat(String key) {
        return getFloat(key, 0f);
    }

    public boolean getFloat(String key, float defValue) {
        return getFloat(key, defValue);
    }

    public void setFloat(String key, Float value) {
        SharedPreferences.Editor editor = getPrefs().edit();
        editor.putFloat(key, value);
        editor.apply();
    }

    /**
     * Persists an Object in prefs at the specified key, class of given Object must implement Model
     * interface
     *
     * @param key         String
     * @param modelObject Object to persist
     * @param <M>         Generic for Object
     */
    public <M extends Model> void setObject(String key, M modelObject) {
        String value = createJSONStringFromObject(modelObject);
        SharedPreferences.Editor editor = getPrefs().edit();
        editor.putString(key, value);
        editor.apply();
    }

    /**
     * Fetches the previously stored Object of given Class from prefs
     *
     * @param key                String
     * @param classOfModelObject Class of persisted Object
     * @param <M>                Generic for Object
     * @return Object of given class
     */
    public <M extends Model> M getObject(String key, Class<M> classOfModelObject) {
        String jsonData = getPrefs().getString(key, null);
        if (null != jsonData) {
            try {
                Gson gson = new Gson();
                M customObject = gson.fromJson(jsonData, classOfModelObject);
                return customObject;
            } catch (ClassCastException cce) {
                Log.d(TAG, "Cannot convert string obtained from prefs into collection of type " +
                        classOfModelObject.getName() + "\n" + cce.getMessage());
            }
        }
        return null;
    }

    /**
     * Persists a Collection object in prefs at the specified key
     *
     * @param key            String
     * @param dataCollection Collection Object
     * @param <C>            Generic for Collection object
     */
    public <C> void setCollection(String key, C dataCollection) {
        SharedPreferences.Editor editor = getPrefs().edit();
        String value = createJSONStringFromObject(dataCollection);
        editor.putString(key, value);
        editor.apply();
    }

    /**
     * Fetches the previously stored Collection Object of given type from prefs
     *
     * @param key     String
     * @param typeOfC Type of Collection Object
     * @param <C>     Generic for Collection Object
     * @return Collection Object which can be casted
     */
    public <C> C getCollection(String key, Type typeOfC) {
        String jsonData = getPrefs().getString(key, null);
        if (null != jsonData) {
            try {
                Gson gson = new Gson();
                C arrFromPrefs = gson.fromJson(jsonData, typeOfC);
                return arrFromPrefs;
            } catch (ClassCastException cce) {
                Log.d(TAG, "Cannot convert string obtained from prefs into collection of type " +
                        typeOfC.toString() + "\n" + cce.getMessage());
            }
        }
        return null;
    }

    public void registerPrefsListener(SharedPreferences.OnSharedPreferenceChangeListener listener) {
        getPrefs().registerOnSharedPreferenceChangeListener(listener);
    }

    public void unregisterPrefsListener(

            SharedPreferences.OnSharedPreferenceChangeListener listener) {
        getPrefs().unregisterOnSharedPreferenceChangeListener(listener);
    }

    public SharedPreferences.Editor getEditor() {
        return getPrefs().edit();
    }

    private static String createJSONStringFromObject(Object object) {
        Gson gson = new Gson();
        return gson.toJson(object);
    } 
}

```

`Model` `interface` which is implemented by classes going to `Gson` to avoid
proguard obfuscation.

```java
public interface Model {
    
}

```

Proguard rules for `Model` interface:

```java
-keep interface com.example.app.Model
-keep class * implements com.example.app.Model { *;}

```



## Listening for SharedPreferences changes


```java
SharedPreferences sharedPreferences = ...;
sharedPreferences.registerOnSharedPreferenceChangeListener(mOnSharedPreferenceChangeListener);


private final SharedPreferences.OnSharedPreferenceChangeListener mOnSharedPreferenceChangeListener = new SharedPreferences.OnSharedPreferenceChangeListener() {
    @Override
    public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
        //TODO
    }
}

```

Please note:

- The listener will fire only if value was added or changed, setting the same value won't call it;
- The listener needs to be saved in a member variable and **NOT** with an anonymous class, because `registerOnSharedPreferenceChangeListener` stores it with a weak reference, so it would be garbage collected;
- Instead of using a member variable, it can also be directly implemented by the class and then call `registerOnSharedPreferenceChangeListener(this);`
- Remember to unregister the listener when it is no more required using `unregisterOnSharedPreferenceChangeListener`.



## getPreferences(int) VS getSharedPreferences(String, int)


`getPreferences(int)`

returns the preferences saved by `Activity's class name` as described in the [docs](https://developer.android.com/reference/android/app/Activity.html#getPreferences%28int%29) :

> 
Retrieve a SharedPreferences object for accessing preferences that are private to this activity. This simply calls the underlying getSharedPreferences(String, int) method by passing in this activity's class name as the preferences name.


While using [getSharedPreferences (String name, int mode)](https://developer.android.com/reference/android/content/ContextWrapper.html#getSharedPreferences(java.lang.String,%20int)) method returns the prefs saved under the given `name`. As in the docs :

> 
Retrieve and hold the contents of the preferences file 'name', returning a SharedPreferences through which you can retrieve and modify its values.


So if the value being saved in the `SharedPreferences` has to be used across the app, one should use `getSharedPreferences (String name, int mode)` with a fixed name. As, using `getPreferences(int)` returns/saves the preferences belonging to the `Activity`  calling it.



## Store, Retrieve, Remove and Clear Data from SharedPreferences


Create SharedPreferences BuyyaPref

```java
SharedPreferences pref = getApplicationContext().getSharedPreferences("BuyyaPref", MODE_PRIVATE); 
Editor editor = pref.edit();

```

Storing data as KEY/VALUE pair

```java
editor.putBoolean("key_name1", true);           // Saving boolean - true/false
editor.putInt("key_name2", 10);        // Saving integer
editor.putFloat("key_name3", 10.1f);    // Saving float
editor.putLong("key_name4", 1000);      // Saving long
editor.putString("key_name5", "MyString");  // Saving string
 
// Save the changes in SharedPreferences
editor.commit(); // commit changes

```

Get SharedPreferences data

If value for key not exist then return second param value(In this case null, this is like default value)

```java
pref.getBoolean("key_name1", null);         // getting boolean
pref.getInt("key_name2", null);             // getting Integer
pref.getFloat("key_name3", null);           // getting Float
pref.getLong("key_name4", null);            // getting Long
pref.getString("key_name5", null);          // getting String

```

Deleting Key value from SharedPreferences

```java
editor.remove("key_name3"); // will delete key key_name3
editor.remove("key_name4"); // will delete key key_name4
 
// Save the changes in SharedPreferences
editor.commit(); // commit changes

```

Clear all data from SharedPreferences

```

editor.clear();
 editor.commit(); // commit changes

```



## Removing keys


```java
private static final String MY_PREF = "MyPref";

// ...

SharedPreferences prefs = ...;

// ...

SharedPreferences.Editor editor = prefs.edit();
editor.putString(MY_PREF, "value");
editor.remove(MY_PREF);
editor.apply();

```

After the `apply()`, `prefs` contains "key" -> "value", in addition to whatever it contained already. Even though it looks like I added "key" and then removed it, the remove actually happens first. The changes in the `Editor` are all applied in one go, not in the order you added them. All removes happen before all puts.



## Different ways of instantiating an object of SharedPreferences


You can access SharedPreferences in several ways:

Get the default SharedPreferences file:

```java
import android.preference.PreferenceManager;
SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);

```

Get a specific SharedPreferences file:

```java
public static final String PREF_FILE_NAME = "PrefFile";
SharedPreferences prefs = getSharedPreferences(PREF_FILE_NAME, MODE_PRIVATE);

```

Get SharedPreferences from another app:

```java
// Note that the other app must declare prefs as MODE_WORLD_WRITEABLE
final ArrayList<HashMap<String,String>> LIST = new ArrayList<HashMap<String,String>>();
Context contextOtherApp = createPackageContext("com.otherapp", Context.MODE_WORLD_WRITEABLE);
SharedPreferences prefs = contextOtherApp.getSharedPreferences("pref_file_name", Context.MODE_WORLD_READABLE); 

```



## Supported data types in SharedPreferences


`SharedPreferences` allows you to store primitive data types only (`boolean`, `float`, `long`, `int`, `String`, and `string set`). You cannot store more complex objects in `SharedPreferences`, and as such is really meant to be a place to store user settings or similar, it's not meant to be a database to keep user data (like saving a todo list a user made for example).

To store something in `SharedPreferences` you use a Key and a Value. The Key is how you can reference what you stored later and the Value data you want to store.

```

   String keyToUseToFindLater = "High Score";
    int newHighScore = 12938;
    //getting SharedPreferences & Editor objects 
    SharedPreferences sharedPref = getActivity().getPreferences(Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = sharedPref.edit();
    //saving an int in the SharedPreferences file
    editor.putInt(keyToUseToFindLater, newHighScore);
    editor.commit();

```



## Add filter for EditTextPreference


**Create this class :**

```java
public class InputFilterMinMax implements InputFilter {

    private int min, max;

    public InputFilterMinMax(int min, int max) {
        this.min = min;
        this.max = max;
    }

    public InputFilterMinMax(String min, String max) {
        this.min = Integer.parseInt(min);
        this.max = Integer.parseInt(max);
    }

    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        try {
            int input = Integer.parseInt(dest.toString() + source.toString());
            if (isInRange(min, max, input))
                return null;
        } catch (NumberFormatException nfe) { }
        return "";
    }

    private boolean isInRange(int a, int b, int c) {
        return b > a ? c >= a && c <= b : c >= b && c <= a;
    }
}

```

**Use :**

```java
EditText compressPic = ((EditTextPreference) findPreference(getString("pref_key_compress_pic"))).getEditText();
compressPic.setFilters(new InputFilter[]{ new InputFilterMinMax(1, 100) });

```



## Support pre-Honeycomb with StringSet


Here's the utility class:

```java
public class SharedPreferencesCompat {

    public static void putStringSet(SharedPreferences.Editor editor, String key, Set<String> values) {
            if (Build.VERSION.SDK_INT >= 11) {
                while (true) {
                    try {
                        editor.putStringSet(key, values).apply();
                        break;
                    } catch (ClassCastException ex) {
                        // Clear stale JSON string from before system upgrade
                        editor.remove(key);
                    }
                }
            } else putStringSetToJson(editor, key, values);
    }

    public static Set<String> getStringSet(SharedPreferences prefs, String key, Set<String> defaultReturnValue) {
        if (Build.VERSION.SDK_INT >= 11) {
            try {
                return prefs.getStringSet(key, defaultReturnValue);
            } catch (ClassCastException ex) {
                // If user upgraded from Gingerbread to something higher read the stale JSON string
                return getStringSetFromJson(prefs, key, defaultReturnValue);
            }
        } else return getStringSetFromJson(prefs, key, defaultReturnValue);
    }

    private static Set<String> getStringSetFromJson(SharedPreferences prefs, String key, Set<String> defaultReturnValue) {
        final String input = prefs.getString(key, null);
        if (input == null) return defaultReturnValue;

        try {
            HashSet<String> set = new HashSet<>();
            JSONArray json = new JSONArray(input);
            for (int i = 0, size = json.length(); i < size; i++) {
                String value = json.getString(i);
                set.add(value);
            }
            return set;
        } catch (JSONException e) {
            e.printStackTrace();
            return defaultReturnValue;
        }
    }

    private static void putStringSetToJson(SharedPreferences.Editor editor, String key, Set<String> values) {
        JSONArray json = new JSONArray(values);
        if (Build.VERSION.SDK_INT >= 9)
            editor.putString(key, json.toString()).apply();
        else
            editor.putString(key, json.toString()).commit();
    }

    private SharedPreferencesCompat() {}
}

```

An example to save preferences as StringSet data type is:

```java
Set<String> sets = new HashSet<>();
sets.add("John");
sets.add("Nicko");
SharedPreferences preferences = PreferenceManager.getDefaultSharedPreferences(this);
SharedPreferencesCompat.putStringSet(preferences.edit(), "pref_people", sets);

```

To retrieve them back:

```java
Set<String> people = SharedPreferencesCompat.getStringSet(preferences, "pref_people", new HashSet<String>());

```

Reference: [Android Support Preference](https://github.com/consp1racy/android-support-preference)



#### Syntax


<li>
**Context Method**
<ul>
- public SharedPreferences getSharedPreferences(String name, int mode)

**Activity Method**

- public SharedPreferences getPreferences()

**SharedPreferences Methods**

- public SharedPreferences.Editor edit()
- public boolean contains()
- public Map<String, ?> getAll()
<li>public boolean getBoolean(String key,
boolean defValue)</li>
<li>public float getFloat(String key,
float defValue)</li>
<li>public int getInt(String key,
int defValue)</li>
<li>public long getLong(String key,
long defValue)</li>
<li>public String getString(String key,
String defValue)</li>
<li>public Set getStringSet(String key,
Set defValues)</li>
- public void registerOnSharedPreferenceChangeListener (SharedPreferences.OnSharedPreferenceChangeListener listener)
- public void unregisterOnSharedPreferenceChangeListener (SharedPreferences.OnSharedPreferenceChangeListener listener)

**SharedPreferences.Editor Methods**

- public void apply()
- public boolean commit()
- public SharedPreferences.Editor clear()
<li>public SharedPreferences.Editor putBoolean (String key,
boolean value)</li>
<li>public SharedPreferences.Editor putFloat (String key,
float value)</li>
<li>public SharedPreferences.Editor putInt (String key,
int value)</li>
<li>public SharedPreferences.Editor putLong (String key,
long value)</li>
<li>public SharedPreferences.Editor putString (String key,
String value)</li>
<li>public SharedPreferences.Editor putStringSet (String key,
Set values)</li>
- public SharedPreferences.Editor remove (String key)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|key|A non-null `String` identifying the parameter. It can contain whitespace or non-printables. This is only used inside your app (and in the XML file), so it doesn't have to be namespaced, but it's a good idea to have it as a constant in your source code. Don't localize it.
|defValue|All the get functions take a default value, which is returned if the given key is not present in the `SharedPreferences`. It's not returned if the key is present but the value has the wrong type: in that case you get a `ClassCastException`.



#### Remarks


<li>
`SharedPreferences` shouldn't be used for storing large amount of data. For such purposes, it's much better to use `SQLiteDatabase`.
</li>
<li>
<p>`SharedPreferences` are single process only, unless you use deprecated mode `MODE_MULTI_PROCESS`. So if your app has multiple processes, you won't be able to read main process's `SharedPreferences` in another process.
In such cases, you should use another mechanism to share data across processes, but don't use `MODE_MULTI_PROCESS` as it is not reliable as well as deprecated.</p>
</li>
<li>
It's better to use `SharedPreferences` instance in `Singleton` class to access all over the Application `context`. If you want to use it only for particular Activity go for `getPreferences()`.
</li>
<li>
Avoid storing sensitive information in clear text while using `SharedPreferences` since it can be read easily.
</li>

### Official Documentation

[https://developer.android.com/reference/android/content/SharedPreferences.html](https://developer.android.com/reference/android/content/SharedPreferences.html)

