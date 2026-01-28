---
metaTitle: "Android - Handling Deep Links"
description: "Retrieving query parameters, Simple deep link, Multiple paths on a single domain, Multiple domains and multiple paths, Both http and https for the same domain, Using pathPrefix"
---

# Handling Deep Links


Deep links are URLs that take users directly to specific content in your app. You can set up deep links by adding intent filters and extracting data from incoming intents to drive users to the right screen in your app.



## Retrieving query parameters


```java
public class MainActivity extends Activity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
    
        Intent intent = getIntent();
        Uri data = intent.getData();

        if (data != null) {
            String param1 = data.getQueryParameter("param1");
            String param2 = data.getQueryParameter("param2");
        }
    }

}

```

If the user clicks on a linkto `http://www.example.com/map?param1=FOO&param2=BAR`, then `param1` here will have a value of `"FOO"` and `param2` will have a value of `"BAR"`.



## Simple deep link


**AndroidManifest.xml:**

```java
<activity android:name="com.example.MainActivity" >
    
    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />

        <data android:scheme="http"
              android:host="www.example.com" />

    </intent-filter>

</activity>

```

This will accept any link starting with `http://www.example.com` as a deep link to start your `MainActivity`.



## Multiple paths on a single domain


**AndroidManifest.xml:**

```java
<activity android:name="com.example.MainActivity" >
    
    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />

        <data android:scheme="http"
              android:host="www.example.com" />

        <data android:path="/" />
        <data android:path="/about" />
        <data android:path="/map" />

    </intent-filter>

</activity>

```

This will launch your `MainActivity` when the user clicks any of these links:

- `http://www.example.com/`
- `http://www.example.com/about`
- `http://www.example.com/map`



## Multiple domains and multiple paths


**AndroidManifest.xml:**

```java
<activity android:name="com.example.MainActivity" >

    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
    
        <data android:scheme="http"
              android:host="www.example.com" />
    
        <data android:scheme="http"
              android:host="www.example2.com" />
    
        <data android:path="/" />
        <data android:path="/map" />
    
    </intent-filter>

</activity>

```

This will launch your MainActivity when the user clicks any of these links:

- `http://www.example.com/`
- `http://www.example2.com/`
- `http://www.example.com/map`
- `http://www.example2.com/map`



## Both http and https for the same domain


**AndroidManifest.xml:**

```java
<activity android:name="com.example.MainActivity" >

    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
    
        <data android:scheme="http" />
        <data android:scheme="https" />
    
        <data android:host="www.example.com" />
    
        <data android:path="/" />
        <data android:path="/map" />
    
    </intent-filter>

</activity>

```

This will launch your MainActivity when the user clicks any of these links:

- `http://www.example.com/`
- `https://www.example.com/`
- `http://www.example.com/map`
- `https://www.example.com/map`



## Using pathPrefix


**AndroidManifest.xml:**

```java
<activity android:name="com.example.MainActivity" >

    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
    
        <data android:scheme="http"
              android:host="www.example.com"
              android:path="/item" />
    
    </intent-filter>

</activity>

```

This will launch your MainActivity when the user clicks any link starting with `http://www.example.com/item`, such as:

- `https://www.example.com/item`
- `http://www.example.com/item/1234`
- `https://www.example.com/item/xyz/details`



#### Parameters


|`<data>` Attribute|Details
|---|---|---|---|---|---|---|---|---|---
|scheme|The **scheme** part of a URI (case-sensitive). Examples: `http`, `https`, `ftp`
|host|The **host** part of a URI (case-sensitive). Examples: `google.com`, `example.org`
|port|The **port** part of a URI. Examples: `80`, `443`
|path|The **path** part of a URI. Must begin with  `/`. Examples:  `/`, `/about`
|pathPrefix|A prefix for the **path** part of a URI. Examples: `/item`, `/article`
|pathPattern|A pattern to match for the **path** part of a URI. Examples: `/item/.*`, `/article/[0-9]*`
|mimeType|A mime type to match. Examples: `image/jpeg`, `audio/*`



#### Remarks


### The `<intent-filter>`

This combination of `<action>` and `<category>` elements is what tells the Android system that a specific Activity should be launched when the user clicks on a link in another application.

```java
<intent-filter>
    <action android:name="android.intent.action.VIEW" />
    <category android:name="android.intent.category.DEFAULT" />
    <category android:name="android.intent.category.BROWSABLE" />

    <data ... />

</intent-filter>

```

### Multiple `<data>` tags

The set of deep links that your `<intent-filter>` supports is the cross-product of all the `<data>` elements that you define in that intent-filter. The multiple domain, multiple path, and multiple scheme examples demonstrate this.

### Resources

- [Enabling Deep Links for App Content](https://developer.android.com/training/app-indexing/deep-linking.html) (developer.android.com)
- [`<intent-filter>`](https://developer.android.com/guide/topics/manifest/intent-filter-element.html) (developer.android.com

