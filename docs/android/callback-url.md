---
metaTitle: "Android - Callback URL"
description: "Callback URL example with Instagram OAuth"
---

# Callback URL



## Callback URL example with Instagram OAuth


One of the use cases of **callback URLs** is OAuth. Let us do this with an Instagram Login: If the user enters their credentials and clicks the **Login** button, Instagram will validate the credentials and return an `access_token`. We need that `access_token` in our app.

For our app to be able to listen to such links, we need to add a callback URL to our `Activity`. We can do this by adding an `<intent-filter/>` to our `Activity`, which will react to that callback URL. Assume that our callback URL is `appSchema://appName.com`. Then you have to add the following lines to your desired `Activity` in the **Manifest.xml** file:

```java
<action android:name="android.intent.action.VIEW" />
<category android:name="android.intent.category.BROWSABLE"/>
<data android:host="appName.com" android:scheme="appSchema"/> 

```

Explanation of the lines above:

- `<category android:name="android.intent.category.BROWSABLE"/>` makes the target activity allow itself to be started by a web browser to display data referenced by a link.
- `<data android:host="appName.com" android:scheme="appSchema"/>` specifies our schema and host of our callback URL.
- All together, these lines will cause the specific `Activity` to be opened whenever the callback URL is called in a browser.

Now, in order to get the contents of the URL in your `Activity`, you need to override the `onResume()` method as follows:

```java
@Override 
public void onResume() { 
    // The following line will return "appSchema://appName.com".
    String CALLBACK_URL = getResources().getString(R.string.insta_callback);
    Uri uri = getIntent().getData();
    if (uri != null && uri.toString().startsWith(CALLBACK_URL)) {
        String access_token = uri.getQueryParameter("access_token");
    }
    // Perform other operations here.
} 

```

Now you have retrieved the `access_token` from Instagram, that is used in various API endpoints of Instagram.

