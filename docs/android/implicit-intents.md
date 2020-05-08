---
metaTitle: "Android - Implicit Intents"
description: "Implicit and Explicit Intents, Implicit Intents"
---

# Implicit Intents



## Implicit and Explicit Intents


An explicit intent is used for starting an activity or service within the same application package. In this case the name of the intended class is explicitly mentioned:

```java
Intent intent = new Intent(this, MyComponent.class);
startActivity(intent);

```

However, an implicit intent is sent across the system for any application installed on the user's device that can handle that intent. This is used to share information between different applications.

```java
Intent intent = new Intent("com.stackoverflow.example.VIEW");

//We need to check to see if there is an application installed that can handle this intent
if (getPackageManager().resolveActivity(intent, 0) != null){ 
    startActivity(intent);
}else{
    //Handle error
}

```

More details on the differences can be found in the Android Developer docs here: [Intent Resolution](https://developer.android.com/reference/android/content/Intent.html#IntentResolution)



## Implicit Intents


[Implicit](https://developer.android.com/guide/components/intents-filters.html#Types) intents do not name a specific component, but instead declare a general action to perform, which allows a component from another app to handle it.

For example, if you want to show the user a location on a map, you can use an implicit intent to request that another capable app show a specified location on a map.

**Example:**

```java
// Create the text message with a string
Intent sendIntent = new Intent();
sendIntent.setAction(Intent.ACTION_SEND);
sendIntent.putExtra(Intent.EXTRA_TEXT, textMessage);
sendIntent.setType("text/plain");

// Verify that the intent will resolve to an activity
if (sendIntent.resolveActivity(getPackageManager()) != null) {
    startActivity(sendIntent);
}

```



#### Syntax


- Intent()
- Intent (Intent o)
- Intent (String action)
- Intent (String action, Uri uri)
- Intent (Context packageContext, Class<?> cls)
<li>Intent (String action,
Uri uri,
Context packageContext,
Class<?> cls)</li>



#### Parameters


|Parameters|Details
|---|---|---|---|---|---|---|---|---|---
|o|Intent
|action|`String:` The Intent action, such as `ACTION_VIEW.`
|uri|`Uri:` The Intent data URI.
|packageContext|`Context:` A Context of the application package implementing this class.
|cls|`Class:` The component class that is to be used for the intent.



#### Remarks


<li>
More about [Intent](https://developer.android.com/reference/android/content/Intent.html)
</li>
<li>
More about [Intent Types](https://developer.android.com/guide/components/intents-filters.html#Types)
</li>

