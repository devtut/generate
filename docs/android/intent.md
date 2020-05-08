---
metaTitle: "Android - Intent"
description: "Getting a result from another Activity, Passing data between activities, Open a URL in a browser, Starter Pattern, Clearing an activity stack, Start an activity, Sending emails, CustomTabsIntent for Chrome Custom Tabs, Intent URI, Start the dialer, Broadcasting Messages to Other Components, Passing custom object between activities, Share intent, Open Google map with specified latitude, longitude, Passing different data through Intent  in Activity, Showing a File Chooser and Reading the Result, Sharing Multiple Files through Intent, Start Unbound Service using an Intent, Getting a result from Activity to Fragment"
---

# Intent


An Intent is a small message passed around the Android system. This message may hold information about our intention to perform a task.

It is basically a passive data structure holding an abstract description of an action to be performed.



## Getting a result from another Activity


By using [`startActivityForResult(Intent intent, int requestCode)`](https://developer.android.com/reference/android/app/Activity.html#startActivityForResult(android.content.Intent,%20int)) you can start another [`Activity`](https://developer.android.com/reference/android/app/Activity.html) and then receive a result from that `Activity` in the [`onActivityResult(int requestCode, int resultCode, Intent data)`](https://developer.android.com/reference/android/app/Activity.html#onActivityResult(int,%20int,%20android.content.Intent)) method. The result will be returned as an [`Intent`](https://developer.android.com/reference/android/content/Intent.html). An intent can contain data via a Bundle

In this example `MainActivity` will start a `DetailActivity` and then expect a result from it.  Each request type should have its own `int` request code, so that in the **overridden** `onActivityResult(int requestCode, int resultCode, Intent data)` method in `MainActivity` , it can be determined which request to process by comparing values of `requestCode` and  `REQUEST_CODE_EXAMPLE` (though in this example, there is only one).

### **MainActivity:**

```java
public class MainActivity extends Activity {

    // Use a unique request code for each use case 
    private static final int REQUEST_CODE_EXAMPLE = 0x9345; 

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Create a new instance of Intent to start DetailActivity
        final Intent intent = new Intent(this, DetailActivity.class);

        // Start DetailActivity with the request code
        startActivityForResult(intent, REQUEST_CODE_EXAMPLE);
    }

    // onActivityResult only get called 
    // when the other Activity previously started using startActivityForResult
    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        // First we need to check if the requestCode matches the one we used.
        if(requestCode == REQUEST_CODE_EXAMPLE) {

            // The resultCode is set by the DetailActivity
            // By convention RESULT_OK means that whatever
            // DetailActivity did was executed successfully
            if(resultCode == Activity.RESULT_OK) {
                // Get the result from the returned Intent
                final String result = data.getStringExtra(DetailActivity.EXTRA_DATA);

                // Use the data - in this case, display it in a Toast.
                Toast.makeText(this, "Result: " + result, Toast.LENGTH_LONG).show();
            } else {
                // setResult wasn't successfully executed by DetailActivity 
                // Due to some error or flow of control. No data to retrieve.
            }
        }
    }
}

```

### **DetailActivity:**

```java
public class DetailActivity extends Activity {

    // Constant used to identify data sent between Activities.
    public static final String EXTRA_DATA = "EXTRA_DATA";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_detail);

        final Button button = (Button) findViewById(R.id.button);
        // When this button is clicked we want to return a result
        button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                // Create a new Intent object as container for the result
                final Intent data = new Intent();

                // Add the required data to be returned to the MainActivity
                data.putExtra(EXTRA_DATA, "Some interesting data!");
                
                // Set the resultCode as Activity.RESULT_OK to 
                // indicate a success and attach the Intent
                // which contains our result data
                setResult(Activity.RESULT_OK, data); 

                // With finish() we close the DetailActivity to 
                // return back to MainActivity
                finish();
            }
        });
    }

    @Override
    public void onBackPressed() {
        // When the user hits the back button set the resultCode 
        // as Activity.RESULT_CANCELED to indicate a failure
        setResult(Activity.RESULT_CANCELED);
        super.onBackPressed();
    }
}

```

### A few things you need to be aware of:

<li>
Data is only returned once you call `finish()`. You need to call `setResult()` before calling `finish()`, otherwise, no result will be returned.
</li>
<li>
Make sure your `Activity` is not using `android:launchMode="singleTask"`, or it will cause the `Activity` to run in a separate task and therefore you will not receive a result from it. If your `Activity` uses `singleTask` as launch mode, it will call `onActivityResult()` immediately with a result code of `Activity.RESULT_CANCELED`.
</li>
<li>
Be careful when using `android:launchMode="singleInstance"`. On devices before Lollipop (Android 5.0, API Level 21), Activities will not return a result.
</li>

<li>You can use [explicit](https://developer.android.com/guide/components/intents-filters.html#ExampleExplicit) or [implicit](https://developer.android.com/guide/components/intents-filters.html#ExampleSend) intents when you call `startActivityForResult()`. When starting one of your own activities to receive a result, you should use an explicit intent to ensure that you receive the expected result.
An explicit `intent` is always delivered to its target, no matter what it contains; the `filter` is not consulted. But an implicit intent is delivered to a component only if it can pass through one of the component's filters.</li>



## Passing data between activities


This example illustrates sending a `String` with value as `"Some data!"` from `OriginActivity` to `DestinationActivity`.

**NOTE:** This is the most straightforward way of sending data between two activities. See the example on using the [starter pattern](http://stackoverflow.com/documentation/android/103/intent/5357/starter-pattern) for a more robust implementation.

### OriginActivity

```java
public class OriginActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_origin);
        
        // Create a new Intent object, containing DestinationActivity as target Activity.
        final Intent intent = new Intent(this, DestinationActivity.class);

        // Add data in the form of key/value pairs to the intent object by using putExtra()
        intent.putExtra(DestinationActivity.EXTRA_DATA, "Some data!");

        // Start the target Activity with the intent object
        startActivity(intent);
    }
} 

```

### DestinationActivity

```java
public class DestinationActivity extends AppCompatActivity {

    public static final String EXTRA_DATA = "EXTRA_DATA";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_destination);

        // getIntent() returns the Intent object which was used to start this Activity
        final Intent intent = getIntent();

        // Retrieve the data from the intent object by using the same key that
        // was previously used to add data to the intent object in OriginActivity.
        final String data = intent.getStringExtra(EXTRA_DATA);
    }
}

```

It is also possible to pass other `primitive` data types as well as `arrays`, [`Bundle`](https://developer.android.com/reference/android/os/Bundle.html) and [`Parcelable`](https://developer.android.com/reference/android/os/Parcelable.html) data. Passing [`Serializable`](https://docs.oracle.com/javase/8/docs/api/java/io/Serializable.html) is also possible, but should be avoided as it is more than three times slower than `Parcelable`.

**Serializable** is a standard Java `interface`.  You simply mark a class as `Serializable` by implementing the `Serializable` `interface` and Java will automatically serialize it during required situations.

**Parcelable** is an Android specific `interface` which can be implemented on custom data types (i.e. your own objects / POJO objects ), it allows your object to be flattened and reconstruct itself without the destination needing to do anything. There is a documentation example of [making an object parcelable](http://stackoverflow.com/documentation/android/1849/parcelable/6057/making-a-custom-object-parcelable#t=201608030741494643781).

Once you have a `parcelable` object you can send it like a primitive type, with an intent object:

```java
intent.putExtra(DestinationActivity.EXTRA_DATA, myParcelableObject);

```

Or in a bundle / as an argument for a fragment:

```java
bundle.putParcelable(DestinationActivity.EXTRA_DATA, myParcelableObject);

```

and then also read it from the intent at the destination using getParcelableExtra:

```java
final MyParcelableType data = intent.getParcelableExtra(EXTRA_DATA); 

```

Or when reading in a fragment from a bundle:

```java
final MyParcelableType data = bundle.getParcelable(EXTRA_DATA); 

```

Once you have a `Serializable` object you can put it in an intent object:

```java
bundle.putSerializable(DestinationActivity.EXTRA_DATA, mySerializableObject);

```

and then also read it from the intent object at the destination as shown below:

```java
final SerializableType data = (SerializableType)bundle.getSerializable(EXTRA_DATA); 

```



## Open a URL in a browser


### Opening with the default browser

This example shows how you can open a URL programmatically in the built-in web browser rather than within your application.  This allows your app to open up a webpage without the need to include the `INTERNET` permission in your manifest file.

```java
public void onBrowseClick(View v) {
    String url = "http://www.google.com";
    Uri uri = Uri.parse(url);
    Intent intent = new Intent(Intent.ACTION_VIEW, uri);
    // Verify that the intent will resolve to an activity
    if (intent.resolveActivity(getPackageManager()) != null) {
        // Here we use an intent without a Chooser unlike the next example
        startActivity(intent);
    } 
}

```

### Prompting the user to select a browser

Note that this example uses the [`Intent.createChooser()`](https://developer.android.com/reference/android/content/Intent.html#createChooser(android.content.Intent,%20java.lang.CharSequence)) method:

```java
public void onBrowseClick(View v) {
    String url = "http://www.google.com";
    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
    // Note the Chooser below. If no applications match, 
    // Android displays a system message.So here there is no need for try-catch.
    startActivity(Intent.createChooser(intent, "Browse with"));
   
}

```

In some cases, the URL may start with **"www"**. If that is the case you will get this exception:

> 
`android.content.ActivityNotFoundException` : No Activity found to handle Intent


The URL must always start with **"http://"** or **"https://"**. Your code should therefore check for it, as shown in the following code snippet:

```java
if (!url.startsWith("https://") && !url.startsWith("http://")){
    url = "http://" + url;
}
Intent openUrlIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
if (openUrlIntent.resolveActivity(getPackageManager()) != null) {
    startActivity(openUrlIntent);
} 

```

### **Best Practices**

Check if there are no apps on the device that can receive the implicit intent.  Otherwise, your app will crash when it calls `startActivity()`. To first verify that an app exists to receive the intent, call `resolveActivity()` on your Intent object. If the result is non-null, there is at least one app that can handle the intent and it's safe to call `startActivity()`. If the result is null, you should not use the intent and, if possible, you should disable the feature that invokes the intent.



## Starter Pattern


This pattern is a more strict approach to starting an `Activity`. Its purpose is to improve code readability, while at the same time decrease code complexity, maintenance costs, and coupling of your components.

The following example implements the starter pattern, which is usually implemented as a static method on the `Activity` itself. This static method accepts all required parameters, constructs a valid `Intent` from that data, and then starts the `Activity`.

An `Intent` is an object that provides runtime binding between separate components, such as two activities. The Intent represents an app’s "intent to do something." You can use intents for a wide variety of tasks, but here, your intent starts another activity.

```java
public class ExampleActivity extends AppCompatActivity {

    private static final String EXTRA_DATA = "EXTRA_DATA";

    public static void start(Context context, String data) {
        Intent intent = new Intent(context, ExampleActivity.class);
        intent.putExtra(EXTRA_DATA, data);
        context.startActivity(intent);
    }       

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Intent intent = getIntent();
        if(!intent.getExtras().containsKey(EXTRA_DATA)){
            throw new UnsupportedOperationException("Activity should be started using the static start method");
        }
        String data = intent.getStringExtra(EXTRA_DATA);
    }
}

```

This pattern also allows you to force additional data to be passed with the intent.

The `ExampleActivity` can then be started like this, where `context` is an activity context:

```java
ExampleActivity.start(context, "Some data!");

```



## Clearing an activity stack


Sometimes you may want to start a new activity while removing previous activities from the back stack, so the back button doesn't take you back to them. One example of this might be starting an app on the Login activity, taking you through to the Main activity of your application, but on logging out you want to be directed back to Login without a chance to go back. In a case like that you can set the `FLAG_ACTIVITY_CLEAR_TOP` flag for the intent, meaning if the activity being launched is already running in the current task (LoginActivity), then instead of launching a new instance of that activity, all of the other activities on top of it will be closed and this Intent will be delivered to the (now on top) old activity as a new Intent.

```java
Intent intent = new Intent(getApplicationContext(), LoginActivity.class);
intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
startActivity(intent);

```

It's also possible to use the flags `FLAG_ACTIVITY_NEW_TASK` along with `FLAG_ACTIVITY_CLEAR_TASK` if you want to clear all Activities on the back stack:

```java
Intent intent = new Intent(getApplicationContext(), LoginActivity.class);
// Closing all the Activities, clear the back stack.
intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
startActivity(intent);

```



## Start an activity


This example will start `DestinationActivity` from `OriginActivity`.

Here, the `Intent` constructor takes two parameters:

1. A Context as its first parameter (this is used because the Activity class is a subclass of Context)
1. The Class of the app component to which the system should deliver the Intent (in this case, the activity that should be started)

```java
public class OriginActivity extends AppCompatActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_origin);

        Intent intent = new Intent(this, DestinationActivity.class);
        
        startActivity(intent);
        finish(); // Optionally, you can close OriginActivity. In this way when the user press back from DestinationActivity he/she won't land on OriginActivity again.
    }
}

```

Another way to create the `Intent` to open `DestinationActivity` is to use the default constructor for the `Intent`, and use the `setClass()` method to tell it which Activity to open:

```java
Intent i=new Intent();
i.setClass(this, DestinationActivity.class);
startActivity(intent);
finish(); // Optionally, you can close OriginActivity. In this way when the user press back from DestinationActivity he/she won't land on OriginActivity 

```



## Sending emails


```java
// Compile a Uri with the 'mailto' schema
Intent emailIntent = new Intent(Intent.ACTION_SENDTO, Uri.fromParts(
        "mailto","johndoe@example.com", null));
// Subject
emailIntent.putExtra(Intent.EXTRA_SUBJECT, "Hello World!");
// Body of email
emailIntent.putExtra(Intent.EXTRA_TEXT, "Hi! I am sending you a test email.");
// File attachment
emailIntent.putExtra(Intent.EXTRA_STREAM, attachedFileUri);

// Check if the device has an email client
if (emailIntent.resolveActivity(getPackageManager()) != null) {
     // Prompt the user to select a mail app
     startActivity(Intent.createChooser(emailIntent,"Choose your mail application"));
} else {
    // Inform the user that no email clients are installed or provide an alternative
}

```

This will pre-fill an email in a mail app of the user's choice.

If you need to add an attachment, you can use `Intent.ACTION_SEND` instead of `Intent.ACTION_SENDTO`. For multiple attachments you can use `ACTION_SEND_MULTIPLE`

A word of caution: not every device has a provider for `ACTION_SENDTO`, and calling `startActivity()` without checking with [`resolveActivity()`](https://developer.android.com/guide/components/intents-common.html#Email) first may throw an ActivityNotFoundException.



## CustomTabsIntent for Chrome Custom Tabs


Using a [`CustomTabsIntent`](https://developer.android.com/reference/android/support/customtabs/CustomTabsIntent.html), it is now possible to configure [Chrome custom tabs](https://developer.chrome.com/multidevice/android/customtabs) in order to customize key UI components in the browser that is opened from your app.

This is a good alternative to using a WebView for some cases.  It allows loading of a web page with an Intent, with the added ability to inject some degree of the look and feel of your app into the browser.

Here is an example of how to open a url using `CustomTabsIntent`

```java
String url = "https://www.google.pl/";
CustomTabsIntent intent = new CustomTabsIntent.Builder()
                    .setStartAnimations(getContext(), R.anim.slide_in_right, R.anim.slide_out_left)
                    .setExitAnimations(getContext(), android.R.anim.slide_in_left, android.R.anim.slide_out_right)
                    .setCloseButtonIcon(BitmapFactory.decodeResource(getResources(), R.drawable.ic_arrow_back_white_24dp))
                    .setToolbarColor(Color.parseColor("#43A047"))
                    .enableUrlBarHiding()
                    .build();
            intent.launchUrl(getActivity(), Uri.parse(url));

```

**Note:**

To use custom tabs, you need to add this dependency to your build.gradle

```java
compile 'com.android.support:customtabs:24.1.1'

```



## Intent URI


This example shows, how to start intent from browser:

```java
<a href="intent://host.com/path#Intent;package=com.sample.test;scheme=yourscheme;end">Start intent</a>

```

This intent will start app with package `com.sample.test` or will open google play with this package.

Also this intent can be started with javascript:

```java
var intent = "intent://host.com/path#Intent;package=com.sample.test;scheme=yourscheme;end";
window.location.replace(intent)

```

In activity this host and path can be obtained from intent data:

```java
@Override
public void onCreate(Bundle bundle) {
    super.onCreate(bundle);
    Uri data = getIntent().getData(); // returns host.com/path
}

```

Intent URI syntax:

```java
HOST/URI-path // Optional host
#Intent;
    package=[string];
    action=[string];
    category=[string];
    component=[string];
    scheme=[string];
end;

```



## Start the dialer


This example shows how to open a default dialer (an app that makes regular calls) with a provided telephone number already in place:

```java
Intent intent = new Intent(Intent.ACTION_DIAL);
intent.setData(Uri.parse("tel:9988776655")); //Replace with valid phone number. Remember to add the tel: prefix, otherwise it will crash.
startActivity(intent);

```

Result from running the code above:

[<img src="http://i.stack.imgur.com/DxDqh.png" alt="enter image description here" />](http://i.stack.imgur.com/DxDqh.png)



## Broadcasting Messages to Other Components


Intents can be used to broadcast messages to other components of your application (such as a running background service) or to the entire Android system.

To send a broadcast **within your application**, use the `LocalBroadcastManager` class:

```java
Intent intent = new Intent("com.example.YOUR_ACTION"); // the intent action
intent.putExtra("key", "value"); // data to be passed with your broadcast

LocalBroadcastManager manager = LocalBroadcastManager.getInstance(context);
manager.sendBroadcast(intent);

```

To send a broadcast to components outside of your application, use the `sendBroadcast()` method on a `Context` object.

```java
Intent intent = new Intent("com.example.YOUR_ACTION"); // the intent action
intent.putExtra("key", "value"); // data to be passed with your broadcast

context.sendBroadcast(intent);

```

Information about **receiving** broadcasts can be found here: [Broadcast Receiver](http://stackoverflow.com/documentation/android/1460/broadcast-receiver#t=201607220748559674078)



## Passing custom object between activities


It is also possible to pass your custom object to other activities using the [`Bundle`](https://developer.android.com/reference/android/os/Bundle.html) class.

There are two ways:

- `Serializable` interface—for Java and Android
- `Parcelable` interface—memory efficient, only for Android (recommended)

### Parcelable

Parcelable processing is much faster than serializable. One of the reasons for this is that we are being explicit about the serialization process instead of using reflection to infer it. It also stands to reason that the code has been heavily optimized for this purpose.

```java
public class MyObjects implements Parcelable {
    
    private int age;
    private String name;
    
    private ArrayList<String> address;
    
    public MyObjects(String name, int age, ArrayList<String> address) {
        this.name = name;
        this.age = age;
        this.address = address;
    
    }
    
    public MyObjects(Parcel source) {
        age = source.readInt();
        name = source.readString();
        address = source.createStringArrayList();
    }
    
    @Override
    public int describeContents() {
        return 0;
    }
    
    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeInt(age);
        dest.writeString(name);
        dest.writeStringList(address);
    }
    
    public int getAge() {
        return age;
    }
    
    public String getName() {
        return name;
    }
    
    public ArrayList<String> getAddress() {
        if (!(address == null))
            return address;
        else
            return new ArrayList<String>();
    }
    
    public static final Creator<MyObjects> CREATOR = new Creator<MyObjects>() {
        @Override
        public MyObjects[] newArray(int size) {
            return new MyObjects[size];
        }
    
        @Override
        public MyObjects createFromParcel(Parcel source) {
            return new MyObjects(source);
        }
    };
}

```

**Sending Activity Code**

```java
MyObject mObject = new MyObject("name","age","Address array here");

//Passing MyOject 
Intent mIntent = new Intent(FromActivity.this, ToActivity.class);
mIntent.putExtra("UniqueKey", mObject);
startActivity(mIntent);

```

**Receiving the object in destination activity.**

```java
//Getting MyObjects 
Intent mIntent = getIntent();
MyObjects workorder = (MyObjects) mIntent.getParcelable("UniqueKey");

```

**You can pass Arraylist of Parceble object as below**

```java
//Array of MyObjects
ArrayList<MyObject> mUsers;

//Passing MyObject List
Intent mIntent = new Intent(FromActivity.this, ToActivity.class);
mIntent.putParcelableArrayListExtra("UniqueKey", mUsers);
startActivity(mIntent);

//Getting MyObject List
Intent mIntent = getIntent();
ArrayList<MyObjects> mUsers = mIntent.getParcelableArrayList("UniqueKey");

```

> 
**Note:** There are Android Studio plugins such as [this one](https://github.com/mcharmas/android-parcelable-intellij-plugin) available to generate Parcelable code


### Serializable

Sending Activity Code

```java
Product product = new Product();
Bundle bundle = new Bundle();
bundle.putSerializable("product", product);
Intent cartIntent = new Intent(mContext, ShowCartActivity.class);
cartIntent.putExtras(bundle);
mContext.startActivity(cartIntent);

```

Receiving the object in destination activity.

```java
protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    Bundle bundle = this.getIntent().getExtras();
    Product product = null;
    if (bundle != null) {
        product = (Product) bundle.getSerializable("product");
    }

```

**`Arraylist` of Serializable object: same as single object passing**

Custom object should implement the [`Serializable`](https://developer.android.com/reference/java/io/Serializable.html) interface.



## Share intent


**Share simple information with differents apps.**

```java
Intent sendIntent = new Intent();
sendIntent.setAction(Intent.ACTION_SEND);
sendIntent.putExtra(Intent.EXTRA_TEXT, "This is my text to send.");
sendIntent.setType("text/plain");
startActivity(Intent.createChooser(sendIntent, getResources().getText(R.string.send_to)));

```

**Share an image with differents apps.**

```java
Intent shareIntent = new Intent();
shareIntent.setAction(Intent.ACTION_SEND);
shareIntent.putExtra(Intent.EXTRA_STREAM, uriToImage);
shareIntent.setType("image/jpeg");
startActivity(Intent.createChooser(shareIntent, getResources().getText(R.string.send_to)));

```



## Open Google map with specified latitude, longitude


You can pass latitude, longitude from your app to Google map using Intent

```java
String uri = String.format(Locale.ENGLISH, "http://maps.google.com/maps?q=loc:%f,%f", 28.43242324,77.8977673);
Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(uri));
startActivity(intent);

```



## Passing different data through Intent  in Activity


**1. Passing integer data:**

**SenderActivity**

```java
Intent myIntent = new Intent(SenderActivity.this, ReceiverActivity.class);
myIntent.putExtra("intVariableName", intValue);
startActivity(myIntent);

```

**ReceiverActivity**

```java
Intent mIntent = getIntent();
int intValue = mIntent.getIntExtra("intVariableName", 0); // set 0 as the default value if no value for intVariableName found

```

**2. Passing double data:**

**SenderActivity**

```java
Intent myIntent = new Intent(SenderActivity.this, ReceiverActivity.class);
myIntent.putExtra("doubleVariableName", doubleValue);
startActivity(myIntent);

```

**ReceiverActivity**

```java
Intent mIntent = getIntent();
double doubleValue = mIntent.getDoubleExtra("doubleVariableName", 0.00); // set 0.00 as the default value if no value for doubleVariableName found

```

**3. Passing String data:**

**SenderActivity**

```java
Intent myIntent = new Intent(SenderActivity.this, ReceiverActivity.class);
myIntent.putExtra("stringVariableName", stringValue);
startActivity(myIntent);

```

**ReceiverActivity**

```java
Intent mIntent = getIntent();
String stringValue = mIntent.getExtras().getString("stringVariableName");

```

or

```java
Intent mIntent = getIntent();
String stringValue = mIntent.getStringExtra("stringVariableName");

```

**4. Passing ArrayList data :**

**SenderActivity**

```java
Intent myIntent = new Intent(SenderActivity.this, ReceiverActivity.class);
myIntent.putStringArrayListExtra("arrayListVariableName", arrayList);
startActivity(myIntent);

```

**ReceiverActivity**

```java
Intent mIntent = getIntent();
arrayList = mIntent.getStringArrayListExtra("arrayListVariableName");

```

**5. Passing Object data :**

**SenderActivity**

```java
Intent myIntent = new Intent(SenderActivity.this, ReceiverActivity.class);
myIntent.putExtra("ObjectVariableName", yourObject);
startActivity(myIntent);

```

**ReceiverActivity**

```java
Intent mIntent = getIntent();
yourObj = mIntent.getSerializableExtra("ObjectVariableName");

```

> 
<p>**Note :** Keep in mind your custom Class must implement the [`Serializable`](https://en.wikipedia.org/wiki/Serialization)
interface.</p>


**6. Passing HashMap<String, String> data :**

**SenderActivity**

HashMap<String, String> hashMap;

```java
Intent mIntent = new Intent(SenderActivity.this, ReceiverActivity.class);
mIntent.putExtra("hashMap", hashMap);
startActivity(mIntent);

```

**ReceiverActivity**

```java
Intent mIntent = getIntent();    
HashMap<String, String> hashMap = (HashMap<String, String>)
mIntent.getSerializableExtra("hashMap");

```

**7. Passing Bitmap data :**

**SenderActivity**

```java
Intent myIntent = new Intent(SenderActivity.this, ReceiverActivity.class);
myIntent.putExtra("image",bitmap);
startActivity(mIntent);

```

**ReceiverActivity**

```java
Intent mIntent = getIntent();
Bitmap bitmap = mIntent.getParcelableExtra("image");

```



## Showing a File Chooser and Reading the Result


### Starting a File Chooser Activity

```java
public void showFileChooser() {
    Intent intent = new Intent(Intent.ACTION_GET_CONTENT);

    // Update with mime types
    intent.setType("*/*");

    // Update with additional mime types here using a String[]. 
    intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);

    // Only pick openable and local files. Theoretically we could pull files from google drive
    // or other applications that have networked files, but that's unnecessary for this example.
    intent.addCategory(Intent.CATEGORY_OPENABLE);
    intent.putExtra(Intent.EXTRA_LOCAL_ONLY, true);

    // REQUEST_CODE = <some-integer>
    startActivityForResult(intent, REQUEST_CODE);
}

```

### Reading the Result

```java
@Override
protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    // If the user doesn't pick a file just return
    if (requestCode != REQUEST_CODE || resultCode != RESULT_OK) {
        return;
    }

    // Import the file
    importFile(data.getData());
}

public void importFile(Uri uri) {
    String fileName = getFileName(uri);

    // The temp file could be whatever you want
    File fileCopy = copyToTempFile(uri, File tempFile)

    // Done!
}

/**
 * Obtains the file name for a URI using content resolvers. Taken from the following link
 * https://developer.android.com/training/secure-file-sharing/retrieve-info.html#RetrieveFileInfo
 *
 * @param uri a uri to query
 * @return the file name with no path
 * @throws IllegalArgumentException if the query is null, empty, or the column doesn't exist
 */
private String getFileName(Uri uri) throws IllegalArgumentException {
    // Obtain a cursor with information regarding this uri
    Cursor cursor = getContentResolver().query(uri, null, null, null, null);

    if (cursor.getCount() <= 0) {
        cursor.close();
        throw new IllegalArgumentException("Can't obtain file name, cursor is empty");
    }

    cursor.moveToFirst();

    String fileName = cursor.getString(cursor.getColumnIndexOrThrow(OpenableColumns.DISPLAY_NAME));

    cursor.close();

    return fileName;
}

/**
 * Copies a uri reference to a temporary file
 *
 * @param uri      the uri used as the input stream
 * @param tempFile the file used as an output stream
 * @return the input tempFile for convenience
 * @throws IOException if an error occurs
 */
private File copyToTempFile(Uri uri, File tempFile) throws IOException {
    // Obtain an input stream from the uri
    InputStream inputStream = getContentResolver().openInputStream(uri);

    if (inputStream == null) {
        throw new IOException("Unable to obtain input stream from URI");
    }

    // Copy the stream to the temp file
    FileUtils.copyInputStreamToFile(inputStream, tempFile);

    return tempFile;
}

```



## Sharing Multiple Files through Intent


The String List passed as a parameter to the `share()` method contains the paths of all the files you want to share.

It basically loops through the paths, adds them to Uri, and starts the Activity which can accept Files of this type.

```

 public static void share(AppCompatActivity context,List<String> paths) {

        if (paths == null || paths.size() == 0) {
            return;
        }
        ArrayList<Uri> uris = new ArrayList<>();
        Intent intent = new Intent();
        intent.setAction(android.content.Intent.ACTION_SEND_MULTIPLE);
        intent.setType("*/*");
        for (String path : paths) {
                File file = new File(path);
                uris.add(Uri.fromFile(file));
        }
        intent.putParcelableArrayListExtra(Intent.EXTRA_STREAM, uris);
        context.startActivity(intent);
    }

```



## Start Unbound Service using an Intent


A Service is a component which runs in the background (on the UI thread) without direct interaction with the user. An unbound Service is just started, and is not bound to the lifecycle of any Activity.

To start a Service you can do as shown in the example below:

```java
// This Intent will be used to start the service
Intent i= new Intent(context, ServiceName.class);
// potentially add data to the intent extras
i.putExtra("KEY1", "Value to be used by the service");
context.startService(i);

```

You can use any extras from the intent by using an `onStartCommand()` override:

```java
public class MyService extends Service {
    public MyService() {
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId)
    {
        if (intent != null) {
            Bundle extras = intent.getExtras();
            String key1 = extras.getString("KEY1", "");
            if (key1.equals("Value to be used by the service")) {
                //do something
            }
        }
        return START_STICKY;
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

}

```



## Getting a result from Activity to Fragment


Like [Getting a result from another Activity](http://stackoverflow.com/documentation/android/103/intent/533/getting-a-result-from-another-activity) you need to call the `Fragment`'s method [`startActivityForResult(Intent intent, int requestCode)`](https://developer.android.com/reference/android/app/Fragment.html#startActivityForResult(android.content.Intent,%20int)). note that you should not call `getActivity().startActivityForResult()` as this will take the result back to the `Fragment`'s parent `Activity`.

Receiving the result can be done using the `Fragment`'s method [`onActivityResult()`](https://developer.android.com/reference/android/app/Fragment.html#onActivityResult(int,%20int,%20android.content.Intent)). You need to make sure that the Fragment's parent Activity also overrides [`onActivityResult()`](https://developer.android.com/reference/android/app/Activity.html#onActivityResult(int,%20int,%20android.content.Intent)) and calls it's `super` implementation.

In the following example `ActivityOne` contains `FragmentOne`, which will start `ActivityTwo` and expect a result from it.

**ActivityOne**

```java
public class ActivityOne extends Activity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_one);
    }

    // You must override this method as the second Activity will always send its results to this Activity and then to the Fragment
    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
       super.onActivityResult(requestCode, resultCode, data);
    }
}

```

**activity_one.xml**

```java
<fragment android:name="com.example.FragmentOne"
    android:id="@+id/fragment_one"
    android:layout_width="match_parent"
    android:layout_height="match_parent" />

```

**FragmentOne**

```java
public class FragmentOne extends Fragment {
    public static final int REQUEST_CODE = 11;
    public static final int RESULT_CODE = 12;
    public static final String EXTRA_KEY_TEST = "testKey";

    // Initializing and starting the second Activity
    private void startSecondActivity() {
        Intent intent = new Intent(getActivity(), ActivityTwo.class);
        startActivityForResult(REQUEST_CODE, intent);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_CODE && resultCode == RESULT_CODE) {
            String testResult = data.getStringExtra(EXTRA_KEY_TEST);
            // TODO: Do something with your extra data
        }
    }
}

```

**ActivityTwo**

```java
public class ActivityTwo extends Activity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_two);
    }

    private void closeActivity() {
        Intent intent = new Intent();
        intent.putExtra(FragmentOne.EXTRA_KEY_TEST, "Testing passing data back to ActivityOne");
        setResult(FragmentOne.RESULT_CODE, intent); // You can also send result without any data using setResult(int resultCode)
        finish();
    }
}

```



#### Syntax


- Intent Intent()
- Intent Intent(Intent intent)
- Intent Intent(String action)
- Intent Intent(String action, Uri uri)
- Intent Intent(Context packageContext, Class<?> cls)
- Intent Intent(String action, Uri uri, Context packageContext, Class<?> cls)
- void startActivity(Intent intent)
- void startActivity(Intent intent, Bundle options)
- void startActivityForResult (Intent intent, int requestCode)
- void startActivityForResult (Intent intent, int requestCode, Bundle options)
- Intent putExtra(String name, double[] value)
- Intent putExtra(String name, int value)
- Intent putExtra(String name, CharSequence value)
- Intent putExtra(String name, char value)
- Intent putExtra(String name, Bundle value)
- Intent putExtra(String name, Parcelable[] value)
- Intent putExtra(String name, Serializable value)
- Intent putExtra(String name, int[] value)
- Intent putExtra(String name, float value)
- Intent putExtra(String name, byte[] value)
- Intent putExtra(String name, long[] value)
- Intent putExtra(String name, Parcelable value)
- Intent putExtra(String name, float[] value)
- Intent putExtra(String name, long value)
- Intent putExtra(String name, String[] value)
- Intent putExtra(String name, boolean value)
- Intent putExtra(String name, boolean[] value)
- Intent putExtra(String name, short value)
- Intent putExtra(String name, double value)
- Intent putExtra(String name, short[] value)
- Intent putExtra(String name, String value)
- Intent putExtra(String name, byte value)
- Intent putExtra(String name, char[] value)
- Intent putExtra(String name, CharSequence[] value)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|intent|The intent to start
|requestCode|Unique number to identify the request
|options|Additional options for how the Activity should be started
|name|The name of the extra data
|value|The value of the extra data
|CHOOSE_CONTACT_REQUEST_CODE|the code of the request, to identify it on `onActivityResult` method
|action|Any action to perform via this intent, ex: Intent.ACTION_VIEW
|uri|data uri to be used by intent to perform specified action
|packageContext|Context to use to initialize the Intent
|cls|Class to be used by this intent



#### Remarks


### Caveats of using implicit intent

When calling a implicit intent it's always helpful to check if it's possible by the system to handle it.

This can be done by checking using `PackageManager.queryIntentActivities(Intent intent, int flags)`

```java
PackageManager pm = getActivity().getPackageManager();
if (intent.resolveActivity(pm) != null) {
    //intent can be handled
    startActivity(intent);
} else {
     //intent can not be handled
}

```

### Starting Activity which is a `singleTask` or `singleTop`

When the activity's [launch mode](https://stackoverflow.com/documentation/android/1481/activities/14083/activity-launchmode#t=201607261721338187395) is `singleTask` or `singleTop`, the `onActivityResult` will be called as soon as the activity is started with a data null. To prevent this, use `Intent.setFlags(0)` to reset the default flags.

