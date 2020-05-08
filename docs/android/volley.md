---
metaTitle: "Android - Volley"
description: "Using Volley for HTTP requests, Basic StringRequest using GET method, Adding custom design time attributes to NetworkImageView, Cancel a request, Request JSON, Adding custom headers to your requests [e.g. for basic auth], Remote server authentication using StringRequest through POST method, Helper Class for Handling Volley Errors, Boolean variable response from server  with json request in volley, Use JSONArray as request body"
---

# Volley


Volley is an Android HTTP library that was introduced by Google to make networking calls much simpler. By default all the Volley network calls are made asynchronously, handling everything in a background thread and returning the results in the foreground with use of callbacks. As fetching data over a network is one of the most common tasks that is performed in any app, the Volley library was made to ease Android app development.



## Using Volley for HTTP requests


**Add the gradle dependency in app-level build.gradle**

`compile 'com.android.volley:volley:1.0.0'`

Also, add the [android.permission.INTERNET](https://developer.android.com/reference/android/Manifest.permission.html#INTERNET) permission to your app's manifest.

**Create Volley RequestQueue instance singleton in your Application **

```java
public class InitApplication extends Application {

private RequestQueue queue;
private static InitApplication sInstance;

private static final String TAG = InitApplication.class.getSimpleName();


@Override
public void onCreate() {
    super.onCreate();

    sInstance = this;

    Stetho.initializeWithDefaults(this);

}

public static synchronized InitApplication getInstance() {
    return sInstance;
}

public <T> void addToQueue(Request<T> req, String tag) {
    req.setTag(TextUtils.isEmpty(tag) ? TAG : tag);
    getQueue().add(req);
}

public <T> void addToQueue(Request<T> req) {
    req.setTag(TAG);
    getQueue().add(req);
}

public void cancelPendingRequests(Object tag) {
    if (queue != null) {
        queue.cancelAll(tag);
    }
}

public RequestQueue getQueue() {
    if (queue == null) {
        queue = Volley.newRequestQueue(getApplicationContext());
        return queue;
    }
    return queue;
}
}

```

Now, you can use the volley instance using the getInstance() method and add a new request in the queue using `InitApplication.getInstance().addToQueue(request);`

A simple example to request JsonObject from server is

```java
JsonObjectRequest myRequest = new JsonObjectRequest(Method.GET,
        url, null,
        new Response.Listener<JSONObject>() {

            @Override
            public void onResponse(JSONObject response) {
                Log.d(TAG, response.toString());
            }
        }, new Response.ErrorListener() {

            @Override
            public void onErrorResponse(VolleyError error) {
                Log.d(TAG, "Error: " + error.getMessage());
            }
});

myRequest.setRetryPolicy(new DefaultRetryPolicy(
        MY_SOCKET_TIMEOUT_MS, 
        DefaultRetryPolicy.DEFAULT_MAX_RETRIES, 
        DefaultRetryPolicy.DEFAULT_BACKOFF_MULT));

```

To handle Volley timeouts you need to use a [`RetryPolicy`](https://pablobaxter.github.io/volley-docs/com/android/volley/RetryPolicy.html). A retry policy is used in case a request cannot be completed due to network failure or some other cases.

Volley provides an easy way to implement your `RetryPolicy` for your requests. By default, Volley sets all socket and connection timeouts to 5 seconds for all requests.
`RetryPolicy` is an interface where you need to implement your logic of how you want to retry a particular request when a timeout occurs.

The constructor takes the following three parameters:

- `initialTimeoutMs` - Specifies the socket timeout in milliseconds for every retry attempt.
- `maxNumRetries` - The number of times retry is attempted.
- `backoffMultiplier` - A multiplier which is used to determine exponential time set to socket for every retry attempt.



## Basic StringRequest using GET method


```java
final TextView mTextView = (TextView) findViewById(R.id.text);
...

// Instantiate the RequestQueue.
RequestQueue queue = Volley.newRequestQueue(this);
String url ="http://www.google.com";

// Request a string response from the provided URL.
StringRequest stringRequest = new StringRequest(Request.Method.GET, url,
            new Response.Listener<String>() {
    @Override
    public void onResponse(String response) {
        // Display the first 500 characters of the response string.
        mTextView.setText("Response is: "+ response.substring(0,500));
    }
}, new Response.ErrorListener() {
    @Override
    public void onErrorResponse(VolleyError error) {
        mTextView.setText("That didn't work!");
    }
});
// Add the request to the RequestQueue.
queue.add(stringRequest);

```



## Adding custom design time attributes to NetworkImageView


There are several additional attributes that the Volley [`NetworkImageView`](https://pablobaxter.github.io/volley-docs/com/android/volley/toolbox/NetworkImageView.html) adds to the standard `ImageView`. However, these attributes can only be set in code. The following is an example of how to make an extension class that will pick up the attributes from your XML layout file and apply them to the `NetworkImageView` instance for you.

In your `~/res/xml` directory, add a file named `attrx.xml`:

```java
<resources>
    <declare-styleable name="MoreNetworkImageView">
        <attr name="defaultImageResId" format="reference"/>
        <attr name="errorImageResId" format="reference"/>
    </declare-styleable>
</resources>

```

Add a new class file to your project:

```java
package my.namespace;

import android.content.Context;
import android.content.res.TypedArray;
import android.support.annotation.NonNull;
import android.util.AttributeSet;

import com.android.volley.toolbox.NetworkImageView;

public class MoreNetworkImageView extends NetworkImageView {
    public MoreNetworkImageView(@NonNull final Context context) {
        super(context);
    }

    public MoreNetworkImageView(@NonNull final Context context, @NonNull final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public MoreNetworkImageView(@NonNull final Context context, @NonNull final AttributeSet attrs, final int defStyle) {
        super(context, attrs, defStyle);

        final TypedArray attributes = context.obtainStyledAttributes(attrs, R.styleable.MoreNetworkImageView, defStyle, 0);

        // load defaultImageResId from XML
        int defaultImageResId = attributes.getResourceId(R.styleable.MoreNetworkImageView_defaultImageResId, 0);
        if (defaultImageResId > 0) {
            setDefaultImageResId(defaultImageResId);
        }

        // load errorImageResId from XML
        int errorImageResId = attributes.getResourceId(R.styleable.MoreNetworkImageView_errorImageResId, 0);
        if (errorImageResId > 0) {
            setErrorImageResId(errorImageResId);
        }
    }
}

```

An example layout file showing the use of the custom attributes:

```java
<?xml version="1.0" encoding="utf-8"?>
<android.support.v7.widget.CardView
  xmlns:android="http://schemas.android.com/apk/res/android"
  xmlns:app="http://schemas.android.com/apk/res-auto"
  xmlns:tools="http://schemas.android.com/tools"
  android:layout_width="wrap_content"
  android:layout_height="fill_parent">

  <my.namespace.MoreNetworkImageView
    android:layout_width="64dp"
    android:layout_height="64dp"
    app:errorImageResId="@drawable/error_img"
    app:defaultImageResId="@drawable/default_img"
    tools:defaultImageResId="@drawable/editor_only_default_img"/>
    <!-- 
      Note: The "tools:" prefix does NOT work for custom attributes in Android Studio 2.1 and 
      older at least, so in this example the defaultImageResId would show "default_img" in the 
      editor, not the "editor_only_default_img" drawable even though it should if it was 
      supported as an editor-only override correctly like standard Android properties.
    -->

</android.support.v7.widget.CardView>

```



## Cancel a request


```java
// assume a Request and RequestQueue have already been initialized somewhere above

public static final String TAG = "SomeTag";

// Set the tag on the request.
request.setTag(TAG);

// Add the request to the RequestQueue.
mRequestQueue.add(request);

// To cancel this specific request
request.cancel();

// ... then, in some future life cycle event, for example in onStop()
// To cancel all requests with the specified tag in RequestQueue
mRequestQueue.cancelAll(TAG);

```



## Request JSON


```java
final TextView mTxtDisplay = (TextView) findViewById(R.id.txtDisplay);
ImageView mImageView;
String url = "http://ip.jsontest.com/";

final JsonObjectRequest jsObjRequest = new JsonObjectRequest
        (Request.Method.GET, url, null, new Response.Listener<JSONObject>() {    
    @Override
    public void onResponse(JSONObject response) {
        mTxtDisplay.setText("Response: " + response.toString());
    }
}, new Response.ErrorListener() {    
    @Override
    public void onErrorResponse(VolleyError error) {
        // ...
    }
});

requestQueue.add(jsObjRequest);

```



## Adding custom headers to your requests [e.g. for basic auth]


If you need to add custom headers to your volley requests, you can't do this after initialisation, as the headers are saved in a private variable.

Instead, you need to override the `getHeaders()` method of `Request.class` as such:

```java
new JsonObjectRequest(REQUEST_METHOD, REQUEST_URL, REQUEST_BODY, RESP_LISTENER, ERR_LISTENER) {
    @Override
    public Map<String, String> getHeaders() throws AuthFailureError {
        HashMap<String, String> customHeaders = new Hashmap<>();

        customHeaders.put("KEY_0", "VALUE_0");
        ...
        customHeaders.put("KEY_N", "VALUE_N");

        return customHeaders;
    }
};

```

Explanation of the parameters:

- `REQUEST_METHOD` - Either of the `Request.Method.*` constants.
- `REQUEST_URL` - The full URL to send your request to.
- `REQUEST_BODY` - A `JSONObject` containing the POST-Body to be sent (or null).
- `RESP_LISTENER` - A `Response.Listener<?>` object, whose `onResponse(T data)` method is called upon successful completion.
- `ERR_LISTENER` - A `Response.ErrorListener` object, whose `onErrorResponse(VolleyError e)` method is called upon a unsuccessful request.

If you want to build a custom request, you can add the headers in it as well:

```java
public class MyCustomRequest extends Request {
    ...
    @Override
    public Map<String, String> getHeaders() throws AuthFailureError {
        HashMap<String, String> customHeaders = new Hashmap<>();

        customHeaders.put("KEY_0", "VALUE_0");
        ...
        customHeaders.put("KEY_N", "VALUE_N");

        return customHeaders;
    }
    ...
}

```



## Remote server authentication using StringRequest through POST method


For the sake of this example, let us assume that we have a server for handling the POST requests that we will be making from our Android app:

```java
// User input data.
String email = "my@email.com";
String password = "123";

// Our server URL for handling POST requests.
String URL = "http://my.server.com/login.php";

// When we create a StringRequest (or a JSONRequest) for sending
// data with Volley, we specify the Request Method as POST, and 
// the URL that will be receiving our data.
StringRequest stringRequest = 
    new StringRequest(Request.Method.POST, URL, 
    new Response.Listener<String>() {
        @Override
        public void onResponse(String response) {
            // At this point, Volley has sent the data to your URL
            // and has a response back from it. I'm going to assume
            // that the server sends an "OK" string.
            if (response.equals("OK")) {
                // Do login stuff.
            } else {
                // So the server didn't return an "OK" response.
                // Depending on what you did to handle errors on your
                // server, you can decide what action to take here.
            }
        }
    },
    new Response.ErrorListener() {
        @Override
        public void onErrorResponse(VolleyError error) {      
            // This is when errors related to Volley happen.
            // It's up to you what to do if that should happen, but
            // it's usually not a good idea to be too clear as to
            // what happened here to your users.
        }
    }) {
        @Override
        protected Map<String, String> getParams() throws AuthFailureError {
            // Here is where we tell Volley what it should send in
            // our POST request. For this example, we want to send
            // both the email and the password. 
            
            // We will need key ids for our data, so our server can know
            // what is what.
            String key_email = "email";
            String key_password = "password";

            Map<String, String> map = new HashMap<String, String>();
            // map.put(key, value);
            map.put(key_email, email);
            map.put(key_password, password);
            return map;
        }
    };

    // This is a policy that we need to specify to tell Volley, what
    // to do if it gets a timeout, how many times to retry, etc.
    stringRequest.setRetryPolicy(new RetryPolicy() {
            @Override
            public int getCurrentTimeout() {
                // Here goes the timeout.
                // The number is in milliseconds, 5000 is usually enough,
                // but you can up or low that number to fit your needs.
                return 50000;
            }
            @Override
            public int getCurrentRetryCount() {
                // The maximum number of attempts.
                // Again, the number can be anything you need.
                return 50000;
            }
            @Override
            public void retry(VolleyError error) throws VolleyError {
                // Here you could check if the retry count has gotten
                // to the maximum number, and if so, send a VolleyError
                // message or similar. For the sake of the example, I'll 
                // show a Toast.  
                Toast.makeText(getContext(), error.toString(), Toast.LENGTH_LONG).show();
            }
    });
    
    // And finally, we create a Volley Queue. For this example, I'm using
    // getContext(), because I was working with a Fragment. But context could
    // be "this", "getContext()", etc.
    RequestQueue requestQueue = Volley.newRequestQueue(getContext());
    requestQueue.add(stringRequest);

} else { 
    // If, for example, the user inputs an email that is not currently
    // on your remote DB, here's where we can inform the user.
    Toast.makeText(getContext(), "Wrong email", Toast.LENGTH_LONG).show();
}

```



## Helper Class for Handling Volley Errors


```java
public class VolleyErrorHelper {
        /**
         * Returns appropriate message which is to be displayed to the user
         * against the specified error object.
         *
         * @param error
         * @param context
         * @return
         */

        public static String getMessage (Object error , Context context){
            if(error instanceof TimeoutError){
                return context.getResources().getString(R.string.timeout);
            }else if (isServerProblem(error)){
                return handleServerError(error ,context);

            }else if(isNetworkProblem(error)){
                return context.getResources().getString(R.string.nointernet);
            }
            return context.getResources().getString(R.string.generic_error);

        }

        private static String handleServerError(Object error, Context context) {

            VolleyError er = (VolleyError)error;
            NetworkResponse response = er.networkResponse;
            if(response != null){
                switch (response.statusCode){

                    case 404:
                    case 422:
                    case 401:
                        try {
                            // server might return error like this { "error": "Some error occured" }
                            // Use "Gson" to parse the result
                            HashMap<String, String> result = new Gson().fromJson(new String(response.data),
                                    new TypeToken<Map<String, String>>() {
                                    }.getType());

                            if (result != null && result.containsKey("error")) {
                                return result.get("error");
                            }

                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        // invalid request
                        return ((VolleyError) error).getMessage();

                    default:
                        return context.getResources().getString(R.string.timeout);
                }
            }

            return context.getResources().getString(R.string.generic_error);
        }

        private static boolean isServerProblem(Object error) {
            return (error instanceof ServerError || error instanceof AuthFailureError);
        }

        private static boolean isNetworkProblem (Object error){
            return (error instanceof NetworkError || error instanceof NoConnectionError);
        }

```



## Boolean variable response from server  with json request in volley


you can custom class below one

```java
private final String PROTOCOL_CONTENT_TYPE = String.format("application/json; charset=%s", PROTOCOL_CHARSET);

    public BooleanRequest(int method, String url, String requestBody, Response.Listener<Boolean> listener, Response.ErrorListener errorListener) {
        super(method, url, errorListener);
        this.mListener = listener;
        this.mErrorListener = errorListener;
        this.mRequestBody = requestBody;
    }

    @Override
    protected Response<Boolean> parseNetworkResponse(NetworkResponse response) {
        Boolean parsed;
        try {
            parsed = Boolean.valueOf(new String(response.data, HttpHeaderParser.parseCharset(response.headers)));
        } catch (UnsupportedEncodingException e) {
            parsed = Boolean.valueOf(new String(response.data));
        }
        return Response.success(parsed, HttpHeaderParser.parseCacheHeaders(response));
    }

    @Override
    protected VolleyError parseNetworkError(VolleyError volleyError) {
        return super.parseNetworkError(volleyError);
    }

    @Override
    protected void deliverResponse(Boolean response) {
        mListener.onResponse(response);
    }

    @Override
    public void deliverError(VolleyError error) {
        mErrorListener.onErrorResponse(error);
    }

    @Override
    public String getBodyContentType() {
        return PROTOCOL_CONTENT_TYPE;
    }

    @Override
    public byte[] getBody() throws AuthFailureError {
        try {
            return mRequestBody == null ? null : mRequestBody.getBytes(PROTOCOL_CHARSET);
        } catch (UnsupportedEncodingException uee) {
            VolleyLog.wtf("Unsupported Encoding while trying to get the bytes of %s using %s",
                    mRequestBody, PROTOCOL_CHARSET);
            return null;
        }
    }
}

```

use this with your activity

```java
try {
        JSONObject jsonBody;
        jsonBody = new JSONObject();
        jsonBody.put("Title", "Android Demo");
        jsonBody.put("Author", "BNK");
        jsonBody.put("Date", "2015/08/28");
        String requestBody = jsonBody.toString();
        BooleanRequest booleanRequest = new BooleanRequest(0, url, requestBody, new Response.Listener<Boolean>() {
            @Override
            public void onResponse(Boolean response) {
                Toast.makeText(mContext, String.valueOf(response), Toast.LENGTH_SHORT).show();
            }
        }, new Response.ErrorListener() {
            @Override
            public void onErrorResponse(VolleyError error) {
                Toast.makeText(mContext, error.toString(), Toast.LENGTH_SHORT).show();
            }
        });
        // Add the request to the RequestQueue.
        queue.add(booleanRequest);
    } catch (JSONException e) {
        e.printStackTrace();
    }

```



## Use JSONArray as request body


The default requests integrated in volley don't allow to pass a `JSONArray` as request body in a `POST` request. Instead, you can only pass a `JSON` object as a parameter.

However, instead of passing a `JSON` object as a parameter to the request constructor, you need to override the `getBody()` method of the `Request.class`. You should pass `null` as third parameter as well:

```java
JSONArray requestBody = new JSONArray();

new JsonObjectRequest(Request.Method.POST, REQUEST_URL, null, RESP_LISTENER, ERR_LISTENER) {
    @Override
    public byte[] getBody() {
        try {
            return requestBody.toString().getBytes(PROTOCOL_CHARSET);
        } catch (UnsupportedEncodingException uee) {
            // error handling
            return null;
        }
    }
};

```

Explanation of the parameters:

- `REQUEST_URL` - The full URL to send your request to.
- `RESP_LISTENER` - A `Response.Listener<?>` object, whose `onResponse(T data)` method is called upon successful completion.
- `ERR_LISTENER` - A `Response.ErrorListener` object, whose `onErrorResponse(VolleyError e)` method is called upon an unsuccessful request.



#### Syntax


- RequestQueue queue = Volley.newRequestQueue(context); // setup the queue
- Request request = new SomeKindOfRequestClass(Request.Method, String url, Response.Listener, Response.ErrorListener); // setup some kind of request, the exact type and arguments change for each request type
- queue.add(request); // add the request to the queue; the appropriate response listener will be called once the request is finished (or terminated for whatever reason)



#### Remarks


### Installation

You can build Volley from the [official Google source code](https://developer.android.com/training/volley/index.html). For a while, that was the only option. Or using one of the third-party pre-built versions. However, Google finally released an official maven package on jcenter.

In your application-level `build.gradle` file, add this to your dependencies list:

```java
dependencies {
    ...
    compile 'com.android.volley:volley:1.0.0'
}

```

Ensure the `INTERNET` permission is set in your app's manifest:

```java
<uses-permission android:name="android.permission.INTERNET"/>

```

### Official Documentation

Google has not provided very extensive documentation on this library, and they haven't touched it in years. But what is available can be found at:

[https://developer.android.com/training/volley/index.html](https://developer.android.com/training/volley/index.html)

There is unofficial documentation hosted on GitHub, although there should be a better location to host this in the future:

[https://pablobaxter.github.io/volley-docs/](https://pablobaxter.github.io/volley-docs/)

