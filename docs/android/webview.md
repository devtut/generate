---
metaTitle: "Android - WebView"
description: "Troubleshooting WebView by printing console messages or by remote debugging, Communication from Javascript to Java (Android), Communication from Java to Javascript, JavaScript alert dialogs in WebView - How to make them work, Open dialer example, Open Local File / Create dynamic content in Webview"
---

# WebView


[WebView](https://developer.android.com/reference/android/webkit/WebView.html) is a view that display web pages inside your application. By this you can add your own URL.



## Troubleshooting WebView by printing console messages or by remote debugging


### Printing webview console messages to logcat

To handle `console` messages from web page you can override [`onConsoleMessage`](https://developer.android.com/reference/android/webkit/WebChromeClient.html#onConsoleMessage(android.webkit.ConsoleMessage)) in [`WebChromeClient`](https://developer.android.com/reference/android/webkit/WebChromeClient.htm):

```java
final class ChromeClient extends WebChromeClient {
    @Override
    public boolean onConsoleMessage(ConsoleMessage msg) {
        Log.d(
            "WebView", 
            String.format("%s %s:%d", msg.message(), msg.lineNumber(), msg.sourceId())
        );
        return true;
    }
}

```

And set it in your activity or fragment:

```java
webView.setWebChromeClient(new ChromeClient());

```

So this sample page:

```java
<html>
<head>
    <script type="text/javascript">
        console.log('test message');
    </script>
</head>
<body>
</body>
</html>

```

will write log 'test message' to logcat:

> 
WebView: test message sample.html:4


`console.info()`, `console.warn()` and `console.error()` are also supported by chrome-client.

### Remote debugging android devices with Chrome

Your can remote debug webview based application from you desktop Chrome.

### Enable USB debugging on your Android device

On your Android device, open up Settings, find the Developer options section, and enable USB debugging.

### Connect and discover your Android device

Open page in chrome following page: [chrome://inspect/#devices](http://chrome://inspect/#devices)

From the Inspect Devices dialog, select your device and press **inspect**. A new instance of Chrome's DevTools opens up on your development machine.

More detailed guideline and description of DevTools can be found on [developers.google.com](https://developers.google.com/web/tools/chrome-devtools/remote-debugging/)



## Communication from Javascript to Java (Android)


**Android Activity**

```java
package com.example.myapp;

import android.os.Bundle;
import android.app.Activity;
import android.webkit.WebView;

public class WebViewActivity extends Activity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    
        WebView webView = new WebView(this);
        setContentView(webView);

       /* 
        *   Note the label Android, this is used in the Javascript side of things
        *   You can of course change this.
        */
        webView.addJavascriptInterface(new JavascriptHandler(), "Android");
    
        webView.loadUrl("http://example.com");
    }
}

```

**Java Javascript Handler**

```java
import android.webkit.JavascriptInterface;

public class JavascriptHandler {

    /**
     *  Key point here is the annotation @JavascriptInterface
     *
     */
    @JavascriptInterface
    public void jsCallback() {
       // Do something
    }

    @JavascriptInterface
    public void jsCallbackTwo(String dummyData) {
       // Do something
    }
}

```

**Web Page, Javascript call**

```java
<script>
... 
Android.jsCallback();
...
Android.jsCallback('hello test');
...
</script>

```

**Extra Tip**

Passing in a complex data structure, a possible solution is use JSON.

```java
Android.jsCallback('{ "fake-var" : "fake-value", "fake-array" : [0,1,2] }');

```

On the Android side use your favorite JSON parser ie: JSONObject



## Communication from Java to Javascript


**Basic Example**

```java
package com.example.myapp;

import android.os.Bundle;
import android.app.Activity;
import android.webkit.WebView;

public class WebViewActivity extends Activity {
    
    private Webview webView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
       super.onCreate(savedInstanceState);

       webView = new WebView(this);
       webView.getSettings().setJavaScriptEnabled(true);

       setContentView(webView);

       webView.loadUrl("http://example.com");

       /*
        * Invoke Javascript function
        */
       webView.loadUrl("javascript:testJsFunction('Hello World!')");
    }

   /**
    * Invoking a Javascript function
    */
   public void doSomething() {
     this.webView.loadUrl("javascript:testAnotherFunction('Hello World Again!')");
   }
}

```



## JavaScript alert dialogs in WebView - How to make them work


By default, WebView does not implement JavaScript alert dialogs, ie. `alert()` will do nothing. In order to make you need to firstly enable JavaScript (obviously..), and then set a `WebChromeClient` to handle requests for alert dialogs from the page:

```java
webView.setWebChromeClient(new WebChromeClient() {
    //Other methods for your WebChromeClient here, if needed..

    @Override
    public boolean onJsAlert(WebView view, String url, String message, JsResult result) {
        return super.onJsAlert(view, url, message, result);
    }
});

```

Here, we override `onJsAlert`, and then we call through to the super implementation, which gives us a standard Android dialog. You can also use the message and URL yourself, for example if you want to create a custom styled dialog or if you want to log them.



## Open dialer example


If the web page a contains phone number you can make a call using your phone's dialer. This code checks for the url which starts with tel: then make an intent to open dialer and you can make a call to the clicked phone number:

```java
public boolean shouldOverrideUrlLoading(WebView view, String url) {
    if (url.startsWith("tel:")) { 
            Intent intent = new Intent(Intent.ACTION_DIAL,
                    Uri.parse(url)); 
            startActivity(intent); 
    }else if(url.startsWith("http:") || url.startsWith("https:")) {
        view.loadUrl(url);
    }
    return true;
}

```



## Open Local File / Create dynamic content in Webview


Layout.xml

```

               <WebView
                android:id="@+id/WebViewToDisplay"
                android:layout_width="fill_parent"
                android:layout_height="fill_parent"
                android:layout_gravity="center"
                android:fadeScrollbars="false" />

```

Load data into WebViewToDisplay

```

       WebView webViewDisplay;
        StringBuffer LoadWEb1;

        webViewDisplay = (WebView) findViewById(R.id.WebViewToDisplay);
        LoadWEb1 = new StringBuffer();
        LoadWEb1.append("<html><body><h1>My First Heading</h1><p>My first paragraph.</p>");
        //Sample code to read parameters at run time
        String strName = "Test Paragraph";
        LoadWEb1.append("<br/><p>"+strName+"</p>");
        String result = LoadWEb1.append("</body></html>").toString();
                WebSettings webSettings = webViewDisplay.getSettings();
                webSettings.setJavaScriptEnabled(true);
                webViewDisplay.getSettings().setBuiltInZoomControls(true);
                if (android.os.Build.VERSION.SDK_INT >= 11){
                    webViewDisplay.setLayerType(View.LAYER_TYPE_SOFTWARE, null);
                    webViewDisplay.getSettings().setDisplayZoomControls(false);
                }

                webViewDisplay.loadDataWithBaseURL(null, result, "text/html", "utf-8",
                        null);
                //To load local file directly from assets folder use below code
                //webViewDisplay.loadUrl("file:///android_asset/aboutapp.html");

```



#### Remarks


Please don't forget to add permission in your Android manifest file

```java
<uses-permission android:name="android.permission.INTERNET" /> 

```

