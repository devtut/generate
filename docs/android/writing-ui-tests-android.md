---
metaTitle: "Android - Writing UI tests - Android"
description: "MockWebServer example, IdlingResource"
---

# Writing UI tests - Android


Focus of this document is to represent goals and ways how to write android UI and integration tests.
[Espresso](http://stackoverflow.com/documentation/android/3485/testing-ui-with-espresso#t=201703302242404424973) and UIAutomator are provided by Google so focus should be around these tools and their respective wrappers e.g. Appium, Spoon etc.



## MockWebServer example


In case your activities, fragments and UI require some background processing a good thing to use is a MockWebServer which runs localy on an android device which brings a closed and testable enviroment for your UI.

[https://github.com/square/okhttp/tree/master/mockwebserver](https://github.com/square/okhttp/tree/master/mockwebserver)

First step is including the gradle dependency:

```java
testCompile 'com.squareup.okhttp3:mockwebserver:(insert latest version)'

```

Now steps for running and using the mock server are:

- create mock server object
- start it at specific address and port (usually localhost:portnumber)
- enqueue responses for specific requests
- start the test

This is nicely explained in the github page of the mockwebserver but in our case we want something nicer and reusable for all tests, and JUnit rules will come nicely into play here:

```java
/**
 *JUnit  rule that starts and stops a mock web server for test runner
*/
 public class MockServerRule extends UiThreadTestRule {

 private MockWebServer mServer;

 public static final int MOCK_WEBSERVER_PORT = 8000;

    @Override
    public Statement apply(final Statement base, Description description) {
        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                startServer();
                try {
                    base.evaluate();
                } finally {
                    stopServer();
                }
            }
        };
    }

    /**
     * Returns the started web server instance
     *
     * @return mock server
     */
    public MockWebServer server() {
        return mServer;
    }

    public void startServer() throws IOException, NoSuchAlgorithmException {
        mServer = new MockWebServer();
        try {
            mServer(MOCK_WEBSERVER_PORT);
        } catch (IOException e) {
            throw new IllegalStateException(e,"mock server start issue");
        }
    }

    public void stopServer() {
        try {
            mServer.shutdown();
        } catch (IOException e) {
            Timber.e(e, "mock server shutdown error”);
        }
    }
}

```

Now lets assume that we have the exact same activity like in previous example,
just in this case when we push the button app will fetch something from the network for example:
[https://someapi.com/name](https://someapi.com/name)

This would return some text string which would be concatenated in the snackbar text e.g.
NAME + text you typed in.

```java
/**
* Testing of the snackbar activity with networking.
**/
@RunWith(AndroidJUnit4.class)
@LargeTest
public class SnackbarActivityTest{
    //espresso rule which tells which activity to start
    @Rule
    public final ActivityTestRule<SnackbarActivity> mActivityRule = 
        new ActivityTestRule<>(SnackbarActivity.class, true, false);

    //start mock web server
    @Rule
    public final MockServerRule mMockServerRule = new MockServerRule();

    @Override
    public void tearDown() throws Exception {
       //same as previous example
    }
    
    @Override
    public void setUp() throws Exception {
       //same as previous example

       **//IMPORTANT:** point your application to your mockwebserver endpoint e.g.
       MyAppConfig.setEndpointURL("http://localhost:8000");
    }
    
    /**
    *Test methods should always start with "testXYZ" and it is a good idea to 
    *name them after the intent what you want to test
    **/
    @Test
    public void testSnackbarIsShown() {
        //setup mockweb server
        mMockServerRule.server().setDispatcher(getDispatcher());

        mActivityRule.launchActivity(null);
        //check is our text entry displayed and enter some text to it
        String textToType="new snackbar text";
        onView(withId(R.id.textEntry)).check(matches(isDisplayed()));
        //we check is our snackbar showing text from mock webserver plus the one we typed
        onView(withId(R.id.textEntry)).perform(typeText("JazzJackTheRabbit" + textToType));
        //click the button to show the snackbar
        onView(withId(R.id.shownSnackbarBtn)).perform(click());
        //assert that a view with snackbar_id with text which we typed and is displayed
        onView(allOf(withId(android.support.design.R.id.snackbar_text), 
        withText(textToType))) .check(matches(isDisplayed()));
    }
    
     /**
     *creates a mock web server dispatcher with prerecorded requests and responses
     **/
    private Dispatcher getDispatcher() {
        final Dispatcher dispatcher = new Dispatcher() {
            @Override
            public MockResponse dispatch(RecordedRequest request) throws InterruptedException {
                if (request.getPath().equals("/name")){
                    return new MockResponse().setResponseCode(200)
                            .setBody("JazzJackTheRabbit");
                }
                throw new IllegalStateException("no mock set up for " + request.getPath());
            }
        };
        return dispatcher;
    }

```

I would suggest wrapping the dispatcher in some sort of a Builder so you can easily chain and add new responses for your screens.
e.g.

```

return newDispatcherBuilder()
            .withSerializedJSONBody("/authenticate", Mocks.getAuthenticationResponse())
            .withSerializedJSONBody("/getUserInfo", Mocks.getUserInfo())
            .withSerializedJSONBody("/checkNotBot", Mocks.checkNotBot());

```



## IdlingResource


The power of idling resources lies in not having to wait for some app's processing (networking, calculations, animations, etc.) to finish with `sleep()`, which brings flakiness and/or prolongs the tests run. The official documentation can be found [here](https://developer.android.com/reference/android/support/test/espresso/IdlingResource.html).

### Implementation

There are three things that you need to do when implementing `IdlingResource` interface:

- **`getName()`** - Returns the name of your idling resource.
- **`isIdleNow()`** - Checks whether your xyz object, operation, etc. is idle at the moment.
- **`registerIdleTransitionCallback`** (`IdlingResource.ResourceCallback` callback) - Provides a callback which you should call when your object transitions to idle.

Now you should create your own logic and determine when your app is idle and when not, since this is dependant on the app. Below you will find a simple example, just to show how it works. There are other examples online, but specific app implementation brings to specific idling resource implementations.

### NOTES

- There have been some Google examples where they put `IdlingResources` in the code of the app. **Do not do this.** They presumably placed it there just to show how they work.
- Keeping your code clean and maintaining single principle of responsibility is up to you!

### Example

Let us say that you have an activity which does weird stuff and takes a long time for the fragment to load and thus making your Espresso tests fail by not being able to find resources from your fragment (you should change how your activity is created and when to speed it up). But in any case to keep it simple, the following example shows how it should look like.

Our example idling resource would get two objects:

- The **tag** of the fragment which you need to find and waiting to get attached to the activity.
- A **FragmentManager** object which is used for finding the fragment.

```java
/**
 * FragmentIdlingResource - idling resource which waits while Fragment has not been loaded.
 */
public class FragmentIdlingResource implements IdlingResource {
    private final FragmentManager mFragmentManager;
    private final String mTag;
    //resource callback you use when your activity transitions to idle
    private volatile ResourceCallback resourceCallback;

    public FragmentIdlingResource(FragmentManager fragmentManager, String tag) {
        mFragmentManager = fragmentManager;
        mTag = tag;
    }

    @Override
    public String getName() {
        return FragmentIdlingResource.class.getName() + ":" + mTag;
    }

    @Override
    public boolean isIdleNow() {
        //simple check, if your fragment is added, then your app has became idle
        boolean idle = (mFragmentManager.findFragmentByTag(mTag) != null);
        if (idle) {
            //IMPORTANT: make sure you call onTransitionToIdle
            resourceCallback.onTransitionToIdle();
        }
        return idle;
    }

    @Override
    public void registerIdleTransitionCallback(ResourceCallback resourceCallback) {
        this.resourceCallback = resourceCallback;
    }
}

```

Now that you have your `IdlingResource` written, you need to use it somewhere right?

### Usage

Let us skip the entire test class setup and just look how a test case would look like:

```java
@Test
public void testSomeFragmentText() {
    mActivityTestRule.launchActivity(null);
   
    //creating the idling resource
    IdlingResource fragmentLoadedIdlingResource = new FragmentIdlingResource(mActivityTestRule.getActivity().getSupportFragmentManager(), SomeFragmentText.TAG);
    //registering the idling resource so espresso waits for it
    Espresso.registerIdlingResources(idlingResource1);
    onView(withId(R.id.txtHelloWorld)).check(matches(withText(helloWorldText)));

    //lets cleanup after ourselves
    Espresso.unregisterIdlingResources(fragmentLoadedIdlingResource);
}

```

### Combination with JUnit rule

This is not to hard; you can also apply the idling resource in form of a JUnit test rule. For example, let us say that you have some SDK that contains Volley in it and you want Espresso to wait for it. Instead of going through each test case or applying it in setup, you could create a JUnit rule and just write:

```java
@Rule
public final SDKIdlingRule mSdkIdlingRule = new SDKIdlingRule(SDKInstanceHolder.getInstance());

```

Now since this is an example, don't take it for granted; all code here is imaginary and used only for demonstration purposes:

```java
public class SDKIdlingRule implements TestRule {
    //idling resource you wrote to check is volley idle or not
    private VolleyIdlingResource mVolleyIdlingResource;
    //request queue that you need from volley to give it to idling resource
    private RequestQueue mRequestQueue;

    //when using the rule extract the request queue from your SDK
    public SDKIdlingRule(SDKClass sdkClass) {
        mRequestQueue = getVolleyRequestQueue(sdkClass);
    }

    private RequestQueue getVolleyRequestQueue(SDKClass sdkClass) {
        return sdkClass.getVolleyRequestQueue();
    }

    @Override
    public Statement apply(final Statement base, Description description) {
        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                //registering idling resource
                mVolleyIdlingResource = new VolleyIdlingResource(mRequestQueue);
                Espresso.registerIdlingResources(mVolleyIdlingResource);
                try {
                    base.evaluate();
                } finally {
                    if (mVolleyIdlingResource != null) {
                        //deregister the resource when test finishes
                        Espresso.unregisterIdlingResources(mVolleyIdlingResource);
                    }
                }
            }
        };
    }
}

```



#### Syntax


- **Idling resource**
- String    getName() - Returns the name of the idling resource(used for logging and idempotency of registration).
- boolean    isIdleNow() - Returns true if resource is currently idle.
- void    registerIdleTransitionCallback(IdlingResource.ResourceCallback callback) - Registers the given IdlingResource.ResourceCallback with the resource



#### Remarks


### JUnit rules:

As you can see in MockWebServer example and ActivityTestRule they all fall under category of JUnit rules which you can create yourself which then should be executed for each test defining its behaviour @see:
[https://github.com/junit-team/junit4/wiki/rules](https://github.com/junit-team/junit4/wiki/rules)

### Appium

### Parameters

Since parameters have some issues placing them here until documentation bug is resolved:

|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|Class activityClass|which activity to start
|initialTouchMode|should the activity be placed in touch mode on start: [https://android-developers.blogspot.de/2008/12/touch-mode.html](https://android-developers.blogspot.de/2008/12/touch-mode.html)
|launchActivity|true if the Activity should be launched once per Test method. It will be launched before the first Before method, and terminated after the last After method.

