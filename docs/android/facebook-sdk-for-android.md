---
metaTitle: "Android - Facebook SDK for Android"
description: "How to add Facebook Login  in Android, Create your own custom button for Facebook login, A minimalistic guide to Facebook login/signup implementation, Setting permissions to access data from the Facebook profile, Logging out of Facebook"
---

# Facebook SDK for Android



## How to add Facebook Login  in Android


Add below dependencies to your `build.gradle`

```

 // Facebook login
    compile 'com.facebook.android:facebook-android-sdk:4.21.1'

```

Add below helper class to your utility package:

```java
/**
 * Created by Andy
 * An utility for Facebook
 */
public class FacebookSignInHelper {
    private static final String TAG = FacebookSignInHelper.class.getSimpleName();
    private static FacebookSignInHelper facebookSignInHelper = null;
    private CallbackManager callbackManager;
    private Activity mActivity;
    private static final Collection<String> PERMISSION_LOGIN = (Collection<String>) Arrays.asList("public_profile", "user_friends","email");
    private FacebookCallback<LoginResult> loginCallback;



    public static FacebookSignInHelper newInstance(Activity context) {
        if (facebookSignInHelper == null)
            facebookSignInHelper = new FacebookSignInHelper(context);
        return facebookSignInHelper;
    }


    public FacebookSignInHelper(Activity mActivity) {
        try {
            this.mActivity = mActivity;
            // Initialize the SDK before executing any other operations,
            // especially, if you're using Facebook UI elements.
            FacebookSdk.sdkInitialize(this.mActivity);
            callbackManager = CallbackManager.Factory.create();
            loginCallback = new FacebookCallback<LoginResult>() {
                @Override
                public void onSuccess(LoginResult loginResult) {
                   // You are logged into Facebook
                }

                @Override
                public void onCancel() {
                    Log.d(TAG, "Facebook: Cancelled by user");
                }

                @Override
                public void onError(FacebookException error) {
                    Log.d(TAG, "FacebookException: " + error.getMessage());
                }
            };
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * To login user on facebook without default Facebook button
     */
    public void loginUser() {
        try {
            LoginManager.getInstance().registerCallback(callbackManager, loginCallback);
            LoginManager.getInstance().logInWithReadPermissions(this.mActivity, PERMISSION_LOGIN);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    

    /**
     * To log out user from facebook
     */
    public void signOut() {
        // Facebook sign out
        LoginManager.getInstance().logOut();
    }

    public CallbackManager getCallbackManager() {
        return callbackManager;
    }

    public FacebookCallback<LoginResult> getLoginCallback() {
        return loginCallback;
    }

    /**
     * Attempts to log debug key hash for facebook
     *
     * @param context : A reference to context
     * @return : A facebook debug key hash
     */
    public static String getKeyHash(Context context) {
        String keyHash = null;
        try {
            PackageInfo info = context.getPackageManager().getPackageInfo(
                    context.getPackageName(),
                    PackageManager.GET_SIGNATURES);
            for (Signature signature : info.signatures) {
                MessageDigest md = MessageDigest.getInstance("SHA");
                md.update(signature.toByteArray());
                keyHash = Base64.encodeToString(md.digest(), Base64.DEFAULT);
                Log.d(TAG, "KeyHash:" + keyHash);
            }
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return keyHash;
    }
}

```

Add below code in Your Activity:

```java
FacebookSignInHelper facebookSignInHelper = FacebookSignInHelper.newInstance(LoginActivity.this, fireBaseAuthHelper);
facebookSignInHelper.loginUser();

```

Add below code to your `OnActivityResult`:

```

facebookSignInHelper.getCallbackManager().onActivityResult(requestCode, resultCode, data);

```



## Create your own custom button for Facebook login


Once you first add the Facebook login/signup, the button looks something like:

[<img src="http://i.stack.imgur.com/nPhcD.png" alt="enter image description here" />](http://i.stack.imgur.com/nPhcD.png)

Most of the times, it doesn't match with the design-specs of your app. And here's how you can customize it:

```java
<FrameLayout
    android:layout_below="@+id/no_network_bar"
    android:id="@+id/FrameLayout1"
    android:layout_width="match_parent"
    android:layout_height="wrap_content">

    <com.facebook.login.widget.LoginButton
        android:id="@+id/login_button"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:visibility="gone" />

    <Button
        android:background="#3B5998"
        android:layout_width="match_parent"
        android:layout_height="60dp"
        android:id="@+id/fb"
        android:onClick="onClickFacebookButton"
        android:textAllCaps="false"
        android:text="Sign up with Facebook"
        android:textSize="22sp"
        android:textColor="#ffffff" />
</FrameLayout>

```

Just wrap the original `com.facebook.login.widget.LoginButton` into a `FrameLayout` and make its visibility gone.

Next, add your custom button in the same `FrameLayout`. I've added some sample specs. You can always make your own drawable background for the facebook button and set it as the background of the button.

The final thing we do is simply convert the click on my custom button to a click on the facecbook button:

```java
//The original Facebook button
LoginButton loginButton = (LoginButton)findViewById(R.id.login_button);

//Our custom Facebook button
fb = (Button) findViewById(R.id.fb);

public void onClickFacebookButton(View view) {
    if (view == fb) {
        loginButton.performClick();
    }
}

```

Great! Now the button looks something like this:

[<img src="http://i.stack.imgur.com/syWnM.png" alt="enter image description here" />](http://i.stack.imgur.com/syWnM.png)



## A minimalistic guide to Facebook login/signup implementation


<li>
You have to setup the [prerequisites](https://developers.facebook.com/docs/facebook-login/android#prerequisites).
</li>
<li>
Add the Facebook activity to the **AndroidManifest.xml** file:

```java
<activity 
    android:name="com.facebook.FacebookActivity"
    android:configChanges= "keyboard|keyboardHidden|screenLayout|screenSize|orientation"
    android:theme="@android:style/Theme.Translucent.NoTitleBar"
    android:label="@string/app_name" />

```


</li>
<li>
Add the login button to your layout XML file:

```java
<com.facebook.login.widget.LoginButton
    android:id="@+id/login_button"
    android:layout_width="wrap_content"
    android:layout_height="wrap_content" />   

```


</li>
<li>
Now you have the Facebook button. If the user clicks on it, the Facebook login dialog will come up on top of the app's screen. Here the user can fill in their credentials and press the **Log In** button. If the credentials are correct, the dialog grants the corresponding permissions and a callback is sent to your original activity containing the button. The following code shows how you can receive that callback:

```java
loginButton.registerCallback(callbackManager, new FacebookCallback<LoginResult>() {
    @Override
    public void onSuccess(LoginResult loginResult) {
        // Completed without error. You might want to use the retrieved data here.
    }

    @Override
    public void onCancel() {
        // The user either cancelled the Facebook login process or didn't authorize the app.
    }

    @Override
    public void onError(FacebookException exception) {
        // The dialog was closed with an error. The exception will help you recognize what exactly went wrong.
    }
});  

```


</li>



## Setting permissions to access data from the Facebook profile


If you want to retrieve the details of a user's Facebook profile, you need to set permissions for the same:

```java
loginButton = (LoginButton)findViewById(R.id.login_button);

loginButton.setReadPermissions(Arrays.asList("email", "user_about_me"));

```

You can keep adding more permissions like friends-list, posts, photos etc. Just pick [the right permission](https://developers.facebook.com/docs/facebook-login/permissions) and add it the above list.

Note: You don't need to set any explicit permissions for accessing the public profile (first name, last name, id, gender etc).



## Logging out of Facebook


Facebook SDK 4.0 onwards, this is how we logout:

```java
com.facebook.login.LoginManager.getInstance().logOut();

```

For versions before 4.0, the logging out is gone by explicitly clearing the access token:

```java
Session session = Session.getActiveSession();
session.closeAndClearTokenInformation();

```



#### Syntax


- **newInstance** : To create single instance of Facebook helper class.
- **loginUser** : To login user.
- **signOut** : To log out user.
- **getCallbackManager** : To get callback for Facebook.
- **getLoginCallback** : To get callback for Login.
- **getKeyHash** : To generate Facebook Key Hash.



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|TAG|A String used while logging
|FacebookSignInHelper|A static reference to facebook helper
|CallbackManager|A callback for facebook operations
|Activity|A context
|PERMISSION_LOGIN|An array that contains all permission required from facebook to login.
|loginCallback|A callback for facebook login

