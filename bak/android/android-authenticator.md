---
metaTitle: "Android - Android Authenticator"
description: "Basic Account Authenticator Service"
---

# Android Authenticator



## Basic Account Authenticator Service


The Android Account Authenticator system can be used to make the client authenticate with a remote server. Three pieces of information are required:

- A service, triggered by the `android.accounts.AccountAuthenticator`. Its `onBind` method should return a subclass of `AbstractAccountAuthenticator`.
- An activity to prompt the user for credentials (Login activity)
- An xml resource file to describe the account

**1. The service:**

Place the following permissions in your AndroidManifest.xml:

```java
<uses-permission android:name="android.permission.GET_ACCOUNTS" />
<uses-permission android:name="android.permission.MANAGE_ACCOUNTS" />
<uses-permission android:name="android.permission.AUTHENTICATE_ACCOUNTS" />
<uses-permission android:name="android.permission.USE_CREDENTIALS" />

```

Declare the service in the manifest file:

```java
<service android:name="com.example.MyAuthenticationService">
    <intent-filter>
        <action android:name="android.accounts.AccountAuthenticator" />
    </intent-filter>
    <meta-data
        android:name="android.accounts.AccountAuthenticator"
        android:resource="@xml/authenticator" />
</service>

```

Note that the `android.accounts.AccountAuthenticator` is included within the `intent-filter` tag. The xml resource (named `authenticator` here) is specified in the `meta-data` tag.

**The service class:**

```java
public class MyAuthenticationService extends Service {

    private static final Object lock = new Object();
    private MyAuthenticator mAuthenticator;

    public MyAuthenticationService() {
        super();
    }

    @Override
    public void onCreate() {
        super.onCreate();

        synchronized (lock) {
            if (mAuthenticator == null) {
                mAuthenticator = new MyAuthenticator(this);
            }
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        return mAuthenticator.getIBinder();
    }

}

```

**2. The xml resource:**

```java
<account-authenticator xmlns:android="http://schemas.android.com/apk/res/android"
    android:accountType="com.example.account"
    android:icon="@drawable/appicon"
    android:smallIcon="@drawable/appicon"
    android:label="@string/app_name" />

```

Do not directly assign a string to `android:label` or assign missing drawables. It will crash without warning.

**3. Extend the AbstractAccountAuthenticator class:**

```java
public class MyAuthenticator extends AbstractAccountAuthenticator {

    private Context mContext;

    public MyAuthenticator(Context context) {
        super(context);
        mContext = context;
    }

    @Override
    public Bundle addAccount(AccountAuthenticatorResponse response,
                             String accountType,
                             String authTokenType,
                             String[] requiredFeatures,
                             Bundle options) throws NetworkErrorException {

        Intent intent = new Intent(mContext, LoginActivity.class);
        intent.putExtra(AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);

        Bundle bundle = new Bundle();
        bundle.putParcelable(AccountManager.KEY_INTENT, intent);

        return bundle;
    }

    @Override
    public Bundle confirmCredentials(AccountAuthenticatorResponse response, Account account, Bundle options) throws NetworkErrorException {
        return null;
    }

    @Override
    public Bundle editProperties(AccountAuthenticatorResponse response, String accountType) {
        return null;
    }

    @Override
    public Bundle getAuthToken(AccountAuthenticatorResponse response, Account account, String authTokenType, Bundle options) throws NetworkErrorException {
        return null;
    }

    @Override
    public String getAuthTokenLabel(String authTokenType) {
        return null;
    }

    @Override
    public Bundle hasFeatures(AccountAuthenticatorResponse response, Account account, String[] features) throws NetworkErrorException {
        return null;
    }

    @Override
    public Bundle updateCredentials(AccountAuthenticatorResponse response, Account account, String authTokenType, Bundle options) throws NetworkErrorException {
        return null;
    }
}

```

The `addAccount()` method in `AbstractAccountAuthenticator` class is important as this method is called when adding an account from the "Add Account" screen in under settings. `AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE` is important, as it will include the AccountAuthenticatorResponse object that is needed to return the account keys upon successful user verification.

