---
metaTitle: "Android - Accounts and AccountManager"
description: "Understanding custom accounts/authentication"
---

# Accounts and AccountManager




## Understanding custom accounts/authentication


The following example is high level coverage of the key concepts and basic skeletal setup:-

1. Collects credentials from the user (Usually from a login screen you've created)
1. Authenticates the credentials with the server (stores custom authentication)
1. Stores the credentials on the device

**Extend an AbstractAccountAuthenticator**
**(Primarily used to retrieve authentication & re-authenticate them)**

```java
public class AccountAuthenticator extends AbstractAccountAuthenticator {

  @Override
  public Bundle addAccount(AccountAuthenticatorResponse response, String accountType,
      String authTokenType, String[] requiredFeatures, Bundle options) {
    //intent to start the login activity
  }


  @Override
  public Bundle confirmCredentials(AccountAuthenticatorResponse response, Account account, Bundle options) {
  }

  @Override
  public Bundle editProperties(AccountAuthenticatorResponse response, String accountType) {
  }

  @Override
  public Bundle getAuthToken(AccountAuthenticatorResponse response, Account account, String authTokenType,
      Bundle options) throws NetworkErrorException {
    //retrieve authentication tokens from account manager storage or custom storage or re-authenticate old tokens and return new ones
  }

  @Override
  public String getAuthTokenLabel(String authTokenType) {
  }

  @Override
  public Bundle hasFeatures(AccountAuthenticatorResponse response, Account account, String[] features)
      throws NetworkErrorException {
    //check whether the account supports certain features
  }

  @Override
  public Bundle updateCredentials(AccountAuthenticatorResponse response, Account account, String authTokenType,
      Bundle options) {
    //when the user's session has expired or requires their previously available credentials to be updated, here is the function to do it.
  }
}

```

**Create a service**
**(Account Manager framework connects to the extended AbstractAccountAuthenticator through the service interface)**

```java
public class AuthenticatorService extends Service {

    private AccountAuthenticator authenticator;

    @Override
    public void onCreate(){
        authenticator = new AccountAuthenticator(this);
    }

    @Override
    public IBinder onBind(Intent intent) {
        return authenticator.getIBinder();
    }
}

```

**Authenticator XML configuration**
**(The account manager framework requires. This is what you'll see inside Settings -> Accounts in Android)**

```java
<account-authenticator xmlns:android="http://schemas.android.com/apk/res/android"
    android:accountType="rename.with.your.applicationid"
    android:icon="@drawable/app_icon"
    android:label="@string/app_name"
    android:smallIcon="@drawable/app_icon" />

```

**Changes to the AndroidManifest.xml**
**(Bring all the above concepts together to make it usable programmatically through the AccountManager)**

```java
<application
...>
    <service
        android:name=".authenticator.AccountAuthenticatorService"
        android:exported="false"
        android:process=":authentication">
        <intent-filter>
            <action android:name="android.accounts.AccountAuthenticator"/>
        </intent-filter>
        <meta-data
            android:name="android.accounts.AccountAuthenticator"
            android:resource="@xml/authenticator"/>
    </service>
</application>

```

The next example will contain how to make use of this setup.

