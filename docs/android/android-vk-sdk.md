---
metaTitle: "Android - Android Vk Sdk"
description: "Initialization and login"
---

# Android Vk Sdk



## Initialization and login


1. Create a new application here: [create application](https://vk.com/editapp?act=create)
1. Choose standalone applicaton and confirm app creation via SMS.
1. Fill **Package namefor Android** as your current package name. **You can get your package name inside android manifest file, at the very begginning.**
1. Get your **Certificate fingerprint** by executing this command in your shell/cmd:

```java
keytool -exportcert -alias androiddebugkey -keystore path-to-debug-or-production-keystore -list -v

```

You can also get this fingerprint by SDK itself:

```java
String[] fingerprints = VKUtil.getCertificateFingerprint(this, this.getPackageName());
Log.d("MainActivity", fingerprints[0]);

```


<li>
Add recieved fingerprint into your **Signing certificate fingerprint for Android:** field in Vk app settings (where you entered your package name)
</li>
<li>
Then add this to your gradle file:
</li>

```java
compile 'com.vk:androidsdk:1.6.5'

```


1. Initialize the SDK on startup using the following method. The best way is to call it in the Applications onCreate method.

```java
private static final int VK_ID = your_vk_id;
public static final String VK_API_VERSION = "5.52"; //current version
@Override
    public void onCreate() {
        super.onCreate();
        VKSdk.customInitialize(this, VK_ID, VK_API_VERSION);
}

```

This is the best way to initizlize VKSdk.  Don't use the methid where VK_ID should be placed inside strings.xml because api will not work correctly after it.

1. Final step is to login using vksdk.

```

   public static final String[] VK_SCOPES = new String[]{
            VKScope.FRIENDS,
            VKScope.MESSAGES,
            VKScope.NOTIFICATIONS,
            VKScope.OFFLINE,
            VKScope.STATUS,
            VKScope.STATS,
            VKScope.PHOTOS
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    
        someButtonForLogin.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
               VKSdk.login(this, VK_SCOPES); 
            }
        });
 
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        VKSdk.onActivityResult(requestCode, resultCode, data, new VKCallback<VKAccessToken>() {
            @Override
            public void onResult(VKAccessToken res) {
                res.accessToken; //getting our token here.
            }

            @Override
            public void onError(VKError error) {
                Toast.makeText(SocialNetworkChooseActivity.this,
                        "User didn't pass Authorization", Toast.LENGTH_SHORT).show();
            }
        });
    }

```

