---
metaTitle: "Android - Displaying Google Ads"
description: "Basic Ad Setup, Adding Interstitial Ad"
---

# Displaying Google Ads



## Basic Ad Setup


You'll need to add the following to your dependencies:

`compile 'com.google.firebase:firebase-ads:10.2.1'`

and then put this in the same file.

`apply plugin: 'com.google.gms.google-services'`

Next you'll need to add relevant information into your strings.xml.

`<string name="banner_ad_unit_id">ca-app-pub-####/####</string>`

Next place an adview wherever you want it and style it just like any other view.

```

   <com.google.android.gms.ads.AdView
        android:id="@+id/adView"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_centerHorizontal="true"
        android:layout_alignParentBottom="true"
        ads:adSize="BANNER"
        ads:adUnitId="@string/banner_ad_unit_id">
    </com.google.android.gms.ads.AdView>

```

And last but not least, throw this in your onCreate.

```java
MobileAds.initialize(getApplicationContext(), "ca-app-pub-YOUR_ID");
AdView mAdView = (AdView) findViewById(R.id.adView);
    AdRequest adRequest = new AdRequest.Builder().build();
    mAdView.loadAd(adRequest);

```

If you copy-pasted exactly you should now have a small banner ad. Simply place more AdViews wherever you need them for more.



## Adding Interstitial Ad


[Interstitial ads](https://firebase.google.com/docs/admob/android/interstitial) are full-screen ads that cover the interface of their host app. They're typically displayed at natural transition points in the flow of an app, such as between activities or during the pause between levels in a game.

Make sure you have necessary permissions in your `Manifest` file:

```java
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />

```


<li>
Go to your [AdMob](https://google.com/admob/) account.
</li>
<li>
Click on **Monetize** tab.
</li>
<li>
Select or Create the app and choose the platform.
</li>
<li>
Select Interstitial and give an ad unit name.
</li>
<li>
Once the ad unit is created, you can notice the Ad unit ID on the dashboard. For example: ca-app-pub-00000000000/000000000
</li>
<li>
Add dependencies
</li>

```java
compile 'com.google.firebase:firebase-ads:10.2.1'

```

This one should be on the bottom.

```java
apply plugin: 'com.google.gms.google-services'

```

Add your **Ad unit ID** to your `strings.xml` file

```java
<string name="interstitial_full_screen">ca-app-pub-00000000/00000000</string>

```

Add ConfigChanges and meta-data to your manifest:

```java
<activity
            android:name="com.google.android.gms.ads.AdActivity"
            android:configChanges="keyboard|keyboardHidden|orientation|screenLayout|uiMode|screenSize|smallestScreenSize"
            android:theme="@android:style/Theme.Translucent" />

```

and

```java
<meta-data
            android:name="com.google.android.gms.version"
            android:value="@integer/google_play_services_version" />

```

Activity:

```java
public class AdActivity extends AppCompatActivity {
 
    private String TAG = AdActivity.class.getSimpleName();
    InterstitialAd mInterstitialAd;
 
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_second);
 
        mInterstitialAd = new InterstitialAd(this);
 
        // set the ad unit ID
        mInterstitialAd.setAdUnitId(getString(R.string.interstitial_full_screen));
 
        AdRequest adRequest = new AdRequest.Builder()
                .build();
 
        // Load ads into Interstitial Ads
        mInterstitialAd.loadAd(adRequest);
 
        mInterstitialAd.setAdListener(new AdListener() {
            public void onAdLoaded() {
                showInterstitial();
            }
        });
    }
 
    private void showInterstitial() {
        if (mInterstitialAd.isLoaded()) {
            mInterstitialAd.show();
        }
    }
 
}

```

This AdActivity will show a full screen ad now.

