---
metaTitle: "Android - AdMob"
description: "Implementing"
---

# AdMob



## Implementing


**Note: This example requires a valid Admob account and valid Admob ad code.**

### Build.gradle on app level

Change to the latest version if existing:

```java
compile 'com.google.firebase:firebase-ads:10.2.1'

```

### Manifest

Internet permission is required to access the ad data. Note that this permission does not have to be requested (using API 23+) as it is a normal permission and not dangerous:

```java
<uses-permission android:name="android.permission.INTERNET" />

```

### XML

The following XML example shows a banner ad:

```java
<com.google.android.gms.ads.AdView
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:id="@+id/adView"
    ads:adSize="BANNER"
    ads:adUnitId="@string/main_screen_ad" />

```

For the code of other types, refer to the [Google AdMob Help](https://support.google.com/admob/answer/6128738?hl=en).

### Java

The following code is for the integration of banner ads. Note that other ad types may require different integration:

```java
// Alternative for faster initialization.
// MobileAds.initialize(getApplicationContext(), "AD_UNIT_ID");

AdView mAdView = (AdView) findViewById(R.id.adView);
// Add your device test ID if you are doing testing before releasing.
// The device test ID can be found in the admob stacktrace.
AdRequest adRequest = new AdRequest.Builder().build();
mAdView.loadAd(adRequest);

```

Add the `AdView` life cycle methods in the `onResume()`, `onPause()`, and `onDestroy()` methods of your activity:

```java
@Override
public void onPause() {
    if (mAdView != null) {
        mAdView.pause();
    }
    super.onPause();
}

@Override
public void onResume() {
    super.onResume();
    if (mAdView != null) {
        mAdView.resume();
    }
}

@Override
public void onDestroy() {
    if (mAdView != null) {
        mAdView.destroy();
    }
    super.onDestroy();
}

```



#### Syntax


- compile 'com.google.firebase:firebase-ads:10.2.1' //NOTE: SET TO NEWEST VERSION IF AVAILABLE
- `<uses-permission android:name="android.permission.INTERNET" />` Required to retrieve the ad
- AdRequest adRequest = new AdRequest.Builder().build();//Banner ad
- AdView mAdView = (AdView) findViewById(R.id.adView);//Banner ad
- mAdView.loadAd(adRequest);//Banner ad



#### Parameters


|Param|Details
|---|---|---|---|---|---|---|---|---|---
|ads:adUnitId="@string/main_screen_ad"|The ID of your ad. Get your ID from the admob site. **"While it's not a requirement, storing your ad unit ID values in a resource file is a good practice. As your app grows and your ad publishing needs mature, it may be necessary to change the ID values. If you keep them in a resource file, you never have to search through your code looking for them."**.[[1](https://firebase.google.com/docs/admob/android/quick-start)]



#### Remarks


- Requires a valid Admob account
- Read the [admob policy](https://support.google.com/admob/answer/6128543?hl=en). Make sure you do not do anything that can get your admob account suspended

