---
metaTitle: "Android - Android Versions"
description: "Checking the Android Version on device at runtime"
---

# Android Versions



## Checking the Android Version on device at runtime


[`Build.VERSION_CODES`](https://developer.android.com/reference/android/os/Build.VERSION_CODES.html) is an enumeration of the currently known SDK version codes.

In order to conditionally run code based on the device's Android version, use the [`TargetApi`](https://developer.android.com/reference/android/annotation/TargetApi.html) annotation to avoid Lint errors, and check the build version before running the code specific to the API level.

Here is an example of how to use a class that was introduced in API-23, in a project that supports API levels lower than 23:

```java
@Override
@TargetApi(23)
public void onResume() {
    super.onResume();
    if (android.os.Build.VERSION.SDK_INT <= Build.VERSION_CODES.M) {
        //run Marshmallow code
        FingerprintManager fingerprintManager = this.getSystemService(FingerprintManager.class);
        //......................
    }
}

```



#### Remarks


|Name|Android version|Release date|API-level|Build.VERSION_CODES
|---|---|---|---|---|---|---|---|---|---
|Angel Cake (Alpha)|1.0|23 September 2008|1|BASE
|Battenberg (Beta)|1.1|9 February 2009|2|BASE_1_1
|Cupcake|1.5|30 April 2009|3|CUPCAKE
|Donut|1.6|15 September 2009|4|DONUT
|Eclair|2.0|26 October 2009|5|ECLAIR
||2.0.1|3 December 2009|6|ECLAIR_0_1
||2.1|12 January 2010|7|ECLAIR_MR1
|Froyo|2.2|20 May 2010|8|FROYO
|Gingerbread|2.3|6 December 2010|9|GINGERBREAD
||2.3.3|9 February 2011|10|GINGERBREAD_MR1
|Honeycomb|3.0|22 February 2011|11|HONEYCOMB
||3.1|10 May 2011|12|HONEYCOMB_MR2
||3.2|15 July 2011|13|HONEYCOMB_MR1
|Ice Cream Sandwich|4.0|19 October 2011|14|ICE_CREAM_SANDWICH
||4.0.3|16 December 2011|15|ICE_CREAM_SANDWICH_MR1
|Jelly Bean|4.1|9 July 2012|16|JELLY_BEAN
||4.2|13 November 2012|17|JELLY_BEAN_MR1
||4.3|24 July 2013|18|JELLY_BEAN_MR2
|KitKat|4.4|31 October 2013|19|KITKAT
|||25 July 2014|20|KITKAT_WATCH
|Lollipop|5.0|17 October 2014|21|LOLLIPOP
||5.1|9 March 2015|22|LOLLIPOP_MR1
|Marshmallow|6.0|5 October 2015|23|M
|Nougat|7.0|22 August 2016|24|N
||7.1.1|5 December 2016|25|N_MR1

