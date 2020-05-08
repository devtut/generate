---
metaTitle: "Android - Location"
description: "Fused location API, Get Address From Location using Geocoder, Requesting location updates using LocationManager, Requesting location updates on a separate thread using LocationManager, Register geofence, Getting location updates in a BroadcastReceiver"
---

# Location


Android Location APIs are used in a wide variety of apps for different purposes such as finding user location, notifying when a user has left a general area (Geofencing), and help interpret user activity (walking, running, driving, etc).

However, Android Location APIs are not the only means of acquiring user location.  The following will give examples of how to use Android's `LocationManager` and other common location libraries.



## Fused location API


### Example Using Activity w/ LocationRequest

```java
/*
 * This example is useful if you only want to receive updates in this 
 * activity only, and have no use for location anywhere else.
 */
public class LocationActivity extends AppCompatActivity implements
        GoogleApiClient.ConnectionCallbacks, GoogleApiClient.OnConnectionFailedListener, LocationListener {

    private GoogleApiClient mGoogleApiClient;
    private LocationRequest mLocationRequest;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        mGoogleApiClient = new GoogleApiClient.Builder(this)
                .addConnectionCallbacks(this)
                .addOnConnectionFailedListener(this)
                .addApi(LocationServices.API)
                .build();

        mLocationRequest = new LocationRequest()
                .setPriority(LocationRequest.PRIORITY_HIGH_ACCURACY) //GPS quality location points
                .setInterval(2000) //At least once every 2 seconds
                .setFastestInterval(1000); //At most once a second
    }

    @Override
    protected void onStart(){
        super.onStart();
        mGoogleApiClient.connect();
    }
    
    @Override
    protected void onResume(){
        super.onResume();
        //Permission check for Android 6.0+
        if(ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            if(mGoogleApiClient.isConnected()) {
                LocationServices.FusedLocationApi.requestLocationUpdates(mGoogleApiClient, mLocationRequest, this);
            }
        }
    }
    
    @Override
    protected void onPause(){
        super.onPause();
        //Permission check for Android 6.0+
        if(ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            if(mGoogleApiClient.isConnected()) {
                LocationServices.FusedLocationApi.removeLocationUpdates(mGoogleApiClient, this);
            }
        }
    }
    
    @Override
    protected void onStop(){
        super.onStop();
        mGoogleApiClient.disconnect();
    }

    @Override
    public void onConnected(@Nullable Bundle bundle) {
        if(ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            LocationServices.FusedLocationApi.requestLocationUpdates(mGoogleApiClient, mLocationRequest, this);
        }
    }

    @Override
    public void onConnectionSuspended(int i) {
        mGoogleApiClient.connect();
    }

    @Override
    public void onConnectionFailed(@NonNull ConnectionResult connectionResult) {
    }

    @Override
    public void onLocationChanged(Location location) {
        //Handle your location update code here
    }
}

```

### Example Using Service w/ PendingIntent and BroadcastReceiver

**ExampleActivity**

Recommended reading: [LocalBroadcastManager](https://developer.android.com/reference/android/support/v4/content/LocalBroadcastManager.html)

```java
/*
 * This example is useful if you have many different classes that should be 
 * receiving location updates, but want more granular control over which ones
 * listen to the updates.
 * 
 * For example, this activity will stop getting updates when it is not visible, but a database
 * class with a registered local receiver will continue to receive updates, until "stopUpdates()" is called here.
 *
 */
public class ExampleActivity extends AppCompatActivity {

    private InternalLocationReceiver mInternalLocationReceiver;

    @Override
    protected void onCreate(Bundle savedInstanceState){
        super.onCreate(savedInstanceState);
        
        //Create internal receiver object in this method only.
        mInternalLocationReceiver = new InternalLocationReceiver(this);
    }

    @Override
    protected void onResume(){
        super.onResume();
        
        //Register to receive updates in activity only when activity is visible
        LocalBroadcastManager.getInstance(this).registerReceiver(mInternalLocationReceiver, new IntentFilter("googleLocation"));
    }

    @Override
    protected void onPause(){
        super.onPause();
        
        //Unregister to stop receiving updates in activity when it is not visible.
        //NOTE: You will still receive updates even if this activity is killed.
        LocalBroadcastManager.getInstance(this).unregisterReceiver(mInternalLocationReceiver);
    }

    //Helper method to get updates
    private void requestUpdates(){
        startService(new Intent(this, LocationService.class).putExtra("request", true));
    }

    //Helper method to stop updates
    private void stopUpdates(){
        startService(new Intent(this, LocationService.class).putExtra("remove", true));
    }

    /*
     * Internal receiver used to get location updates for this activity.
     * 
     * This receiver and any receiver registered with LocalBroadcastManager does
     * not need to be registered in the Manifest.
     *
     */
    private static class InternalLocationReceiver extends BroadcastReceiver{

        private ExampleActivity mActivity;

        InternalLocationReceiver(ExampleActivity activity){
            mActivity = activity;
        }

        @Override
        public void onReceive(Context context, Intent intent) {
            final ExampleActivity activity = mActivity;
            if(activity != null) {
                LocationResult result = intent.getParcelableExtra("result");
                //Handle location update here
            }
        }
    }
}

```

**LocationService**

NOTE: Don't forget to register this service in the Manifest!

```java
public class LocationService extends Service implements
        GoogleApiClient.ConnectionCallbacks, GoogleApiClient.OnConnectionFailedListener {

    private GoogleApiClient mGoogleApiClient;
    private LocationRequest mLocationRequest;

    @Override
    public void onCreate(){
        super.onCreate();
        mGoogleApiClient = new GoogleApiClient.Builder(this)
                .addConnectionCallbacks(this)
                .addOnConnectionFailedListener(this)
                .addApi(LocationServices.API)
                .build();

        mLocationRequest = new LocationRequest()
                .setPriority(LocationRequest.PRIORITY_HIGH_ACCURACY) //GPS quality location points
                .setInterval(2000) //At least once every 2 seconds
                .setFastestInterval(1000); //At most once a second
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId){
        super.onStartCommand(intent, flags, startId);
        //Permission check for Android 6.0+
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            if (intent.getBooleanExtra("request", false)) {
                if (mGoogleApiClient.isConnected()) {
                    LocationServices.FusedLocationApi.requestLocationUpdates(mGoogleApiClient, mLocationRequest, getPendingIntent());
                } else {
                    mGoogleApiClient.connect();
                }
            }
            else if(intent.getBooleanExtra("remove", false)){
                stopSelf();
            }
        }

        return START_STICKY;
    }

    @Override
    public void onDestroy(){
        super.onDestroy();
        if(mGoogleApiClient.isConnected()){
            LocationServices.FusedLocationApi.removeLocationUpdates(mGoogleApiClient, getPendingIntent());
            mGoogleApiClient.disconnect();
        }
    }
    
    private PendingIntent getPendingIntent(){
        
        //Example for IntentService
        //return PendingIntent.getService(this, 0, new Intent(this, **YOUR_INTENT_SERVICE_CLASS_HERE**), PendingIntent.FLAG_UPDATE_CURRENT);
        
        //Example for BroadcastReceiver
        return PendingIntent.getBroadcast(this, 0, new Intent(this, LocationReceiver.class), PendingIntent.FLAG_UPDATE_CURRENT);
    }

    @Override
    public void onConnected(@Nullable Bundle bundle) {
        //Permission check for Android 6.0+
        if(ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            LocationServices.FusedLocationApi.requestLocationUpdates(mGoogleApiClient, mLocationRequest, getPendingIntent());
        }
    }

    @Override
    public void onConnectionSuspended(int i) {
        mGoogleApiClient.connect();
    }

    @Override
    public void onConnectionFailed(@NonNull ConnectionResult connectionResult) {

    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}

```

**LocationReceiver**

NOTE: Don't forget to register this receiver in the Manifest!

```java
public class LocationReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        if(LocationResult.hasResult(intent)){
            LocationResult locationResult = LocationResult.extractResult(intent);
            LocalBroadcastManager.getInstance(context).sendBroadcast(new Intent("googleLocation").putExtra("result", locationResult));
        }
    }
}

```



## Get Address From Location using Geocoder


After you got the `Location` object from `FusedAPI`, you can easily acquire `Address` information from that object.

```java
private Address getCountryInfo(Location location) {
    Address address = null;
    Geocoder geocoder = new Geocoder(getActivity(), Locale.getDefault());
    String errorMessage;
    List<Address> addresses = null;
    try {
        addresses = geocoder.getFromLocation(
                location.getLatitude(),
                location.getLongitude(),
                // In this sample, get just a single address.
                1);
    } catch (IOException ioException) {
        // Catch network or other I/O problems.
        errorMessage = "IOException>>" + ioException.getMessage();
    } catch (IllegalArgumentException illegalArgumentException) {
        // Catch invalid latitude or longitude values.
        errorMessage = "IllegalArgumentException>>" + illegalArgumentException.getMessage();
    }
    if (addresses != null && !addresses.isEmpty()) {
        address = addresses.get(0);
    }
    return country;
}

```



## Requesting location updates using LocationManager


**As always, you need to make sure you have the required permissions.**

```java
public class MainActivity extends AppCompatActivity implements LocationListener{

    private LocationManager mLocationManager = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main2);

        mLocationManager = (LocationManager) getSystemService(Context.LOCATION_SERVICE);
    }


    @Override
    protected void onResume() {
        super.onResume();

        try {
            mLocationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, this);
        }
        catch(SecurityException e){
            // The app doesn't have the correct permissions
        }
    }


    @Override
    protected void onPause() {
        try{
            mLocationManager.removeUpdates(this);
        }
        catch (SecurityException e){
            // The app doesn't have the correct permissions
        }

        super.onPause();
    }




    @Override
    public void onLocationChanged(Location location) {
        // We received a location update!
        Log.i("onLocationChanged", location.toString());
    }

    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {

    }

    @Override
    public void onProviderEnabled(String provider) {

    }

    @Override
    public void onProviderDisabled(String provider) {

    }
}

```



## Requesting location updates on a separate thread using LocationManager


**As always, you need to make sure you have the required permissions.**

```java
public class MainActivity extends AppCompatActivity implements LocationListener{

    private LocationManager mLocationManager = null;
    HandlerThread mLocationHandlerThread = null;
    Looper mLocationHandlerLooper = null;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main2);

        mLocationManager = (LocationManager) getSystemService(Context.LOCATION_SERVICE);
        mLocationHandlerThread = new HandlerThread("locationHandlerThread");
    }


    @Override
    protected void onResume() {
        super.onResume();

        mLocationHandlerThread.start();
        mLocationHandlerLooper = mLocationHandlerThread.getLooper();

        try {
            mLocationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, this, mLocationHandlerLooper);
        }
        catch(SecurityException e){
            // The app doesn't have the correct permissions
        }
    }


    @Override
    protected void onPause() {
        try{
            mLocationManager.removeUpdates(this);
        }
        catch (SecurityException e){
            // The app doesn't have the correct permissions
        }

        mLocationHandlerLooper = null;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2)
            mLocationHandlerThread.quitSafely();
        else
            mLocationHandlerThread.quit();

        mLocationHandlerThread = null;


        super.onPause();
    }




    @Override
    public void onLocationChanged(Location location) {
        // We received a location update on a separate thread!
        Log.i("onLocationChanged", location.toString());

        // You can verify which thread you're on by something like this:
        // Log.d("Which thread?", Thread.currentThread() == Looper.getMainLooper().getThread() ? "UI Thread" : "New thread");
    }

    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {

    }

    @Override
    public void onProviderEnabled(String provider) {

    }

    @Override
    public void onProviderDisabled(String provider) {

    }
}

```



## Register geofence


I have created `GeoFenceObserversationService` **singleton** class.

**GeoFenceObserversationService.java**:

```java
public class GeoFenceObserversationService extends Service implements GoogleApiClient.ConnectionCallbacks, GoogleApiClient.OnConnectionFailedListener, ResultCallback<Status> {

    protected static final String TAG = "GeoFenceObserversationService";
    protected GoogleApiClient mGoogleApiClient;
    protected ArrayList<Geofence> mGeofenceList;
    private boolean mGeofencesAdded;
    private SharedPreferences mSharedPreferences;
    private static GeoFenceObserversationService mInstant;
    public static GeoFenceObserversationService getInstant(){
        return mInstant;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mInstant = this;
        mGeofenceList = new ArrayList<Geofence>();
        mSharedPreferences = getSharedPreferences(AppConstants.SHARED_PREFERENCES_NAME, MODE_PRIVATE);
        mGeofencesAdded = mSharedPreferences.getBoolean(AppConstants.GEOFENCES_ADDED_KEY, false);

        buildGoogleApiClient();
    }


    @Override
    public void onDestroy() {
        mGoogleApiClient.disconnect();
        super.onDestroy();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    protected void buildGoogleApiClient() {
        mGoogleApiClient = new GoogleApiClient.Builder(this)
                .addConnectionCallbacks(this)
                .addOnConnectionFailedListener(this)
                .addApi(LocationServices.API)
                .build();
        mGoogleApiClient.connect();
    }

    @Override
    public void onConnected(Bundle connectionHint) {
    }

    @Override
    public void onConnectionFailed(ConnectionResult result) {
    }

    @Override
    public void onConnectionSuspended(int cause) {

    }

    private GeofencingRequest getGeofencingRequest() {

        GeofencingRequest.Builder builder = new GeofencingRequest.Builder();
        builder.setInitialTrigger(GeofencingRequest.INITIAL_TRIGGER_ENTER);
        builder.addGeofences(mGeofenceList);
        return builder.build();
    }


    public void addGeofences() {
            if (!mGoogleApiClient.isConnected()) {
                Toast.makeText(this, getString(R.string.not_connected), Toast.LENGTH_SHORT).show();
                return;
            }

        populateGeofenceList();
        if(!mGeofenceList.isEmpty()){
            try {
                LocationServices.GeofencingApi.addGeofences(mGoogleApiClient, getGeofencingRequest(), getGeofencePendingIntent()).setResultCallback(this);
            } catch (SecurityException securityException) {
                securityException.printStackTrace();
            }
        }

        }

    public void removeGeofences() {
        if (!mGoogleApiClient.isConnected()) {
            Toast.makeText(this, getString(R.string.not_connected), Toast.LENGTH_SHORT).show();
            return;
        }
        try {
            LocationServices.GeofencingApi.removeGeofences(mGoogleApiClient,getGeofencePendingIntent()).setResultCallback(this);
        } catch (SecurityException securityException) {
            securityException.printStackTrace();
        }
    }


    public void onResult(Status status) {

        if (status.isSuccess()) {
            mGeofencesAdded = !mGeofencesAdded;
            SharedPreferences.Editor editor = mSharedPreferences.edit();
            editor.putBoolean(AppConstants.GEOFENCES_ADDED_KEY, mGeofencesAdded);
            editor.apply();
        } else {
            String errorMessage = AppConstants.getErrorString(this,status.getStatusCode());
            Log.i("Geofence", errorMessage);
        }
    }

    private PendingIntent getGeofencePendingIntent() {
        Intent intent = new Intent(this, GeofenceTransitionsIntentService.class);
        return PendingIntent.getService(this, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT);
    }

    private void populateGeofenceList() {
        mGeofenceList.clear();
        List<GeoFencingResponce> geoFenceList = getGeofencesList;
        if(geoFenceList!=null&&!geoFenceList.isEmpty()){
            for (GeoFencingResponce obj : geoFenceList){
                mGeofenceList.add(obj.getGeofence());
                Log.i(TAG,"Registered Geofences : " + obj.Id+"-"+obj.Name+"-"+obj.Lattitude+"-"+obj.Longitude);
            }
        }
    }
}

```

**AppConstant**:

```

  public static final String SHARED_PREFERENCES_NAME = PACKAGE_NAME + ".SHARED_PREFERENCES_NAME";
    public static final String GEOFENCES_ADDED_KEY = PACKAGE_NAME + ".GEOFENCES_ADDED_KEY";
    public static final String DETECTED_GEOFENCES = "detected_geofences";
    public static final String DETECTED_BEACONS = "detected_beacons";

    public static String getErrorString(Context context, int errorCode) {
        Resources mResources = context.getResources();
        switch (errorCode) {
            case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE:
                return mResources.getString(R.string.geofence_not_available);
            case GeofenceStatusCodes.GEOFENCE_TOO_MANY_GEOFENCES:
                return mResources.getString(R.string.geofence_too_many_geofences);
            case GeofenceStatusCodes.GEOFENCE_TOO_MANY_PENDING_INTENTS:
                return mResources.getString(R.string.geofence_too_many_pending_intents);
            default:
                return mResources.getString(R.string.unknown_geofence_error);
        }
    }

```

Where I started Service ? From Application class

- `startService(new Intent(getApplicationContext(),GeoFenceObserversationService.class));`

How I registered Geofences ?

- `GeoFenceObserversationService.getInstant().addGeofences();`



## Getting location updates in a BroadcastReceiver


First create a BroadcastReceiver class to handle the incoming Location updates:

```java
public class LocationReceiver extends BroadcastReceiver implements Constants {

    @Override
     public void onReceive(Context context, Intent intent) {

        if (LocationResult.hasResult(intent)) {
            LocationResult locationResult = LocationResult.extractResult(intent);
            Location location = locationResult.getLastLocation();
            if (location != null) {
                // Do something with your location
            } else {
                Log.d(LocationReceiver.class.getSimpleName(), "*** location object is null ***");
            }
        }
    }
}

```

Then when you connect to the GoogleApiClient in the onConnected callback:

```java
@Override
public void onConnected(Bundle connectionHint) {
    Intent backgroundIntent = new Intent(this, LocationReceiver.class);
    mBackgroundPendingIntent = backgroundPendingIntent.getBroadcast(getApplicationContext(), LOCATION_REUEST_CODE, backgroundIntent, PendingIntent.FLAG_CANCEL_CURRENT);
    mFusedLocationProviderApi.requestLocationUpdates(mLocationClient, mLocationRequest, backgroundPendingIntent);
}

```

Don't forget to remove the location update intent in the appropriate lifecycle callback:

```java
@Override
public void onDestroy() {
    if (servicesAvailable && mLocationClient != null) {
        if (mLocationClient.isConnected()) {
            fusedLocationProviderApi.removeLocationUpdates(mLocationClient, backgroundPendingIntent);
            // Destroy the current location client
            mLocationClient = null;
        } else {
            mLocationClient.unregisterConnectionCallbacks(this);
            mLocationClient = null;
        }
    }
    super.onDestroy();
}

```



#### Remarks


For building Location aware apps in Android, there are two paths:

- Android's native open source [`LocationManager`](https://developer.android.com/reference/android/location/LocationManager.html)
- Google's [`FusedLocationProviderApi`](https://developers.google.com/android/reference/com/google/android/gms/location/FusedLocationProviderApi), which is part of Google Play Services

### LocationManager

**Pros**

- More granular control
- Available in all devices
- Part of the Android framework

**Cons**

- Battery drain is an issue, if not managed properly
- Requires logic to switch location providers, if the device is unable to find a location (ex. poor GPS inside a building)

**Features**

- NMEA Listener
- GPS Status Listener
- Listen to provider status changes (ex. GPS is turned off by user)
- List of providers to choose location source from

**Providers**

**GPS**

<li>Permissions Required:
<ul>
- [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION)

- Location updates typically come in once every second, but in situations where GPS has not been used for some time and [A-GPS](https://en.wikipedia.org/wiki/Assisted_GPS) is unavailable, it make take several minutes for a location to be received.
- In cases where clear view of the sky is obstructed, GPS points will not cluster very well (location points "jump") and accuracy may be misleading in certain areas due to the "[Urban Canyon](https://en.wikipedia.org/wiki/Street_canyon)" effect.

**Network**

<li>Permissions Required:
<ul>
- [`ACCESS_COARSE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_COARSE_LOCATION) or [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION)

- Location updates occur less frequently than GPS
- Location updates typically do not cluster well (location points "jump") and accuracy can range depending number of different factors (number of wifi signals, signal strength, type of cell tower, etc.)

**Passive**

<li>Permissions Required:
<ul>
- [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION)

- Don't rely on this to give you continuous updates.  This listens passively to other apps that make location requests, and passes those locations back.
- Does not return FusedLocationProviderApi generated points, only the underlying location points used to generate them.

### FusedLocationProviderApi

**Pros**

- Offers less battery drain "out of the box"
- Handles poor GPS well
- Gets updates more regularly

**Cons**

- Less granular control over GPS
- May not be available on all devices or in certain countries
- Requires third party library dependency

**Features**

- Well managed use of location providers for optimal batter savings
- Typically generates more accurate points then Network Location Provider
- More frequent updates of library, allowing for more improvement
- No need to specify what type of provider to use

**LocationRequest Priority Levels**

**PRIORITY_HIGH_ACCURACY**

<li>Permissions Required:
<ul>
- [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION) for more accurate location or [`ACCESS_COARSE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_COARSE_LOCATION) for less accurate location

- If [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION) is not used, this will not use GPS for generating location updates, but will still find a fairly accurate point in the right conditions.
- If [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION) is used, it may or may not use GPS to generate location points, depending on how accurate it can currently track the device given the environmental conditions.
- Although this may report more accurate location updates than the other settings, it is still prone to the "[Urban Canyon](https://en.wikipedia.org/wiki/Street_canyon)" effect.

**PRIORITY_BALANCED_POWER_ACCURACY**

<li>Permissions Required:
<ul>
- [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION) for more accurate location or [`ACCESS_COARSE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_COARSE_LOCATION) for less accurate location

- Same notes as `PRIORITY_HIGH_ACCURACY`
- Although it is unlikely, this setting may still use GPS to generate a location.

**PRIORITY_LOW_POWER**

<li>Permissions Required:
<ul>
- [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION) or [`ACCESS_COARSE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_COARSE_LOCATION)

- Probably does not use GPS, but untested thus far.
- Updates are typically not very accurate
- Used generally to detect significant changes in location

**PRIORITY_NO_POWER**

<li>Permissions Required:
<ul>
- [`ACCESS_FINE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_FINE_LOCATION) or [`ACCESS_COARSE_LOCATION`](https://developer.android.com/reference/android/Manifest.permission.html#ACCESS_COARSE_LOCATION)

- Functions almost identically to `LocationManager` `PASSIVE_PROVIDER`
- Reports back Google Play Services updates when received, where `PASSIVE_PROVIDER` reports back underlying location updates used

### TroubleShooting

**OnLocationChanged() Never Called**

Since this seems to be a common issue with getting Android Locations, I'll put down a quick checklist of common fixes:

<li>
Check your manifest!
One of the most common issues is that the right permissions were never given. If you are using GPS (with or without Network), use `<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION"/>`, else use `<uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION"/>`. Google's FusedLocationApi requires `ACCESS_FINE_LOCATION`.
</li>

<li>
(For Android 6+) [Check runtime permissions](https://developer.android.com/training/permissions/requesting.html)!
Check for and request permissions! If you are never given permissions, you'll end up with crashes, or worse (if you are catching all exceptions), you'll end up with no indication of anything! It doesn't matter if the user grants you permission at the start of the app, always check to see if you have permissions for all calls. The user can easily go to their settings and revoke them.
</li>

<li>
Double check your code!
Are you sure you are passing in the right listener? Did you add that `BroadcastReceiver` or `IntentService` to your manifest? Are you using `PendingIntent.getService()` on a `BroadcastReceiver` class, or `getBroadcast()` on an `IntentService` class? Are you sure you are not unregistering your listener somewhere else in your code immediately after requesting?
</li>

<li>
Check device settings!
Obviously, make sure you have location services turned on.
[<img src="https://i.stack.imgur.com/XDsXs.jpg" alt="enter image description here" />](https://i.stack.imgur.com/XDsXs.jpg)
If you are using Network services, did you turn on "Scanning Always Available"? Do you have your location mode set to "Best" ("High Accuracy") or "Battery Saving" ("Network Only")?
[<img src="https://i.stack.imgur.com/IJpXQ.jpg" alt="enter image description here" />](https://i.stack.imgur.com/IJpXQ.jpg)
If you are using GPS, did you turn on "Best" ("High Accuracy") or "Device only" in location mode?
[<img src="https://i.stack.imgur.com/wgBuU.jpg" alt="enter image description here" />](https://i.stack.imgur.com/wgBuU.jpg)
</li>

<li>
Double check your code!
Yes, this is on here twice. Did you try using a `LocationListener` instead of a `PendingIntent`, or vice-versa, to ensure you actually implemented `LocationManager` properly? Are you sure that the location request isn't being removed in some part of the Activity or Service lifecycle that you didn't expect to happen?
</li>

<li>
Check your surroundings!
Are you testing GPS on the first floor of a building in the middle of San Francisco? Are you testing Network locations in the middle of nowhere? Do you work in a secret underground bunker void of all radio signals, wondering why your device isn't getting location? Always double check your surroundings when trying to troubleshoot location problems!
</li>

There could be many other less obvious reasons why location isn't working, but before searching out those esoteric fixes, just run through this quick checklist.

