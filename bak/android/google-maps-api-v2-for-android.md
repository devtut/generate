---
metaTitle: "Android - Google Maps API v2 for Android"
description: "Custom Google Map Styles, Default Google Map Activity, Show Current Location in a Google Map, Adding markers to a map, MapView: embedding a GoogleMap in an existing layout, Obtaining the SH1-Fingerprint of your certificate keystore file, Do not launch Google Maps  when the map is clicked (lite mode), UISettings, Get debug SHA1 fingerprint, InfoWindow Click Listener, Change Offset"
---

# Google Maps API v2 for Android




## Custom Google Map Styles


**Map Style**

Google Maps come with a set of different styles to be applied, using this code :

```java
// Sets the map type to be "hybrid"
map.setMapType(GoogleMap.MAP_TYPE_HYBRID);

```

The different map styles are :

**Normal**

```java
map.setMapType(GoogleMap.MAP_TYPE_NORMAL);

```

Typical road map. Roads, some man-made features, and important natural features such as rivers are shown. Road and feature labels are also visible.

[<img src="https://i.stack.imgur.com/KhPjr.png" alt="enter image description here" />](https://i.stack.imgur.com/KhPjr.png)

**Hybrid**

```java
map.setMapType(GoogleMap.MAP_TYPE_HYBRID);

```

Satellite photograph data with road maps added. Road and feature labels are also visible.

[<img src="https://i.stack.imgur.com/7s9r1.jpg" alt="enter image description here" />](https://i.stack.imgur.com/7s9r1.jpg)

**Satellite**

```java
map.setMapType(GoogleMap.MAP_TYPE_SATELLITE);

```

Satellite photograph data. Road and feature labels are not visible.

[<img src="https://i.stack.imgur.com/qmMzD.jpg" alt="enter image description here" />](https://i.stack.imgur.com/qmMzD.jpg)

**Terrain**

```java
map.setMapType(GoogleMap.MAP_TYPE_TERRAIN);

```

Topographic data. The map includes colors, contour lines and labels, and perspective shading. Some roads and labels are also visible.

[<img src="https://i.stack.imgur.com/cRKSl.jpg" alt="enter image description here" />](https://i.stack.imgur.com/cRKSl.jpg)

**None**

```java
map.setMapType(GoogleMap.MAP_TYPE_NONE);

```

No tiles. The map will be rendered as an empty grid with no tiles loaded.

[<img src="https://i.stack.imgur.com/HO8Bj.png" alt="enter image description here" />](https://i.stack.imgur.com/HO8Bj.png)

****OTHER STYLE OPTIONS****

**Indoor Maps**

At high zoom levels, the map will show floor plans for indoor spaces. These are called indoor maps, and are displayed only for the 'normal' and 'satellite' map types.

to enable or disable indoor maps, this is how it's done :

```java
GoogleMap.setIndoorEnabled(true).
GoogleMap.setIndoorEnabled(false).

```

**We can add custom styles to maps.**

In onMapReady method add the following code snippet

```java
mMap = googleMap;
    try {
        // Customise the styling of the base map using a JSON object defined
        // in a raw resource file.
        boolean success = mMap.setMapStyle(
                MapStyleOptions.loadRawResourceStyle(
                        MapsActivity.this, R.raw.style_json));

        if (!success) {
            Log.e(TAG, "Style parsing failed.");
        }
    } catch (Resources.NotFoundException e) {
        Log.e(TAG, "Can't find style.", e);
    }

```

under **res** folder create a folder name **raw** and add the styles json file.
Sample style.json file

```

   [
  {
    "featureType": "all",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#242f3e"
      }
    ]
  },
  {
    "featureType": "all",
    "elementType": "labels.text.stroke",
    "stylers": [
      {
        "lightness": -80
      }
    ]
  },
  {
    "featureType": "administrative",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#746855"
      }
    ]
  },
  {
    "featureType": "administrative.locality",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#d59563"
      }
    ]
  },
  {
    "featureType": "poi",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#d59563"
      }
    ]
  },
  {
    "featureType": "poi.park",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#263c3f"
      }
    ]
  },
  {
    "featureType": "poi.park",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#6b9a76"
      }
    ]
  },
  {
    "featureType": "road",
    "elementType": "geometry.fill",
    "stylers": [
      {
        "color": "#2b3544"
      }
    ]
  },
  {
    "featureType": "road",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#9ca5b3"
      }
    ]
  },
  {
    "featureType": "road.arterial",
    "elementType": "geometry.fill",
    "stylers": [
      {
        "color": "#38414e"
      }
    ]
  },
  {
    "featureType": "road.arterial",
    "elementType": "geometry.stroke",
    "stylers": [
      {
        "color": "#212a37"
      }
    ]
  },
  {
    "featureType": "road.highway",
    "elementType": "geometry.fill",
    "stylers": [
      {
        "color": "#746855"
      }
    ]
  },
  {
    "featureType": "road.highway",
    "elementType": "geometry.stroke",
    "stylers": [
      {
        "color": "#1f2835"
      }
    ]
  },
  {
    "featureType": "road.highway",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#f3d19c"
      }
    ]
  },
  {
    "featureType": "road.local",
    "elementType": "geometry.fill",
    "stylers": [
      {
        "color": "#38414e"
      }
    ]
  },
  {
    "featureType": "road.local",
    "elementType": "geometry.stroke",
    "stylers": [
      {
        "color": "#212a37"
      }
    ]
  },
  {
    "featureType": "transit",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#2f3948"
      }
    ]
  },
  {
    "featureType": "transit.station",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#d59563"
      }
    ]
  },
  {
    "featureType": "water",
    "elementType": "geometry",
    "stylers": [
      {
        "color": "#17263c"
      }
    ]
  },
  {
    "featureType": "water",
    "elementType": "labels.text.fill",
    "stylers": [
      {
        "color": "#515c6d"
      }
    ]
  },
  {
    "featureType": "water",
    "elementType": "labels.text.stroke",
    "stylers": [
      {
        "lightness": -20
      }
    ]
  }
]

```

To generate styles json file click this [link](https://mapstyle.withgoogle.com/)
[<img src="https://i.stack.imgur.com/r8B7G.png" alt="enter image description here" />](https://i.stack.imgur.com/r8B7G.png)



## Default Google Map Activity


This Activity code will provide basic functionality for including a Google Map using a SupportMapFragment.

The Google Maps V2 API includes an all-new way to load maps.

Activities now have to implement the **OnMapReadyCallBack** interface, which comes with a **onMapReady()** method override that is executed everytime we run **SupportMapFragment**.**getMapAsync(OnMapReadyCallback)**; and the call is successfully completed.

Maps use [**Markers**](https://developers.google.com/android/reference/com/google/android/gms/maps/model/Marker) , [**Polygons**](https://developers.google.com/android/reference/com/google/android/gms/maps/model/Polygon) and [**PolyLines**](https://developers.google.com/android/reference/com/google/android/gms/maps/model/Polyline) to show interactive information to the user.

**MapsActivity.java:**

```java
public class MapsActivity extends AppCompatActivity implements OnMapReadyCallback {

    private GoogleMap mMap;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_maps);
        SupportMapFragment mapFragment = (SupportMapFragment) getSupportFragmentManager()
                .findFragmentById(R.id.map);
        mapFragment.getMapAsync(this);
    }

    @Override
    public void onMapReady(GoogleMap googleMap) {
        mMap = googleMap;

        // Add a marker in Sydney, Australia, and move the camera.
        LatLng sydney = new LatLng(-34, 151);
        mMap.addMarker(new MarkerOptions().position(sydney).title("Marker in Sydney"));
        mMap.moveCamera(CameraUpdateFactory.newLatLng(sydney));
    }
}

```

Notice that the code above inflates a layout, which has a SupportMapFragment nested inside the container Layout, defined with an ID of `R.id.map`.  The layout file is shown below:

**activity_maps.xml**

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:orientation="vertical" android:layout_width="match_parent"
    android:layout_height="match_parent">

    <fragment xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        xmlns:map="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:id="@+id/map"
        tools:context="com.example.app.MapsActivity"
        android:name="com.google.android.gms.maps.SupportMapFragment"/>

</LinearLayout>

```



## Show Current Location in a Google Map


Here is a full Activity class that places a Marker at the current location, and also moves the camera to the current position.

There are a few thing going on in sequence here:

- Check Location permission
- Once Location permission is granted, call `setMyLocationEnabled()`, build the GoogleApiClient, and connect it
- Once the GoogleApiClient is connected, request location updates

```java
public class MapLocationActivity extends AppCompatActivity
        implements OnMapReadyCallback,
        GoogleApiClient.ConnectionCallbacks,
        GoogleApiClient.OnConnectionFailedListener,
        LocationListener {

    GoogleMap mGoogleMap;
    SupportMapFragment mapFrag;
    LocationRequest mLocationRequest;
    GoogleApiClient mGoogleApiClient;
    Location mLastLocation;
    Marker mCurrLocationMarker;

    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        getSupportActionBar().setTitle("Map Location Activity");

        mapFrag = (SupportMapFragment) getSupportFragmentManager().findFragmentById(R.id.map);
        mapFrag.getMapAsync(this);
    }

    @Override
    public void onPause() {
        super.onPause();

        //stop location updates when Activity is no longer active
        if (mGoogleApiClient != null) {
            LocationServices.FusedLocationApi.removeLocationUpdates(mGoogleApiClient, this);
        }
    }

    @Override
    public void onMapReady(GoogleMap googleMap)
    {
        mGoogleMap=googleMap;
        mGoogleMap.setMapType(GoogleMap.MAP_TYPE_HYBRID);

        //Initialize Google Play Services
        if (android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
            if (ContextCompat.checkSelfPermission(this,
                    Manifest.permission.ACCESS_FINE_LOCATION)
                    == PackageManager.PERMISSION_GRANTED) {
                //Location Permission already granted
                buildGoogleApiClient();
                mGoogleMap.setMyLocationEnabled(true);
            } else {
                //Request Location Permission
                checkLocationPermission();
            }
        }
        else {
            buildGoogleApiClient();
            mGoogleMap.setMyLocationEnabled(true);
        }
    }

    protected synchronized void buildGoogleApiClient() {
        mGoogleApiClient = new GoogleApiClient.Builder(this)
                .addConnectionCallbacks(this)
                .addOnConnectionFailedListener(this)
                .addApi(LocationServices.API)
                .build();
        mGoogleApiClient.connect();
    }

    @Override
    public void onConnected(Bundle bundle) {
        mLocationRequest = new LocationRequest();
        mLocationRequest.setInterval(1000);
        mLocationRequest.setFastestInterval(1000);
        mLocationRequest.setPriority(LocationRequest.PRIORITY_BALANCED_POWER_ACCURACY);
        if (ContextCompat.checkSelfPermission(this,
                Manifest.permission.ACCESS_FINE_LOCATION)
                == PackageManager.PERMISSION_GRANTED) {
            LocationServices.FusedLocationApi.requestLocationUpdates(mGoogleApiClient, mLocationRequest, this);
        }
    }

    @Override
    public void onConnectionSuspended(int i) {}

    @Override
    public void onConnectionFailed(ConnectionResult connectionResult) {}

    @Override
    public void onLocationChanged(Location location)
    {
        mLastLocation = location;
        if (mCurrLocationMarker != null) {
            mCurrLocationMarker.remove();
        }

        //Place current location marker
        LatLng latLng = new LatLng(location.getLatitude(), location.getLongitude());
        MarkerOptions markerOptions = new MarkerOptions();
        markerOptions.position(latLng);
        markerOptions.title("Current Position");
        markerOptions.icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_MAGENTA));
        mCurrLocationMarker = mGoogleMap.addMarker(markerOptions);

        //move map camera
        mGoogleMap.moveCamera(CameraUpdateFactory.newLatLng(latLng));
        mGoogleMap.animateCamera(CameraUpdateFactory.zoomTo(11));

        //stop location updates
        if (mGoogleApiClient != null) {
            LocationServices.FusedLocationApi.removeLocationUpdates(mGoogleApiClient, this);
        }
    }

    public static final int MY_PERMISSIONS_REQUEST_LOCATION = 99;
    private void checkLocationPermission() {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION)
                != PackageManager.PERMISSION_GRANTED) {

            // Should we show an explanation?
            if (ActivityCompat.shouldShowRequestPermissionRationale(this,
                    Manifest.permission.ACCESS_FINE_LOCATION)) {

                // Show an explanation to the user *asynchronously* -- don't block
                // this thread waiting for the user's response! After the user
                // sees the explanation, try again to request the permission.
                new AlertDialog.Builder(this)
                        .setTitle("Location Permission Needed")
                        .setMessage("This app needs the Location permission, please accept to use location functionality")
                        .setPositiveButton("OK", new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialogInterface, int i) {
                                //Prompt the user once explanation has been shown
                                ActivityCompat.requestPermissions(MapLocationActivity.this,
                                        new String[]{Manifest.permission.ACCESS_FINE_LOCATION},
                                        MY_PERMISSIONS_REQUEST_LOCATION );
                            }
                        })
                        .create()
                        .show();


            } else {
                // No explanation needed, we can request the permission.
                ActivityCompat.requestPermissions(this,
                        new String[]{Manifest.permission.ACCESS_FINE_LOCATION},
                        MY_PERMISSIONS_REQUEST_LOCATION );
            }
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode,
                                           String permissions[], int[] grantResults) {
        switch (requestCode) {
            case MY_PERMISSIONS_REQUEST_LOCATION: {
                // If request is cancelled, the result arrays are empty.
                if (grantResults.length > 0
                        && grantResults[0] == PackageManager.PERMISSION_GRANTED) {

                    // permission was granted, yay! Do the
                    // location-related task you need to do.
                    if (ContextCompat.checkSelfPermission(this,
                            Manifest.permission.ACCESS_FINE_LOCATION)
                            == PackageManager.PERMISSION_GRANTED) {

                        if (mGoogleApiClient == null) {
                            buildGoogleApiClient();
                        }
                        mGoogleMap.setMyLocationEnabled(true);
                    }

                } else {

                    // permission denied, boo! Disable the
                    // functionality that depends on this permission.
                    Toast.makeText(this, "permission denied", Toast.LENGTH_LONG).show();
                }
                return;
            }

            // other 'case' lines to check for other
            // permissions this app might request
        }
    }

}

```

activity_main.xml:

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:orientation="vertical" android:layout_width="match_parent"
    android:layout_height="match_parent">

    <fragment xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        xmlns:map="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:id="@+id/map"
        tools:context="com.example.app.MapLocationActivity"
        android:name="com.google.android.gms.maps.SupportMapFragment"/>

</LinearLayout>

```

**Result:**

Show explanation if needed on Marshmallow and Nougat using an AlertDialog (this case happens when the user had previously denied a permission request, or had granted the permission and then later revoked it in the settings):

[<img src="https://i.stack.imgur.com/CWSkM.png" alt="enter image description here" />](https://i.stack.imgur.com/CWSkM.png)

Prompt the user for Location permission on Marshmallow and Nougat by calling `ActivityCompat.requestPermissions()`:

[<img src="https://i.stack.imgur.com/o1cmu.png" alt="enter image description here" />](https://i.stack.imgur.com/o1cmu.png)

Move camera to current location and place Marker when the Location permission is granted:

[<img src="https://i.stack.imgur.com/qpxoy.png" alt="enter image description here" />](https://i.stack.imgur.com/qpxoy.png)



## Adding markers to a map


To add markers to a Google Map, for example from an `ArrayList` of `MyLocation` Objects, we can do it this way.

The `MyLocation` holder class:

```java
public class MyLocation {
  LatLng latLng;
  String title;
  String snippet;
}

```

Here is a method that would take a list of `MyLocation` Objects and place a Marker for each one:

```java
private void LocationsLoaded(List<MyLocation> locations){
 
 for (MyLocation myLoc : locations){
    mMap.addMarker(new MarkerOptions()
     .position(myLoc.latLng)
     .title(myLoc.title)
     .snippet(myLoc.snippet)
     .icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_MAGENTA));
 }
}

```

Note:
For the purpose of this example, `mMap` is a class member variable of the Activity, where we've assigned it to the map reference received in the `onMapReady()` override.



## MapView: embedding a GoogleMap in an existing layout


It is possible to treat a GoogleMap as an Android view if we make use of the provided MapView class. Its usage is very similar to MapFragment.

In your layout use MapView as follows:

```java
<com.google.android.gms.maps.MapView 
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:map="http://schemas.android.com/apk/res-auto"
    android:id="@+id/map"
    android:layout_width="match_parent"
    android:layout_height="match_parent" 
    <!-- 
    map:mapType="0" Specifies a change to the initial map type
    map:zOrderOnTop="true"  Control whether the map view's surface is placed on top of its window
    map:useVieLifecycle="true" When using a MapFragment, this flag specifies whether the lifecycle of the map should be tied to the fragment's view or the fragment itself
    map:uiCompass="true" Enables or disables the compass
    map:uiRotateGestures="true" Sets the preference for whether rotate gestures should be enabled or disabled
    map:uiScrollGestures="true" Sets the preference for whether scroll gestures should be enabled or disabled
    map:uiTiltGestures="true" Sets the preference for whether tilt gestures should be enabled or disabled
    map:uiZoomGestures="true" Sets the preference for whether zoom gestures should be enabled or disabled
    map:uiZoomControls="true" Enables or disables the zoom controls
    map:liteMode="true" Specifies whether the map should be created in lite mode
    map:uiMapToolbar="true" Specifies whether the mapToolbar should be enabled
    map:ambientEnabled="true" Specifies whether ambient-mode styling should be enabled
    map:cameraMinZoomPreference="0.0" Specifies a preferred lower bound for camera zoom
    map:cameraMaxZoomPreference="1.0" Specifies a preferred upper bound for camera zoom -->
    />

```

Your activity needs to implement the OnMapReadyCallback interface in order to work:

```java
/**
* This shows how to create a simple activity with a raw MapView and add a marker to it. This
* requires forwarding all the important lifecycle methods onto MapView.
*/
public class RawMapViewDemoActivity extends AppCompatActivity implements OnMapReadyCallback {

    private MapView mMapView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.raw_mapview_demo);

        mMapView = (MapView) findViewById(R.id.map);
        mMapView.onCreate(savedInstanceState);

        mMapView.getMapAsync(this);
    }

    @Override
    protected void onResume() {
        super.onResume();
        mMapView.onResume();
    }

    @Override
    public void onMapReady(GoogleMap map) {
        map.addMarker(new MarkerOptions().position(new LatLng(0, 0)).title("Marker"));
    }

    @Override
    protected void onPause() {
        mMapView.onPause();
        super.onPause();
    }

    @Override
    protected void onDestroy() {
        mMapView.onDestroy();
        super.onDestroy();
    }

    @Override
    public void onLowMemory() {
        super.onLowMemory();
        mMapView.onLowMemory();
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        mMapView.onSaveInstanceState(outState);
    }
}

```



## Obtaining the SH1-Fingerprint of your certificate keystore file


In order to obtain a Google Maps API key for your certificate, you must provide the API console with the SH1-fingerprint of your debug/release keystore.

You can obtain the keystore by using the **JDK's keytool** program as described [here](https://developers.google.com/maps/documentation/android-api/signup) in the docs.

Another approach is to obtain the fingerprint programmatically by running this snippet with your app signed with the debug/release certificate and printing the hash to the log.

```java
PackageInfo info;
try {
    info = getPackageManager().getPackageInfo("com.package.name", PackageManager.GET_SIGNATURES);
    for (Signature signature : info.signatures) {
        MessageDigest md;
        md = MessageDigest.getInstance("SHA");
        md.update(signature.toByteArray());
        String hash= new String(Base64.encode(md.digest(), 0));
        Log.e("hash", hash);
    }
} catch (NameNotFoundException e1) {
    Log.e("name not found", e1.toString());
} catch (NoSuchAlgorithmException e) {
    Log.e("no such an algorithm", e.toString());
} catch (Exception e) {
    Log.e("exception", e.toString());
}

```



## Do not launch Google Maps  when the map is clicked (lite mode)


When a Google Map is displayed in lite mode clicking on a map will open the Google Maps application.  To disable this functionality you must call `setClickable(false)` on the `MapView`, **e.g.**:

```java
final MapView mapView = (MapView)view.findViewById(R.id.map);
mapView.setClickable(false);

```



## UISettings


Using [`UISettings`](https://developers.google.com/android/reference/com/google/android/gms/maps/UiSettings), the appearance of the Google Map can be modified.

Here is an example of some common settings:

```

   mGoogleMap.setMapType(GoogleMap.MAP_TYPE_HYBRID);
    mGoogleMap.getUiSettings().setMapToolbarEnabled(true);
    mGoogleMap.getUiSettings().setZoomControlsEnabled(true);
    mGoogleMap.getUiSettings().setCompassEnabled(true);

```

Result:

[<img src="https://i.stack.imgur.com/XY5GA.png" alt="enter image description here" />](https://i.stack.imgur.com/XY5GA.png)



## Get debug SHA1 fingerprint


1. Open Android Studio
1. Open Your Project
1. Click on Gradle (From Right Side Panel, you will see **Gradle Bar**)
1. Click on Refresh (Click on Refresh from **Gradle Bar**, you will see **List** Gradle scripts of your Project)
1. Click on Your Project (Your Project Name form **List** (root))
1. Click on Tasks
1. Click on android
1. Double Click on signingReport (You will get **SHA1** and **MD5** in **Run Bar**)

**[<img src="https://i.stack.imgur.com/WgxB3.png" alt="Reference Screenshot" />](https://i.stack.imgur.com/WgxB3.png)**



## InfoWindow Click Listener


Here is an example of how to define a different action for each Marker's InfoWindow click event.

Use a HashMap in which the marker ID is the key, and the value is the corresponding action it should take when the InfoWindow is clicked.

Then, use a `OnInfoWindowClickListener` to handle the event of a user clicking the InfoWindow, and use the HashMap to determine which action to take.

In this simple example we will open up a different Activity based on which Marker's InfoWindow was clicked.

Declare the HashMap as an instance variable of the Activity or Fragment:

```

//Declare HashMap to store mapping of marker to Activity
HashMap<String, String> markerMap = new HashMap<String, String>();

```

Then, each time you add a Marker, make an entry in the HashMap with the Marker ID and the action it should take when it's InfoWindow is clicked.

For example, adding two Markers and defining an action to take for each:

```java
Marker markerOne = googleMap.addMarker(new MarkerOptions().position(latLng1)
        .title("Marker One")
        .snippet("This is Marker One");
String idOne = markerOne.getId();
markerMap.put(idOne, "action_one");

Marker markerTwo = googleMap.addMarker(new MarkerOptions().position(latLng2)
        .title("Marker Two")
        .snippet("This is Marker Two");
String idTwo = markerTwo.getId();
markerMap.put(idTwo, "action_two");

```

In the InfoWindow click listener, get the action from the HashMap, and open up the corresponding Activity based on the action of the Marker:

```

mGoogleMap.setOnInfoWindowClickListener(new GoogleMap.OnInfoWindowClickListener() {
  @Override
  public void onInfoWindowClick(Marker marker) {

    String actionId = markerMap.get(marker.getId());

    if (actionId.equals("action_one")) {
      Intent i = new Intent(MainActivity.this, ActivityOne.class);
      startActivity(i);
    } else if (actionId.equals("action_two")) {
      Intent i = new Intent(MainActivity.this, ActivityTwo.class);
      startActivity(i);
    }
  }
});

```

Note If the code is in a Fragment, replace MainActivity.this with getActivity().



## Change Offset


By changing mappoint **x** and **y** values as you need you can change offset possition of google map,by default it will be in the center of the map view. Call below method where you want to change it! Better to use it inside your `onLocationChanged` like  `changeOffsetCenter(location.getLatitude(),location.getLongitude());`

```

public void changeOffsetCenter(double latitude,double longitude) {
            Point mappoint = mGoogleMap.getProjection().toScreenLocation(new LatLng(latitude, longitude));
            mappoint.set(mappoint.x, mappoint.y-100); // change these values as you need , just hard coded a value if you want you can give it based on a ratio like using DisplayMetrics  as well
            mGoogleMap.animateCamera(CameraUpdateFactory.newLatLng(mGoogleMap.getProjection().fromScreenLocation(mappoint)));
        }

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|GoogleMap|the `GoogleMap` is an object that is received on a `onMapReady()` event
|MarkerOptions|`MarkerOptions` is the builder class of a `Marker`, and is used to add one marker to a map.



#### Remarks


**Requirements**

1. Google Play Services SDK installed.
1. A Google Console Account.
1. A Google Maps API Key obtained in Google Console.

