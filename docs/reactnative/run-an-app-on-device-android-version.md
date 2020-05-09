---
metaTitle: "React Native - Run an app on device (Android Version)"
description: "Running an app on Android Device."
---

# Run an app on device (Android Version)



## Running an app on Android Device.


<li>`adb devices`
<ul>
1. Is your phone displaying? If not, enable developer mode on your phone, and connect it by USB.
</ul>
</li>
<li>`adb reverse tcp:8081 tcp:8081` :
<ul>
1. In order to link correctly your phone and that React-Native recognize him during build. (**NOTE:`Android Version 5` or above.**)
</ul>
</li>
<li>`react-native run-android` :
<ul>
1. To run the app on your phone.
</ul>
</li>
<li>`react-native start` :
<ul>
1. In order to start a local server for development (mandatory). This server is automatically started if you use the last version of React-native.
</ul>
</li>

- In order to link correctly your phone and that React-Native recognize him during build. (**NOTE:`Android Version 5` or above.**)

- In order to start a local server for development (mandatory). This server is automatically started if you use the last version of React-native.



#### Remarks


Troubleshootings : <br>
`Could not connect to development server` => Do this : `adb reverse tcp:8081 tcp:8081`, make sure that your phone is connected (adb devices). Verify also that there is a local server launched, if not run `react-native start`

