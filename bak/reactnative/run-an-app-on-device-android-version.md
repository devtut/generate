---
metaTitle: "React Native - Run an app on device (Android Version)"
description: "Running an app on Android Device."
---

# Run an app on device (Android Version)



## Running an app on Android Device.


&lt;li>`adb devices`
&lt;ul>
1. Is your phone displaying? If not, enable developer mode on your phone, and connect it by USB.
&lt;/ul>
&lt;/li>
&lt;li>`adb reverse tcp:8081 tcp:8081` :
&lt;ul>
1. In order to link correctly your phone and that React-Native recognize him during build. (**NOTE:`Android Version 5` or above.**)
&lt;/ul>
&lt;/li>
&lt;li>`react-native run-android` :
&lt;ul>
1. To run the app on your phone.
&lt;/ul>
&lt;/li>
&lt;li>`react-native start` :
&lt;ul>
1. In order to start a local server for development (mandatory). This server is automatically started if you use the last version of React-native.
&lt;/ul>
&lt;/li>

- In order to link correctly your phone and that React-Native recognize him during build. (**NOTE:`Android Version 5` or above.**)

- In order to start a local server for development (mandatory). This server is automatically started if you use the last version of React-native.



#### Remarks


Troubleshootings : <br>
`Could not connect to development server` => Do this : `adb reverse tcp:8081 tcp:8081`, make sure that your phone is connected (adb devices). Verify also that there is a local server launched, if not run `react-native start`

