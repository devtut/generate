---
metaTitle: "iOS - FCM Messaging in Swift"
description: "Initialize FCM in Swift"
---

# FCM Messaging in Swift



## Initialize FCM in Swift


follow below step to add FCM in your swift Project

1- If you don't have an Xcode project yet, create one now.
Create a Podfile if you don't have one:

> 
<p>$ cd your-project directory<br />
$ pod init</p>


2- Add the pods that you want to install. You can include a Pod in your Podfile like this:

> 
<p>pod 'Firebase/Core'<br />
pod 'Firebase/Messaging'</p>


3- Install the pods and open the .xcworkspace file to see the project in Xcode.

> 
<p>$ pod install<br />
$ open your-project.xcworkspace</p>


4- Download a GoogleService-Info.plist file from [plist](https://firebase.google.com/console/) and include it in your app.

5- Upload APNs certificate to Firebase. [APN Cert](https://firebase.google.com/docs/cloud-messaging/ios/certs)

6- add "import Firebase" in your appDelegate file of project

7- add this "FIRApp.configure()" in your "application:didFinishLaunchingWithOptions"

8- register for remote notification

```

 if #available(iOS 10.0, *) {
  let authOptions : UNAuthorizationOptions = [.Alert, .Badge, .Sound]
  UNUserNotificationCenter.currentNotificationCenter().requestAuthorizationWithOptions(
    authOptions,
    completionHandler: {_,_ in })

  // For iOS 10 display notification (sent via APNS)
  UNUserNotificationCenter.currentNotificationCenter().delegate = self
  // For iOS 10 data message (sent via FCM)
  FIRMessaging.messaging().remoteMessageDelegate = self

} else {
  let settings: UIUserNotificationSettings =
  UIUserNotificationSettings(forTypes: [.Alert, .Badge, .Sound], categories: nil)
  application.registerUserNotificationSettings(settings)
}

application.registerForRemoteNotifications()

```

9- to get register token use

```swift
let token = FIRInstanceID.instanceID().token()!

```

10- and if u want monitor for token change use below code in appDelegate file

```swift
func tokenRefreshNotification(notification: NSNotification) {
if let refreshedToken = FIRInstanceID.instanceID().token() {
    print("InstanceID token: \(refreshedToken)")
  }

  // Connect to FCM since connection may have failed when attempted before having a token.
  connectToFcm()
}

```

11- to recieve message from fcm add below code in appDelegate

```swift
func connectToFcm() {
  FIRMessaging.messaging().connectWithCompletion { (error) in
    if (error != nil) {
      print("Unable to connect with FCM. \(error)")
    } else {
      print("Connected to FCM.")
    }
  }
}

```

12- and for disconnect use

```swift
func applicationDidEnterBackground(application: UIApplication) {
  FIRMessaging.messaging().disconnect()
  print("Disconnected from FCM.")
}

```

in your appDelegate.

the initialization complete and client ready to recieve message from fcm panel or send by token from third party server



#### Remarks


FCM : [https://firebase.google.com/docs/cloud-messaging/ios/client](https://firebase.google.com/docs/cloud-messaging/ios/client)

