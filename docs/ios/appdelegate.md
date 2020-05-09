---
metaTitle: "iOS - AppDelegate"
description: "All States of Application through AppDelegate methods, AppDelegate Roles:, Opening a URL-Specified Resource, Handling Local and Remote Notifications"
---

# AppDelegate


**AppDelegate** is a protocol which defines methods that are called by the singleton UIApplication object in response to important events in the lifetime of an app.

Normally used to perform tasks on application startup (setup app environment, analitycs (ex.: Mixpanel/GoogleAnalytics/Crashlitics), DB stack etc.) and shutdown (ex.: save DB context), handling URL open requests and similar application-wide tasks.



## All States of Application through AppDelegate methods


For Getting updated or to do something before app goes live to user you can use below method.

**AppDidFinishLaunching**

```swift
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // Write your code before app launch
    return YES;
}

```

**While App Enter in Foreground:**

```swift
- (void)applicationWillEnterForeground:(UIApplication *)application {
    // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
}

```

**When App Launching and also background to Foreground hit below method:**

```swift
- (void)applicationDidBecomeActive:(UIApplication *)application {
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}

```

**While App Enter in background:**

```swift
- (void)applicationDidEnterBackground:(UIApplication *)application {
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
}

```

**While app resign active**

```swift
- (void)applicationWillResignActive:(UIApplication *)application {
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
}

```

**While app terminate:**

```swift
- (void)applicationWillTerminate:(UIApplication *)application {
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

```



## AppDelegate Roles:


- AppDelegate contains your app’s `startup code`.
- It responds to `key changes` in the `state` of your app. Specifically, it responds to both temporary interruptions and to changes in the execution state of your app, such as when your app transitions from the foreground to the background.
- It `responds to notifications` originating from outside the app, such as remote notifications (also known as push notifications), low-memory warnings, download completion notifications, and more.
- It `determines` whether `state preservation` and `restoration` should occur and assists in the preservation and restoration process as needed.
<li>It `responds to events` that target the app itself and are not specific to your app’s views or view controllers.
You can use it to store your app’s central data objects or any content that does not have an owning view controller.</li>



## Opening a URL-Specified Resource


Asks the delegate to open a resource specified by a URL, and provides a dictionary of launch options.

**Example of usage:**

```swift
func application(_ app: UIApplication, open url: URL, options: [UIApplicationOpenURLOptionsKey : Any] = [:]) -> Bool {
        return SomeManager.shared.handle(
            url,
            sourceApplication: options[.sourceApplication] as? String,
            annotation: options[.annotation]
        )
    }

```



## Handling Local and Remote Notifications


**Example of usage:**

```swift
/* Instance of your custom APNs/local notification manager */  
private var pushManager: AppleNotificationManager!

```

Registration:

```swift
func application(application: UIApplication, didRegisterUserNotificationSettings notificationSettings: UIUserNotificationSettings) {
    // Called to tell the delegate the types of notifications that can be used to get the user’s attention
    pushManager.didRegisterSettings(notificationSettings)
}

func application(application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: NSData) {
    // Tells the delegate that the app successfully registered with Apple Push Notification service (APNs)
    pushManager.didRegisterDeviceToken(deviceToken)
}

func application(application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: NSError) {
    // Sent to the delegate when Apple Push Notification service cannot successfully complete the registration process.
    pushManager.didFailToRegisterDeviceToken(error)
}

```

Remote notifications handling:

```swift
func application(application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject]) {
    // Remote notification arrived, there is data to be fetched
    // Handling it
    pushManager.handleNotification(userInfo,
                                   background: application.applicationState == .Background
    )
}

```

Local notifications handling:

```swift
func application(application: UIApplication, didReceiveLocalNotification notification: UILocalNotification) {
    pushManager.handleLocalNotification(notification, background: false)
}

```

Handling action (deprecated):

```swift
func application(application: UIApplication, handleActionWithIdentifier identifier: String?, forRemoteNotification userInfo: [NSObject : AnyObject],
                     completionHandler: () -> Void) {
    pushManager.handleInteractiveRemoteNotification(userInfo, actionIdentifier: identifier, completion: completionHandler)
}

```

