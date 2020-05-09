---
metaTitle: "iOS - UILocalNotification"
description: "Scheduling a local notification, Managing local notifications using UUID, Presenting a local notification immediately, Registering for local notifications, what's new in UILocalNotification with iOS10, Responding to received local notification, Register and Schedule Local Notification in Swift 3.0 (iOS 10), Notification Sound"
---

# UILocalNotification


Local notifications allow your app to notify the user about content which does not require the use of a server.

Unlike remote notifications which are triggered from a server, local notifications are scheduled and triggered within an app. Notifications in general are targeted to increase user interaction with the app, inviting or tempting the user to open and interact with it.

UILocalNotification was deprecated in iOS 10. Use the UserNotifications framework instead.



## Scheduling a local notification


Make sure you see [Registering for local notifications](http://stackoverflow.com/documentation/ios/635/uilocalnotification/2075/registering-for-local-notifications#t=201608111702069639816) in order for this to work:

**Swift**

```swift
let notification = UILocalNotification()
notification.alertBody = "Hello, local notifications!"
notification.fireDate = NSDate().dateByAddingTimeInterval(10) // 10 seconds after now
UIApplication.sharedApplication().scheduleLocalNotification(notification)

```

**Objective-C**

```swift
UILocalNotification *notification = [[UILocalNotification alloc] init];
notification.alertBody = @"Hello, local notifications!";
notification.fireDate = [NSDate dateWithTimeIntervalSinceNow:10]; // 10 seconds after now
[[UIApplication sharedApplication] scheduleLocalNotification:notification];

```

To see the notification in the iOS Simulator, type `^⌘H` (control-command-H) to go home and then type `⌘L` (command-L) to lock the device. Wait a few seconds, and the notification should appear (this appearance will vary depending on notification type discussed in "Registering for local notifications"):

[<img src="http://i.stack.imgur.com/XEwzD.png" alt="Local Notification Banner" />](http://i.stack.imgur.com/XEwzD.png)

Swipe on the notification to get back to the app (note that if you called this in the first view controller's `viewDidLoad`, `viewWillAppear`, `viewDidAppear`, etc., the notification will be scheduled again).



## Managing local notifications using UUID


Often times you will need to be able to manage your notifications, by being able to keep track of them and cancel them.

### Track a notification

You can assign a UUID (universally unique identifier) to a notification, so you can track it:

**Swift**

```swift
let notification = UILocalNotification()
let uuid = NSUUID().uuidString
notification.userInfo = ["UUID": uuid]
UIApplication.shared.scheduleLocalNotification(notification)

```

**Objective-C**

```swift
UILocalNotification *notification = [[UILocalNotification alloc] init];
NSString *uuid = [[NSUUID UUID] UUIDString];
notification.userInfo = @{ @"UUID": uuid };
[[UIApplication sharedApplication] scheduleLocalNotification:notification];

```

### Cancel a notification

To cancel a notification, we first get a list of all the notifications and then find the one with a matching UUID. Finally, we cancel it.

**Swift**

```swift
let scheduledNotifications = UIApplication.shared.scheduledLocalNotifications

guard let scheduledNotifications = scheduledNotifications else {
    return
}

for notification in scheduledNotifications where "\(notification.userInfo!["UUID"]!)" == UUID_TO_CANCEL {
    UIApplication.sharedApplication().cancelLocalNotification(notification)
}

```

**Objective-C**

```swift
NSArray *scheduledNotifications = [[UIApplication sharedApplication] scheduledLocalNotifications];

for (UILocalNotification *notification in scheduledNotifications) {
    if ([[notification.userInfo objectForKey:"UUID"] compare: UUID_TO_CANCEL]) {
        [[UIApplication sharedApplication] cancelLocalNotification:notification];
        break;
    }
}

```

You would probably want to store all of these UUID's in Core Data or Realm.



## Presenting a local notification immediately


If you want to show local notification immediately, you should call:

**Swift 3**

```swift
UIApplication.shared.presentLocalNotificationNow(notification)

```

**Swift 2**

```swift
UIApplication.sharedApplication().presentLocalNotificationNow(notification)

```

**Objective-C**

```swift
[[UIApplication sharedApplication] presentLocalNotificationNow:notification];

```

An advantage of using this is so you won't have to set the `fireDate` and `timeZone` properties of your `UILocalNotification` object.



## Registering for local notifications


In order to present local notifications to the user, you have to register your app with the device:

**Swift**

```swift
let settings = UIUserNotificationSettings(forTypes: [.Badge, .Sound, .Alert], categories: nil)
UIApplication.sharedApplication().registerUserNotificationSettings(settings)

```

**Objective-C**

```swift
UIUserNotificationSettings *settings = [UIUserNotificationSettings settingsForTypes:(UIUserNotificationTypeBadge | UIUserNotificationTypeSound | UIUserNotificationTypeAlert) categories:nil];
[[UIApplication sharedApplication] registerUserNotificationSettings:settings];

```

This will present an alert the first time it is called:

[<img src="http://i.stack.imgur.com/XIv5n.png" alt="Register for local notifications alert" />](http://i.stack.imgur.com/XIv5n.png)

Regardless of what the user chooses, the alert will not show up again and changes will have to be initiated by the user in Settings.



## what's new in UILocalNotification with iOS10


You can use `UILocalNotification`, old APIs also works fine with iOS10, but we had better use the APIs in the User Notifications framework instead. There are also some new features, you can only use with iOS10 User Notifications framework.

This also happens to Remote Notification, for more information: [Here](https://stackoverflow.com/a/40253296/3395008).

New Features:

1. Now you can either present alert, sound or increase badge while the app is in foreground too with iOS 10
1. Now you can handle all event in one place when user tapped (or slided) the action button, even while the app has already been killed.
1. Support 3D touch instead of sliding gesture.
1. Now you can remove specifical local notification just by one row code.
1. Support Rich Notification with custom UI.

It is really easy for us to convert `UILocalNotification` APIs to iOS10
User Notifications framework APIs, they are really similar.

I write a Demo here to show how to use new and old APIs at the same time:  [**iOS10AdaptationTips**](https://github.com/ChenYilong/iOS10AdaptationTips) .

For example,

With Swift implementation:

1. import UserNotifications

```

   ///    Notification become independent from UIKit
    import UserNotifications

```


<li>
request authorization for localNotification

```swift
    let center = UNUserNotificationCenter.current()
    center.requestAuthorization(options: [.alert, .sound]) { (granted, error) in
        // Enable or disable features based on authorization.
    }

```


</li>

<li>
schedule localNotification
</li>
<li>
update application icon badge number

```swift
@IBAction  func triggerNotification(){
    let content = UNMutableNotificationContent()
    content.title = NSString.localizedUserNotificationString(forKey: "Elon said:", arguments: nil)
    content.body = NSString.localizedUserNotificationString(forKey: "Hello Tom！Get up, let's play with Jerry!", arguments: nil)
    content.sound = UNNotificationSound.default()
    content.badge = UIApplication.shared().applicationIconBadgeNumber + 1;
    content.categoryIdentifier = "com.elonchan.localNotification"
    // Deliver the notification in five seconds.
    let trigger = UNTimeIntervalNotificationTrigger.init(timeInterval: 60.0, repeats: true)
    let request = UNNotificationRequest.init(identifier: "FiveSecond", content: content, trigger: trigger)

    // Schedule the notification.
    let center = UNUserNotificationCenter.current()
    center.add(request)
}

@IBAction func stopNotification(_ sender: AnyObject) {
    let center = UNUserNotificationCenter.current()
    center.removeAllPendingNotificationRequests()
    // or you can remove specifical notification:
    // center.removePendingNotificationRequests(withIdentifiers: ["FiveSecond"])
}

```


</li>

Objective-C implementation:

1. import UserNotifications

```

   // Notifications are independent from UIKit
    #import <UserNotifications/UserNotifications.h>

```


<li>
request authorization for localNotification

```swift
UNUserNotificationCenter *center = [UNUserNotificationCenter currentNotificationCenter];
[center requestAuthorizationWithOptions:(UNAuthorizationOptionBadge | UNAuthorizationOptionSound | UNAuthorizationOptionAlert)
                      completionHandler:^(BOOL granted, NSError * _Nullable error) {
                          if (!error) {
                              NSLog(@"request authorization succeeded!");
                              [self showAlert];
                          }
                      }];

```


</li>

<li>
schedule localNotification
</li>
<li>
update application icon badge number

```swift
UNMutableNotificationContent *content = [[UNMutableNotificationContent alloc] init];
content.title = [NSString localizedUserNotificationStringForKey:@"Elon said:"
                                                    arguments:nil];
content.body = [NSString localizedUserNotificationStringForKey:@"Hello Tom！Get up, let's play with Jerry!"
                                                   arguments:nil];
content.sound = [UNNotificationSound defaultSound];

// 4. update application icon badge number
content.badge = [NSNumber numberWithInteger:([UIApplication sharedApplication].applicationIconBadgeNumber + 1)];
// Deliver the notification in five seconds.
UNTimeIntervalNotificationTrigger *trigger = [UNTimeIntervalNotificationTrigger
                                            triggerWithTimeInterval:5.f
                                            repeats:NO];
UNNotificationRequest *request = [UNNotificationRequest requestWithIdentifier:@"FiveSecond"
                                                                    content:content
                                                                    trigger:trigger];
/// 3. schedule localNotification
UNUserNotificationCenter *center = [UNUserNotificationCenter currentNotificationCenter];
[center addNotificationRequest:request withCompletionHandler:^(NSError * _Nullable error) {
    if (!error) {
        NSLog(@"add NotificationRequest succeeded!");
    }
}];

```


</li>

Go to here for more information: [**iOS10AdaptationTips**](https://github.com/ChenYilong/iOS10AdaptationTips).

#updated

> 
Terminating app due to uncaught exception 'NSInternalInconsistencyException', reason: 'time interval must be at least 60 if repeating'


```swift
let trigger = UNTimeIntervalNotificationTrigger.init(timeInterval: 60, repeats: true)

```



## Responding to received local notification


**IMPORTANT: This delegate method is only called in the foreground.**

**Swift**

```swift
func application(application: UIApplication, didReceiveLocalNotification notification: UILocalNotification) {
    
}

```

**Objective-C**

```swift
- (void)application:(UIApplication *)application didReceiveLocalNotification:(UILocalNotification *)notification {
    
}

```

This method is generally overridden in the AppDelegate, which conforms to the UIApplicationDelegate protocol.



## Register and Schedule Local Notification in Swift 3.0 (iOS 10)


**Registration**

in **AppDelegate**

```swift
import UserNotifications

```

in **didFinishLaunchingWithOptions** method,

```swift
UNUserNotificationCenter.current().requestAuthorization(options: [.alert,.sound,.badge]) { (granted, error) in

// Here you can check Request is Granted or not.

}

```

Create and Schedule notification.

```

   let content = UNMutableNotificationContent()
    content.title = "10 Second Notification Demo"
    content.subtitle = "From Wolverine"
    content.body = "Notification after 10 seconds - Your pizza is Ready!!"
    content.categoryIdentifier = "myNotificationCategory"

    let trigger = UNTimeIntervalNotificationTrigger(
        timeInterval: 10.0,
        repeats: false)
    
    let request = UNNotificationRequest(
        identifier: "10.second.message",
        content: content,
        trigger: trigger
    )
    UNUserNotificationCenter.current().add(request, withCompletionHandler: nil)

```

Where ever this part of code is triggered, If you have allowed Notification Permission, you will receive an notification.

To test it properly, Make sure your application in Background mode.



## Notification Sound


Custom sounds may be provided for notifications generated by your app. When the system displays an alert for a local notification or badges an app icon, it plays this sound (so long as the user has not disabled notification sounds).

The default value is nil which means no sound is played for your notification.

To supply a custom sound, add a `.caf`, `.wav`, or `.aiff` file to your app bundle. Sounds that last longer than 30 seconds are not supported. Supplying a sound that does not meet those requirements will cause the default sound to play (`UILocalNotificationDefaultSoundName`).

**Objective-C**

```swift
UILocalNotification *notification = [UILocalNotification new];
notification.soundName = @"nameOfSoundInBundle.wav"; // Use UILocalNotificationDefaultSoundName for the default alert sound

```

**Swift**

```swift
let notification = UILocalNotification()
notification.soundName = "nameOfSoundInBundle.wav"

```



#### Remarks


Do not confuse UILocalNotification with push notifications. UILocalNotification is triggered by your device, and when scheduled, is copied to the system.

Links:

- [UILocalNotification Class Reference](https://developer.apple.com/library/ios/documentation/iPhone/Reference/UILocalNotification_Class/)
- [UILocalNotification on Stack Overflow](http://stackoverflow.com/questions/tagged/uilocalnotification)

