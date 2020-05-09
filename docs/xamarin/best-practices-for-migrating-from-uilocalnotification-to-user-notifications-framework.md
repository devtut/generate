---
metaTitle: "Xamarin - Best practices for migrating from UILocalNotification to User Notifications framework"
description: "UserNotifications"
---

# Best practices for migrating from UILocalNotification to User Notifications framework



## UserNotifications


<li>
You will have to import UserNotifications

```cs
 @import UserNotifications;

```


</li>

2.Request authorization for localNotification

```cs
let center = UNUserNotificationCenter.current()
center.requestAuthorization([.alert, .sound]) { (granted, error) in
    // Enable or disable features based on authorization.
}

```


<li>
Now we will update the application icon badge number

```cs
 @IBAction  func triggerNotification(){
 let content = UNMutableNotificationContent()
 content.title = NSString.localizedUserNotificationString(forKey: "Tom said:", arguments: nil)
 content.body = NSString.localizedUserNotificationString(forKey: "Hello Mike！Let's go.", arguments: nil)
 content.sound = UNNotificationSound.default()
 content.badge = UIApplication.shared().applicationIconBadgeNumber + 1;
 content.categoryIdentifier = "com.mike.localNotification"
 //Deliver the notification in two seconds.
 let trigger = UNTimeIntervalNotificationTrigger.init(timeInterval: 1.0, repeats: true)
 let request = UNNotificationRequest.init(identifier: "TwoSecond", content: content, trigger: trigger)

 //Schedule the notification.
 let center = UNUserNotificationCenter.current()
 center.add(request)
 }

 @IBAction func stopNotification(_ sender: AnyObject) {
 let center = UNUserNotificationCenter.current()
 center.removeAllPendingNotificationRequests()
 }

```


</li>

