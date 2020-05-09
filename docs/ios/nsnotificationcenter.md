---
metaTitle: "iOS - NSNotificationCenter"
description: "Removing Observers, Adding an Observer, Posting a Notification with Data, Add and remove observer for name, Posting a Notification, Observing a Notification, Adding/Removing an Observer with a Block"
---

# NSNotificationCenter


iOS notifications are a simple and powerful way to send data in a loosely coupled way. That is, the sender of a notification doesn't have to care about who (if anyone) receives the notification, it just posts it out there to the rest of the app and it could be picked up by lots of things or nothing depending on your app's state.

**Source** : - [HACKING with Swift](https://www.hackingwithswift.com/example-code/system/how-to-post-messages-using-notificationcenter)



## Removing Observers


### Swift 2.3

```swift
//Remove observer for single notification
NSNotificationCenter.defaultCenter().removeObserver(self, name: "TestNotification", object: nil)
    
//Remove observer for all notifications
NotificationCenter.defaultCenter().removeObserver(self)

```

### Swift 3

```swift
//Remove observer for single notification
NotificationCenter.default.removeObserver(self, name: NSNotification.Name(rawValue: "TestNotification"), object: nil)

//Remove observer for all notifications
NotificationCenter.default.removeObserver(self)

```

### Objective-C

```swift
//Remove observer for single notification
[[NSNotificationCenter defaultCenter] removeObserver:self name:@"TestNotification" object:nil];
    
//Remove observer for all notifications
[[NSNotificationCenter defaultCenter] removeObserver:self];

```



## Adding an Observer


### Naming Convention

Notifications are identified by global NSString objects whose names are composed in this way:

`Name of associated class` + `Did | Will` + `UniquePartOfName` + `Notification`

For example:

- NSApplicationDidBecomeActiveNotification
- NSWindowDidMiniaturizeNotification
- NSTextViewDidChangeSelectionNotification
- NSColorPanelColorDidChangeNotification

### Swift 2.3

```swift
NSNotificationCenter.defaultCenter().addObserver(self, 
                                                 selector: #selector(self.testNotification(_:)), 
                                                 name: "TestNotification", 
                                                 object: nil)

```

### Swift 3

```swift
NSNotificationCenter.default.addObserver(self, 
                                         selector: #selector(self.testNotification(_:)), 
                                         name: NSNotification.Name(rawValue: "TestNotification"), 
                                         object: nil)

```

### Objective-C

```swift
[[NSNotificationCenter defaultCenter] addObserver:self 
                                      selector:@selector(testNotification:) 
                                      name:@"TestNotification" 
                                      object:nil];

```

PS: It is also worth noting that the number of times an observer has been added has to be exactly the number of times the observer is removed. A rookie mistake is to add the observer in the `viewWillAppear:` of a UIViewController, but removing the observer in `viewDidUnload:`, will cause an uneven number of pushes and thus leaking the observer and the notification selector getting called in a superfluous manner.



## Posting a Notification with Data


### Swift

```swift
let userInfo: [String: AnyObject] = ["someKey": myObject]
NSNotificationCenter.defaultCenter().postNotificationName("TestNotification", object: self, userInfo: userInfo)

```

### Objective-C

```swift
NSDictionary *userInfo = [NSDictionary dictionaryWithObject:myObject forKey:@"someKey"];
[[NSNotificationCenter defaultCenter] postNotificationName: @"TestNotification" object:nil userInfo:userInfo];

```



## Add and remove observer for name


```swift
// Add observer
let observer = NSNotificationCenter.defaultCenter().addObserverForName("nameOfTheNotification", object: nil, queue: nil) { (notification) in
    // Do operations with the notification in this block
}

// Remove observer
NSNotificationCenter.defaultCenter().removeObserver(observer)

```



## Posting a Notification


### Swift

```swift
NSNotificationCenter.defaultCenter().postNotificationName("TestNotification", object: self)

```

### Objective-C

```swift
[[NSNotificationCenter defaultCenter] postNotificationName:@"TestNotification" object:nil];

```



## Observing a Notification


### Swift

```swift
func testNotification(notification: NSNotification) {
    let userInfo = notification.userInfo
    let myObject: MyObject = userInfo["someKey"]
}

```

### Objective-C

```swift
- (void)testNotification:(NSNotification *)notification {
    NSDictionary *userInfo = notification.userInfo;
    MyObject *myObject = [userInfo objectForKey:@"someKey"];
}

```



## Adding/Removing an Observer with a Block


Instead of adding an observer with a selector, a block can be used:

```swift
id testObserver = [[NSNotificationCenter defaultCenter] addObserverForName:@"TestNotification"
                                                                    object:nil 
                                                                     queue:nil 
                                                                usingBlock:^(NSNotification* notification) {
    NSDictionary *userInfo = notification.userInfo;
    MyObject *myObject = [userInfo objectForKey:@"someKey"];
}];

```

The observer can then be removed with:

```swift
[[NSNotificationCenter defaultCenter] removeObserver:testObserver 
                                                name:@"TestNotification"
                                              object:nil];

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|name|The name of the notification for which to register the observer; that is, only notifications with this name are used to add the block to the operation queue. If you pass nil, the notification center doesn’t use a notification’s name to decide whether to add the block to the operation queue.
|obj|The object whose notifications the observer wants to receive; that is, only notifications sent by this sender are delivered to the observer. If you pass nil, the notification center doesn’t use a notification’s sender to decide whether to deliver it to the observer.
|queue|The operation queue to which block should be added. If you pass nil, the block is run synchronously on the posting thread.
|block|The block to be executed when the notification is received. The block is copied by the notification center and (the copy) held until the observer registration is removed.



#### Remarks


An NSNotificationCenter object (or simply, notification center) provides a mechanism for broadcasting information within a program. An NSNotificationCenter object is essentially a notification dispatch table.

For more info, check out the Apple Documentation [here](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSNotificationCenter_Class/)

[NSNotification & NSNotificationCenter in Swift](https://iosdevcenters.blogspot.com/2016/03/nsnotification-nsnotificationcenter-in.html)

