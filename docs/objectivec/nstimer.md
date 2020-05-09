---
metaTitle: "Objective-C - NSTimer"
description: "Creating a Timer, Invalidating a timer, Manually firing a timer, Storing information in the Timer"
---

# NSTimer




## Creating a Timer


This will create a timer to call the `doSomething` method on `self` in `5.0` seconds.

```objc
[NSTimer scheduledTimerWithTimeInterval:5.0
         target:self 
         selector:@selector(doSomething) 
         userInfo:nil 
         repeats:NO];

```

Setting the `repeats` parameter to `false/NO` indicates that we want the timer to fire only once. If we set this to `true/YES`, it would fire every five seconds until manually invalidated.



## Invalidating a timer


```objc
[timer invalidate];
timer = nil;

```

This will stop the timer from firing. **Must be called from the thread the timer was created in,** see [Apple's notes](https://developer.apple.com/library/ios/documentation/Cocoa/Reference/Foundation/Classes/NSTimer_Class/#//apple_ref/occ/instm/NSTimer/invalidate):

> 
You must send this message from the thread on which the timer was installed. If you send this message from another thread, the input source associated with the timer may not be removed from its run loop, which could prevent the thread from exiting properly.


Setting `nil` will help you next to check whether it's running or not.

```objc
if(timer) {
    [timer invalidate];
    timer = nil;
}

//Now set a timer again.

```



## Manually firing a timer


```objc
[timer fire];

```

Calling the `fire` method causes an NSTimer to perform the task it would have usually performed on a schedule.

In a **non-repeating timer**, this will automatically invalidate the timer. That is, calling `fire` before the time interval is up will result in only one invocation.

In a **repeating timer**, this will simply invoke the action without interrupting the usual schedule.



## Storing information in the Timer


When creating a timer, you can set the `userInfo` parameter to include information that you want to pass to the function you call with the timer.

By taking a timer as a parameter in said function, you can access the `userInfo` property.

```objc
NSDictionary *dictionary = @{
                             @"Message" : @"Hello, world!"
                            }; //this dictionary contains a message
[NSTimer scheduledTimerWithTimeInterval:5.0
     target:self 
     selector:@selector(doSomething) 
     userInfo:dictionary
     repeats:NO]; //the timer contains the dictionary and later calls the function

...

- (void) doSomething:(NSTimer*)timer{
    //the function retrieves the message from the timer
    NSLog("%@", timer.userInfo["Message"]);
}

```

