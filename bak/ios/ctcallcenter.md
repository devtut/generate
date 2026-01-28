---
metaTitle: "iOS - CTCallCenter"
description: "Intercepting calls from your app even from the background, CallKit - ios 10"
---

# CTCallCenter



## Intercepting calls from your app even from the background


From Apple documentation:

> 
Use the CTCallCenter class to obtain a list of current cellular calls, and to respond to state changes for calls such as from a dialing state to a connected state. Such state changes are known as cellular call events.


The purpose of CTCallCenter is to give the developer the opportunity to pause his app state during a call in order to give the user the best experience.

**Objective-C:**

First, we will define a new class member inside the class we want to handle the interceptions:

```swift
@property (atomic, strong) CTCallCenter *callCenter;

```

Inside our class init (constructor) we will allocate new memory for our class member:

```swift
[self setCallCenter:[CTCallCenter new]];

```

Afterwards, we will invoke our new method that actually handles the interceptions:

```swift
- (void)registerPhoneCallListener
{
[[self callCenter] setCallEventHandler:^(CTCall * _Nonnull call) {
    NSLog(@"CallEventHandler called - interception in progress");

     if ([call.callState isEqualToString: CTCallStateConnected])
     {
         NSLog(@"Connected");
     }
     else if ([call.callState isEqualToString: CTCallStateDialing])
     {
         NSLog(@"Dialing");
     }
     else if ([call.callState isEqualToString: CTCallStateDisconnected])
     {
         NSLog(@"Disconnected");

     } else if ([call.callState isEqualToString: CTCallStateIncoming])
     {
         NSLog(@"Incomming");
     }
 }];
}

```

That's it, if the user will use your app and will receive a phone call you could intercept this call and handle your app for a save state.

It is worth mentioning that there are 4 call states you can intercept:

```swift
CTCallStateDialing
CTCallStateIncoming
CTCallStateConnected
CTCallStateDisconnected

```

**Swift:**

Define your class member at the relevant class and define it:

```

   self.callCenter = CTCallCenter()
    self.callCenter.callEventHandler = { call in
        //  Handle your interception
        if call.callState == CTCallStateConnected
        {
        }
    }

```

What will happen if your app is in the background and you need to intercept calls while the app is in the background ?

For example, if you develop an **enterprise** app you can basically just add 2 capabilities (VOIP & Background fetch) in the Capabilities tab:

Your project target -> Capabilities -> Background Modes -> mark Voice over IP & Background fetch



## CallKit - ios 10


```swift
//Header File

<CallKit/CXCallObserver.h>

CXCallObserver *callObserver = [[CXCallObserver alloc] init];

// If queue is nil, then callbacks will be performed on main queue

[callObserver setDelegate:self queue:nil];

// Don't forget to store reference to callObserver, to prevent it from being released

self.callObserver = callObserver;

// get call status
- (void)callObserver:(CXCallObserver *)callObserver callChanged:(CXCall *)call {
    if (call.hasConnected) {
        // perform necessary actions
    }
}

```

