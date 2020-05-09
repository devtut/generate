---
metaTitle: "iOS - Background Modes and Events"
description: "Play Audio in Background"
---

# Background Modes and Events



## Play Audio in Background


Add a key named **Required background modes** in property list (.plist) file ..

as following picture..

<img src="http://i.stack.imgur.com/m453q.png" alt="enter image description here" />

And add following code in

**AppDelegate.h**

```swift
#import <AVFoundation/AVFoundation.h>
#import <AudioToolbox/AudioToolbox.h>

```

**AppDelegate.m**

in application didFinishLaunchingWithOptions

```swift
[[AVAudioSession sharedInstance] setDelegate:self];
[[AVAudioSession sharedInstance] setCategory:AVAudioSessionCategoryPlayback error:nil];
[[AVAudioSession sharedInstance] setActive:YES error:nil];
[[UIApplication sharedApplication] beginReceivingRemoteControlEvents];

UInt32 size = sizeof(CFStringRef);
CFStringRef route;
AudioSessionGetProperty(kAudioSessionProperty_AudioRoute, &size, &route);
NSLog(@"route = %@", route);

```

If you want changes as per events you have to add following code in AppDelegate.m

```swift
- (void)remoteControlReceivedWithEvent:(UIEvent *)theEvent {
    
    if (theEvent.type == UIEventTypeRemoteControl)    {
        switch(theEvent.subtype)        {
            case UIEventSubtypeRemoteControlPlay:
                [[NSNotificationCenter defaultCenter] postNotificationName:@"TogglePlayPause" object:nil];
                break;
            case UIEventSubtypeRemoteControlPause:
                [[NSNotificationCenter defaultCenter] postNotificationName:@"TogglePlayPause" object:nil];
                break;
            case UIEventSubtypeRemoteControlStop:
                break;
            case UIEventSubtypeRemoteControlTogglePlayPause:
                [[NSNotificationCenter defaultCenter] postNotificationName:@"TogglePlayPause" object:nil];
                break;
            default:
                return;
        }
    }
}

```

Based on notification have to work on it..

