---
metaTitle: "iOS - Checking for Network Connectivity"
description: "Creating a Reachability listener, Add observer to network changes, Alert when network becomes unavailable, Alert when connection becomes a WIFI or cellular network, Verify if is connected to network"
---

# Checking for Network Connectivity



## Creating a Reachability listener


Apple's [Reachability](https://developer.apple.com/library/ios/samplecode/Reachability/Introduction/Intro.html) class periodically checks the network status and alerts observers to changes.

```swift
Reachability *internetReachability = [Reachability reachabilityForInternetConnection];
[internetReachability startNotifier];

```



## Add observer to network changes


`Reachability` uses `NSNotification` messages to alert observers when the network state has changed. Your class will need to become an observer.

```swift
[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(reachabilityChanged:) name:kReachabilityChangedNotification object:nil];

```

Elsewhere in your class, implement method signature

```swift
- (void) reachabilityChanged:(NSNotification *)note {
    //code which reacts to network changes
}

```



## Alert when network becomes unavailable


```swift
- (void)reachabilityChanged:(NSNotification *)note {
    Reachability* reachability = [note object];
    NetworkStatus netStatus = [reachability currentReachabilityStatus];

    if (netStatus == NotReachable) {
        NSLog(@"Network unavailable");
    }
}

```



## Alert when connection becomes a WIFI or cellular network


```swift
- (void)reachabilityChanged:(NSNotification *)note {
    Reachability* reachability = [note object];
    NetworkStatus netStatus = [reachability currentReachabilityStatus];

    switch (netStatus) {
        case NotReachable:
            NSLog(@"Network unavailable");
            break;
        case ReachableViaWWAN:
            NSLog(@"Network is cellular");
            break;
        case ReachableViaWiFi:
            NSLog(@"Network is WIFI");
            break;
    }
}

```



## Verify if is connected to network


**Swift**

```swift
import SystemConfiguration

/// Class helps to code reuse in handling internet network connections.
class NetworkHelper {

    /**
     Verify if the device is connected to internet network.
     - returns:          true if is connected to any internet network, false if is not
     connected to any internet network.
     */
   class func isConnectedToNetwork() -> Bool {
       var zeroAddress = sockaddr_in()
    
       zeroAddress.sin_len = UInt8(sizeofValue(zeroAddress))
       zeroAddress.sin_family = sa_family_t(AF_INET)
    
       let defaultRouteReachability = withUnsafePointer(&zeroAddress) {
           SCNetworkReachabilityCreateWithAddress(nil, UnsafePointer($0))
       }
    
       var flags = SCNetworkReachabilityFlags()
    
       if !SCNetworkReachabilityGetFlags(defaultRouteReachability!, &flags) {
           return false
       }
    
       let isReachable = (flags.rawValue & UInt32(kSCNetworkFlagsReachable)) != 0
       let needsConnection = (flags.rawValue & UInt32(kSCNetworkFlagsConnectionRequired)) != 0
    
      return (isReachable && !needsConnection)
   }
}



if NetworkHelper.isConnectedToNetwork() {
    // Is connected to network
}

```

**Objective-C:**

we can check network connectivity within few lines of code as:

```swift
-(BOOL)isConntectedToNetwork
{
    Reachability *networkReachability = [Reachability reachabilityForInternetConnection];
    NetworkStatus networkStatus = [networkReachability currentReachabilityStatus];
    if (networkStatus == NotReachable)
    {
        NSLog(@"There IS NO internet connection");
        return false;
    } else
    {
        NSLog(@"There IS internet connection");
        return true;
    }
}

```



#### Remarks


The source code for `Reachability.h` and `Reachability.m` can be found on Apple's developer documentation [site](https://developer.apple.com/library/ios/samplecode/Reachability/Introduction/Intro.html).

### Caveats

Unlike other platforms, Apple is yet to provide a standard set of APIs to determine an iOS device's network status and offer only these code examples linked above. The source file change over time, but once imported into an app project, they are seldom updated by the developers.

For this reason most app developers tend to use one of the many Github/[Cocoapod](https://cocoapods.org/?q=reachability) maintained libraries for reachability.

Apple also recommends, for requests made at the user’s behest, that you [always attempt a connection **first**, before using Reachability/SCNetworkReachability to diagnose the failure or to wait for the connection to return](https://developer.apple.com/library/content/documentation/NetworkingInternetWeb/Conceptual/NetworkingOverview/WhyNetworkingIsHard/WhyNetworkingIsHard.html#//apple_ref/doc/uid/TP40010220-CH13-SW3).

