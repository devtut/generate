---
metaTitle: "iOS - Checking iOS version"
description: "iOS 8 and later, Swift 2.0 and later, Compare versions, Device iOS Version"
---

# Checking iOS version



## iOS 8 and later


**Swift 3:**

```swift
let minimumVersion = OperatingSystemVersion(majorVersion: 8, minorVersion: 1, patchVersion: 2)
if ProcessInfo().isOperatingSystemAtLeast(minimumVersion) {
    //current version is >= (8.1.2)
} else {
    //current version is < (8.1.2)
}

```



## Swift 2.0 and later


```swift
if #available(iOS 9, *) {
  // iOS 9
} else {
  // iOS 8 or earlier
}

```



## Compare versions


```swift
let minimumVersionString = "3.1.3"
let versionComparison = UIDevice.current.systemVersion.compare(minimumVersionString, options: .numeric)
switch versionComparison {
    case .orderedSame, .orderedDescending:
        //current version is >= (3.1.3)
        break
    case .orderedAscending:
        //current version is < (3.1.3)
        fallthrough
    default:
        break;
}

```

### Objective-C

```swift
NSString *version = @"3.1.3"; 
NSString *currentVersion = @"3.1.1";
NSComparisonResult result = [currentVersion compare:version options:NSNumericSearch];
switch(result){
  case: NSOrderedAscending:
        //less than the current version
  break;
  case: NSOrderedDescending:
  case: NSOrderedSame:
       // equal or greater than the current version
  break;
}

```



## Device iOS Version


This will give current system version.

### Objective-C

```swift
NSString *version = [[UIDevice currentDevice] systemVersion]

```

### Swift

```swift
let version = UIDevice.currentDevice().systemVersion

```

### Swift 3

```swift
let version = UIDevice.current.systemVersion

```

