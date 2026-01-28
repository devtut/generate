---
metaTitle: "iOS - NSUserActivity"
description: "Creating a NSUserActivity"
---

# NSUserActivity


An NSUserActivity object can be used to coordinate significant events in an app with the system. It is the basis for [Handoff](https://developer.apple.com/handoff/) between different devices running iOS and macOS. Additionally, it may also be used to improve public-indexing and augment or create Spotlight Search results for an app. As of iOS 10, it may also be used to coordinate interactions between your app and Siri using SiriKit.



## Creating a NSUserActivity


To create a `NSUserActivity` object, your app must declare the types of activities it supports in its `Info.plist` file. Supported activities are defined by your application and should be unique. An activity is defined using a reverse-domain style naming scheme (i.e. "com.companyName.productName.activityName"). Here is what an entry in your Info.plist might look like:

|Key|Value
|---|---|---|---|---|---|---|---|---|---
|NSUserActivityTypes|[Array]
|- item0|com.companyName.productName.activityName01
|- item1|com.companyName.productName.activityName02

Once you have defined all supported activity types, you may begin to access and use them in your application's code.

To create a `NSUserActivity` object you must do the following

```swift
// Initialize the activity object and set its type from one of the ones specified in your app's plist
NSUserActivity *currentActivity = [[NSUserActivity alloc] initWithActivityType:@"com.companyName.productName.activityName01"];

// Set the title of the activity.
// This title may be displayed to the user, so make sure it is localized and human-readable
currentActivity.title = @"Current Activity";

// Configure additional properties like userInfo which will be included in the activity
currentActivity.userInfo = @{@"informationKey" : @"value"};

// Configure the activity so the system knows what may be done with it
// It is important that you only set YES to tasks that your application supports
// In this example, we will only enable the activity for use with Handoff
[currentActivity setEligibleForHandoff:YES];
[currentActivity setEligibleForSearch:NO]; // Defaults to NO
[currentActivity setEligibleForPublicIndexing:NO]; // Defaults to NO

// Set this activity as the current user activity
// Only one activity may be current at a time on a device. Calling this method invalidates any other current activities.
[currentActivity becomeCurrent];

```

After this, the activity above should be available for Handoff (although more work is required to properly handle the "Handoff").



#### Remarks


### Activity Types

Supported activity types must be defined in your app's `Info.plist` file under the `NSUserActivityTypes` key. Activities are tied to your Developer Team ID, meaning that activity coordination is restricted between apps that have the same Team ID (e.g. "Safari" could not accept a Handoff activity from "Chrome" or vice versa).

### Becoming / Resigning the Current Activity

Marking an activity as current using `becomeCurrent` makes it available for Handoff or Spotlight Indexing. Only one activity may be current at a time. You may mark an activity as inactive without invalidating by calling `resignCurrent`.

If you `invalidate` an activity, the same instance may not be made current again.

Do not mark an activity as current when providing it for [SiriKit](https://developer.apple.com/sirikit/).

### Search Indexing

Activities are **not** to be used as a general-purpose indexing mechanism within your app. Instead, they should only be used in response to user-initiated actions. To index all content in your app, use CoreSpotlight.

