---
metaTitle: "iOS - UIActivityViewController"
description: "Initializing the Activity View Controller"
---

# UIActivityViewController




## Initializing the Activity View Controller


### **Objective-C**

```swift
NSString *textToShare = @"StackOverflow Documentation!! Together, we can do for Documentation what we did for Q&A.";
NSURL *documentationURL = [NSURL URLWithString:@"http://stackoverflow.com/tour/documentation"];

NSArray *objectsToShare = @[textToShare, documentationURL];

UIActivityViewController *activityVC = [[UIActivityViewController alloc] initWithActivityItems:objectsToShare applicationActivities:nil];

[self presentViewController:activityVC animated:YES completion:nil];

```

### **Swift**

```swift
let textToShare = "StackOverflow Documentation!! Together, we can do for Documentation what we did for Q&A."
let documentationURL = NSURL(string:"http://stackoverflow.com/tour/documentation")

let objToShare : [AnyObject] = [textToShare, documentationURL!]

let activityVC = UIActivityViewController(activityItems: objToShare, applicationActivities: nil)
self.presentViewController(activityVC, animated: true, completion: nil)

```



#### Parameters


|Parameter Name|Description
|---|---|---|---|---|---|---|---|---|---
|activityItems|Contains array of object to perform the activity. This array must not be nil and must contain at least one object.
|applicationActivities|An array of UIActivity objects representing the custom services that your application supports. This parameter can be nil.

