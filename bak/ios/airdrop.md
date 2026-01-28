---
metaTitle: "iOS - AirDrop"
description: "AirDrop"
---

# AirDrop



## AirDrop


**Objective-C**

Airdrop can be used from `UIActivityViewController`. The `UIActivityViewController` class is a standard view controller that provides several standard services, such as copying items to the clipboard, sharing content to social media sites, sending items via Messages, AirDrop and some third party applications.

In this case we would be sending an image via `UIActivityViewController`

```swift
UIImage *hatImage = [UIImage imageNamed:@"logo.png"];
if (hatImage)//checks if the image file is not nil
{
//Initialise a UIActivityViewController
UIActivityViewController *controller = [[UIActivityViewController alloc] initWithActivityItems:@[hatImage] applicationActivities:nil];
//Excludes following options from the UIActivityViewController menu
NSArray *excludeActivities = @[UIActivityTypePostToWeibo,UIActivityTypePrint, UIActivityTypeMail,UIActivityTypeMessage,UIActivityTypePostToTwitter,UIActivityTypePostToFacebook,
                                                              UIActivityTypeCopyToPasteboard,UIActivityTypeAssignToContact,
                                                              UIActivityTypeSaveToCameraRoll,UIActivityTypeAddToReadingList,
                                                              UIActivityTypePostToFlickr,UIActivityTypePostToVimeo,
                                                              UIActivityTypePostToTencentWeibo];
controller.excludedActivityTypes = excludeActivities;
[self presentViewController:controller animated:YES completion:nil];
}

```

**Swift**

```swift
if ((newImage) != nil)
        {
            let activityVC = UIActivityViewController(activityItems: [newImage], applicationActivities: nil)
            activityVC.excludedActivityTypes =[UIActivityTypeAddToReadingList]
            self.presentViewController(activityVC, animated: true, completion: nil)
            
        }

```

