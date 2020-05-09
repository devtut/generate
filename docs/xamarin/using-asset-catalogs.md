---
metaTitle: "Xamarin - Using Asset Catalogs"
description: "Adding image assets to asset catalog"
---

# Using Asset Catalogs



## Adding image assets to asset catalog


This is how the Asset Catalog in Xamarin Studio looks like,

[<img src="http://i.stack.imgur.com/CD8Hv.png" alt="Asset Catalog options" />](http://i.stack.imgur.com/CD8Hv.png)

As shown in above picture there are 5 types of assets you can create within the catalog.

I will cover only image set, because its the simplest one.

When you create a new image set. You will get options like this
[<img src="http://i.stack.imgur.com/gMke8.png" alt="New Image Set Options" />](http://i.stack.imgur.com/gMke8.png)

To add images to the catalog you can simply click on the dashed squares and select the image you want to set for particular option.

In XCode you have options of 1x, 2x and 3x to cover the most recent iOS device screen sizes. But Xamarin has one extra option Vector using which you can upload PDF formatted Vector image which would be automatically scaled depending on the device your application is running on.

For iPhone images Xamarin retains the iOS7 special image size R4 which is used for 4-Inch screen sized iPhone (5, 5S and SE).

Please refer [Xamarin Documentation on how to add images to iOS application](https://developer.xamarin.com/guides/ios/application_fundamentals/working_with_images/displaying-an-image/#asset-catalogs) for more information.

