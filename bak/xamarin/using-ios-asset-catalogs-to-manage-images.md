---
metaTitle: "Xamarin - Using iOS Asset Catalogs to Manage Images"
description: "Loading an asset catalog image, Managing Images in an asset catalog, Adding Asset Catalog images in storyboard"
---

# Using iOS Asset Catalogs to Manage Images



## Loading an asset catalog image


Load an image from an asset catalog using `UIImage.FromBundle(string imageName)`

```cs
UIImage image = UIImage.FromBundle("ImageName");
// use the name of the image set from the asset catalog

```

You can use the image for a `UIImageView` or anything else you need to do.



## Managing Images in an asset catalog


[<img src="http://i.stack.imgur.com/qrpRA.png" alt="Adding an image set" />](http://i.stack.imgur.com/qrpRA.png)

Asset catalogs allow managing images, app icons, and launch images. Image Set is used for images which are displayed in the app. Universal images are usually the best option. You can either use a vector based image (such as PDF) which will scale for all screens, or include a 1x, 2x, and 3x variant and iOS will select the appropriate version of the image for the user's current device.

[<img src="http://i.stack.imgur.com/Qqoew.png" alt="Manage asset catalog images" />](http://i.stack.imgur.com/Qqoew.png)

You can change the name of any set in the asset catalog by double-clicking on name. Images can be added by either drag and drop or click on the image you want to fill-in for a file picker.



## Adding Asset Catalog images in storyboard


Asset catalog images can be used from storyboards like any other kind of image added to the project. They will be automatically populated as an option in `UIImageView` and other views which support adding an image.



#### Remarks


Asset catalogs are way to manage multiple resolutions of iOS image assets. In order to display optimal images, iOS uses 1x, 2x, and 3x versions of each image according to the device's screen density. The 1x version is only for very old, non-retina devices so it isn't necessary for apps only supporting iOS 9.

Asset catalogs will help support app thinning and slicing, optimizing the resources users have to download to install an app from the App Store.

