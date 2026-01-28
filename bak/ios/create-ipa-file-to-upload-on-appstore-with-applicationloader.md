---
metaTitle: "iOS - Create .ipa File to upload on appstore with Applicationloader"
description: "create .ipa file to upload app to appstore with Application Loader"
---

# Create .ipa File to upload on appstore with Applicationloader



## create .ipa file to upload app to appstore with Application Loader


If you want to upload .ipa file to itunesconnect **without integrating developer account in Xcode** and you want to use **application loader**. then you can **generate .ipa with iTunes** .

**Step 1 :-** Select device inplace of simulator.

[<img src="http://i.stack.imgur.com/Xq6iC.png" alt="enter image description here" />](http://i.stack.imgur.com/Xq6iC.png)

**Step 2 :-** Go to Product -> select Archive

[<img src="http://i.stack.imgur.com/9h28M.png" alt="enter image description here" />](http://i.stack.imgur.com/9h28M.png)

**Step 3 :-** After complited process right click to your Archive -> and select show in Finder

[<img src="http://i.stack.imgur.com/9jofK.png" alt="enter image description here" />](http://i.stack.imgur.com/9jofK.png)

**Step 4 :-** when you click on show in finder you will redirect to Archive folder, looks like this

[<img src="http://i.stack.imgur.com/RlIU8.png" alt="enter image description here" />](http://i.stack.imgur.com/RlIU8.png)

**Step 5 :-** Right click on .xarchive file -> select Show in finder option.

[<img src="http://i.stack.imgur.com/hhtc1.png" alt="enter image description here" />](http://i.stack.imgur.com/hhtc1.png)

**Step 6 :-** Go to Product Folder -> Application Folder -> You will find yourprojectname.app

[<img src="http://i.stack.imgur.com/KHoiu.png" alt="enter image description here" />](http://i.stack.imgur.com/KHoiu.png)

**Step 7 :-** Now to convert .app to .ipa just drag and drop into itunes . check below image ,

[<img src="http://i.stack.imgur.com/udTTp.gif" alt="enter image description here" />](http://i.stack.imgur.com/udTTp.gif)

**Step 8 :-** Now put this .ipa file in safe place and use when upload with application loader .

**Note :-** if you want to know how to upload app with application loader then check this ,

[Upload app with application Loader](http://help.apple.com/itc/apploader/)

**EDIT :-**

**WARNING :-**  Don't make .ipa with  changing extension from .aap to .zip and .zip to .ipa.

I have seen in many answer that , they have suggest compress .app file and then change the extension from .zip to .ipa . It is not working now . By this method you will get Error like ,

> 
IPA is invalid, it does not include a payload directory.


