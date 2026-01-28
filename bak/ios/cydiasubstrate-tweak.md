---
metaTitle: "iOS - CydiaSubstrate tweak"
description: "Create new tweak using Theos"
---

# CydiaSubstrate tweak


Learn how to create cydia substrate tweaks for jailbroken iPhones.

Those tweaks will enable you to modify the operating system's behavior to act the way you would like it to.



## Create new tweak using Theos


### Use nic to create a new project

Enter this command in your terminal

```swift
$THEOS/bin/nic.pl

```

```swift
NIC 2.0 - New Instance Creator
------------------------------
  [1.] iphone/activator_event
  [2.] iphone/application_modern
  [3.] iphone/cydget
  [4.] iphone/flipswitch_switch
  [5.] iphone/framework
  [6.] iphone/ios7_notification_center_widget
  [7.] iphone/library
  [8.] iphone/notification_center_widget
  [9.] iphone/preference_bundle_modern
  [10.] iphone/tool
  [11.] iphone/tweak
  [12.] iphone/xpc_service
Choose a Template (required):

```

Choose template `[11.] iphone/tweak`

Fill in the details and you will get the following files created:

```swift
-rw-r--r--@  1 gkpln3  staff   214B Jun 12 15:09 Makefile
-rw-r--r--@  1 gkpln3  staff    89B Jun 11 22:58 TorchonFocus.plist
-rw-r--r--   1 gkpln3  staff   2.7K Jun 12 16:10 Tweak.xm
-rw-r--r--   1 gkpln3  staff   224B Jun 11 16:17 control
drwxr-xr-x   3 gkpln3  staff   102B Jun 11 16:18 obj
drwxr-xr-x  16 gkpln3  staff   544B Jun 12 16:12 packages

```

### Override iOS screenshots saving method

open the `Tweak.xm` file using your favorite code editor.

hook  to a certain method from the operating system.

```swift
%hook SBScreenShotter
- (void)saveScreenshot:(BOOL)screenshot
{
    %orig;
    NSLog(@"saveScreenshot: is called");
}
%end

```

Note you can choose wether or not the original function should be called, for example:

```swift
%hook SBScreenShotter
- (void)saveScreenshot:(BOOL)screenshot
{
    NSLog(@"saveScreenshot: is called");
}
%end

```

will override the function without calling the original one, thus casing screenshots not being saved.



#### Remarks


### Installing Theos

[https://github.com/theos/theos/wiki/Installation](https://github.com/theos/theos/wiki/Installation)

