---
metaTitle: "iOS - Custom fonts"
description: "Embedding custom fonts, Custom Fonts with Storyboard, Applying custom fonts  to controls within a Storyboard"
---

# Custom fonts




## Embedding custom fonts


> 
<p>**Custom Font Support**<br>
Applications that want to use custom fonts can now include those fonts in their application bundle and register those fonts with the system by including the UIAppFonts key in their Info.plist file. The value of this key is an array of strings identifying the font files in the application’s bundle. When the system sees the key, it loads the specified fonts and makes them available to the application.</p>


Once the fonts have been set in the `Info.plist`, you can use your custom fonts as any other font in IB or programatically.

1. Drag and drop your font to Xcode Supporting Files folder. Don't forget to mark your app at "Add to targets" section. From this moment you can use this font in IB and choose it from font pallet.

[<img src="https://i.stack.imgur.com/d9Vun.png" alt="enter image description here" />](https://i.stack.imgur.com/d9Vun.png)

<li>To make this font available on the device, open `Info.plist` and add `Fonts provided by application key` (UIAppFonts). Add font name as the value to the Item 0 key. Note: Font name can vary from your font file name.
[<img src="https://i.stack.imgur.com/nN47U.png" alt="enter image description here" />](https://i.stack.imgur.com/nN47U.png)</li>

1. Get the custom added font name using below snippet

[**Swift 3**]

```swift
for family in UIFont.familyNames {
            print("\(family)")

            for name in UIFont.fontNames(forFamilyName: family) {
                print("   \(name)")
            }
        }

```

[**Objective - C**]

```swift
for (NSString *familyName in [UIFont familyNames]){
        NSLog(@"Family name: %@", familyName);
        for (NSString *fontName in [UIFont fontNamesForFamilyName:familyName]) {
            NSLog(@"--Font name: %@", fontName);
        }
    }

```



## Custom Fonts with Storyboard


Custom Fonts for UI components from storyboard can be easily achieved with [User Defined Runtime Attributes](https://developer.apple.com/library/ios/recipes/xcode_help-interface_builder/Chapters/AddUserDefinedRuntimeAttributes.html) in storyboard and [Categories](https://developer.apple.com/library/ios/documentation/General/Conceptual/DevPedia-CocoaCore/Category.html).

The advantages are like,

- No need to define outlets for the ui element
- No need to set font for elements programatically.

> 
**Steps to follow**
<ol>
<li>
<p>**Font File:** Add the Font file (.ttf) to the application bundle and add the entry for the font in Info.plist under <strong><em>Font provided by
application</em></strong> as in this [documentation](http://stackoverflow.com/documentation/ios/drafts/41034) of custom fonts.</p>
</li>
<li>
<p>**Define Categories:** Add a file like ****UIKit+IBExtensions**** and add the categories for UI elements like UILabel, UIButton etc. for
which you want to set custom font. All the categories will be having a
custom property say **fontName**. This will be using from the
storyboard later for setting custom font (as in step 4).</p>
</li>
</ol>


### UIKit+IBExtensions.h

```swift
#import <UIKit/UIKit.h>

//Category extension for UILabel
@interface UILabel (IBExtensions)

@property (nonatomic, copy) NSString *fontName;
@end

// Category extension for UITextField
@interface UITextField (IBExtensions)

@property (nonatomic, copy) NSString *fontName;
@end

// Category extension for UIButton
@interface UIButton (IBExtensions)

@property (nonatomic, copy) NSString *fontName;
@end

```

> 
<ol start="3">
- **Getters and Setters:** Define getters and setters for the fontName property towards each category added.
</ol>


### UIKit+IBExtensions.m

```swift
#import "UIKit+IBExtensions.h"

@implementation UILabel (IBExtensions)

- (NSString *)fontName {
    return self.font.fontName;
}

- (void)setFontName:(NSString *)fontName {
    self.font = [UIFont fontWithName:fontName size:self.font.pointSize];
}
@end

@implementation UITextField (IBExtensions)

- (NSString *)fontName {
    return self.font.fontName;
}

- (void)setFontName:(NSString *)fontName {
    self.font = [UIFont fontWithName:fontName size:self.font.pointSize];
}
@end

@implementation UIButton (IBExtensions)

- (NSString *)fontName {
    return self.titleLabel.font.fontName;
}

- (void)setFontName:(NSString *)fontName{
    self.titleLabel.font = [UIFont fontWithName:fontName size:self.titleLabel.font.pointSize];
}
@end

```

> 
<ol start="4">
<li>**Setting font in storyboard:**  Add an entry in User Defined Runtime Attributes with ****fontName**** as keyPath and your <strong><em>Custom
Font's Name</em></strong> as value with type as String as shown.</li>
</ol>


[<img src="http://i.stack.imgur.com/uG7Kh.png" alt="enter image description here" />](http://i.stack.imgur.com/uG7Kh.png)

**This will set your custom font while running the app.**

Notes:

- Lato-Regular is the custom font I have used.
- Same name in the ****.ttf**** file added in bundle should be used without extension in storyboard.
- Font size will be same as it is defined in the UI element's attribute inspector.



## Applying custom fonts  to controls within a Storyboard


The following example shows how to apply custom fonts to a Navigation Bar and includes fixes for some quirky behaviors found in Xcode. One also may apply the custom fonts to **any other UIControls** such as **UILabels**, **UIButtons**, and more by using the attributes inspector after the custom font is added to the project. Please note the external links to working samples and videos near the bottom.

1. Select Your Navigation Bar within your Navigation Controller

[<img src="https://i.stack.imgur.com/kTgul.png" alt="navbar" />](https://i.stack.imgur.com/kTgul.png)

1. Change the Title Font in the Attributes Inspector

[<img src="https://i.stack.imgur.com/FQyiW.png" alt="title-font" />](https://i.stack.imgur.com/FQyiW.png)

**(You will likely need to toggle the Bar Tint for the Navigation Bar before Xcode picks up the new font)**

### Notes (Caveats)

Verified that this does work on Xcode 7.1.1+. (**See the Samples below**)

1. You do need to toggle the nav bar tint before the font takes effect (seems like a bug in Xcode; you can switch it back to default and font will stick)
<li>If you choose a system font ~ Be sure to make sure the size is not
0.0 (Otherwise the new font will be ignored)</li>

[<img src="https://i.stack.imgur.com/33nJ9.png" alt="size" />](https://i.stack.imgur.com/33nJ9.png)

<li>Seems like this works with no problem when only one NavBar is in the view
hierarchy. It appears that secondary NavBars in the same stack are ignored. (Note that if you show the master navigation controller's navBar all the other custom navBar settings are ignored).</li>

### Gotchas (deux)

**Some of these are repeated which means they are very likely worth noting.**

<li>Sometimes the storyboard xml gets corrupt. This requires you to
review the structure in Storyboard as Source Code mode (right click
the storyboard file > Open As ...)</li>
<li>In some cases the navigationItem tag associated with user defined runtime attribute was set as an xml
child of the view tag instead of the view controller tag. If so
remove it from between the  tags for proper operation.</li>
<li>Toggle the NavBar Tint to ensure the custom font is
used.</li>
<li>Verify the size parameter of the font unless using a dynamic font
style</li>
<li>View hierarchy will override the settings. It appears that one font
per stack is possible.</li>

### Result

[<img src="http://s28.postimg.org/gvgs0lxwd/NHg_Ex.png" alt="navbar-italic" />](http://s28.postimg.org/gvgs0lxwd/NHg_Ex.png)

### Samples

- [Video Showing Multiple Fonts In Advanced Project](https://googledrive.com/host/0B8D3kbf8rViMS3MxQ3prcXRXUHM)
- [Simple Source Download](https://bitbucket.org/mingsai/navbarfontupdate/get/b796b40e1ec5.zip)
- [Advanced Project Download ~ Shows Multiple NavBar Fonts & Custom Font Workaround](https://bitbucket.org/mingsai/multiplenavstackfonttest/get/bfd5fcb34be9.zip)
- [Video Showing Multiple Fonts & Custom Fonts](https://googledrive.com/host/0B8D3kbf8rViMUHdUSE1BbW4zUnM)

### Handling Custom Fonts

**Note ~ A [nice checklist](http://codewithchris.com/common-mistakes-with-adding-custom-fonts-to-your-ios-app/) can be found from the Code With Chris website and you can see the sample download project.**

If you have your own font and want to use that in your storyboard, then there is a decent set of answers on the following [SO Question](http://stackoverflow.com/questions/9090745/custom-font-in-a-storyboard#15155081). One answer identifies these steps.

1. Get you custom font file(.ttf,.ttc)
1. Import the font files to your Xcode project
<li>In the app-info.plist,add a key named Fonts provided by
application.It's an array type , add all your font file names to the
array,note:including the file extension.</li>
<li>In the storyboard , on the NavigationBar go to the Attribute
Inspector,click the right icon button of the Font select area.In the
popup panel , choose Font to Custom, and choose the Family of you
embeded font name.</li>

### Custom Font Workaround

So Xcode naturally looks like it can handle custom fonts on UINavigationItem but that feature is just not updating properly (The font selected is ignored).

[<img src="https://i.stack.imgur.com/1SvP0.png" alt="UINavigationItem" />](https://i.stack.imgur.com/1SvP0.png)

> 
**To workaround this:**
<p>One way is to fix using the storyboard and adding a line of
code: First add a UIView (UIButton, UILabel, or some other UIView
subclass) to the View Controller (Not the Navigation Item...Xcode is not currently allowing one to do that). After you add the control
you can modify the font in the storyboard and add a reference as an
outlet to your View Controller. Just assign that view to the
UINavigationItem.titleView. You could also set the text name in code
if necessary. Reported Bug (23600285).</p>


```swift
@IBOutlet var customFontTitleView: UIButton!

//Sometime later...    
self.navigationItem.titleView = customFontTitleView

```

Note - This example is derived from an answer I posted on SO ([here](http://stackoverflow.com/questions/19791762/ios-change-navigation-bar-title-font-and-color/33761674#33761674)).

