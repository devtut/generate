---
metaTitle: "Xamarin - Binding Swift Libraries"
description: "Binding a Swift Library in Xamarin.iOS"
---

# Binding Swift Libraries


An easy to follow guide that will lead you through the process of binding Swift .framework files for use in a Xamarin project.



## Binding a Swift Library in Xamarin.iOS


Binding a Swift Library in Xamarin.iOS follows the same process for Objective-C as shown in [https://developer.xamarin.com/guides/ios/advanced_topics/binding_objective-c/](https://developer.xamarin.com/guides/ios/advanced_topics/binding_objective-c/), but with some caveats.

1. A swift class must inherit from NSObject to be binded.
1. Swift compiler will translate class and protocol names into something else unless you use the [@objc](https://developer.apple.com/library/content/documentation/Swift/Conceptual/BuildingCocoaApps/InteractingWithObjective-CAPIs.html#//apple_ref/doc/uid/TP40014216-CH4-ID35) annotation (e.g. @objc(MyClass)) in your swift classes to specify the explicit objective c name.
1. In runtime your APP must include some swift core libraries alongside your binded framework in a folder called Frameworks;
1. When the App is pushed to AppStore it must include a SwiftSupport folder alongside your Payload folder. Those are inside the IPA file.

Here you can find a simple sample binding: [https://github.com/Flash3001/Xamarin.BindingSwiftLibrarySample](https://github.com/Flash3001/Xamarin.BindingSwiftLibrarySample)

And a full binding sample: [https://github.com/Flash3001/iOSCharts.Xamarin](https://github.com/Flash3001/iOSCharts.Xamarin)

Please find steps below:

### 1.1 Prepare the Swift classes you want to export

For any Swift class you want to use you either have to inherit from NSObject and make the Objective-C name explicit using the objc annotation. Otherwise the Swift compiler will generate different names. Below is an example code of how a Swift class could look like. Note that it doesn't matter which class it inherits as long as the root class inherits from NSObject.

```cs
//Add this to specify explicit objective c name
@objc(MyClass)
open class MyClass: NSObject {
    open func getValue() -> String
    {
        return "Value came from MyClass.swift!";
    }
}

```

### 1.2 Build the framework

Disable Bitcode. *

[<img src="https://i.stack.imgur.com/LStuW.png" alt="Disable Bitcode" />](https://i.stack.imgur.com/LStuW.png)

Build for release for Device and Simulator. *
[<img src="https://i.stack.imgur.com/0kbTg.png" alt="Set it as Release in Xcode Schemes" />](https://i.stack.imgur.com/0kbTg.png)
[<img src="https://i.stack.imgur.com/d9doz.png" alt="Build for Device" />](https://i.stack.imgur.com/d9doz.png)

[<img src="https://i.stack.imgur.com/8bW84.png" alt="Build for Simulator" />](https://i.stack.imgur.com/8bW84.png)

[<img src="https://i.stack.imgur.com/MXUnB.png" alt="Find frameworks" />](https://i.stack.imgur.com/MXUnB.png)

[<img src="https://i.stack.imgur.com/OZotY.png" alt="Frameworks" />](https://i.stack.imgur.com/OZotY.png)

- Not related only to Swift binding.

### 2. Create a fat library

A framework contains several files, the one that needs to eat a little is NAME.framework/NAME (without extension).

- Copy Release-iphoneos/NAME.framework to NAME.framework
<li>Create the FAT library using:
<ul>
<li><strong>lipo -create Release-iphonesimulator/NAME.framework/NAME
Release-iphoneos/NAME.framework/NAME -output NAME.framework/NAME</strong></li>

[<img src="https://i.stack.imgur.com/0XE0b.png" alt="Create fat library" />](https://i.stack.imgur.com/0XE0b.png)

[<img src="https://i.stack.imgur.com/zmmCf.png" alt="Copy pending files" />](https://i.stack.imgur.com/zmmCf.png)

[<img src="https://i.stack.imgur.com/BYtfy.png" alt="Files copied" />](https://i.stack.imgur.com/BYtfy.png)

### 3. Import the library

I’ll assume you already created the Binding project in File -> New -> iOS -> Binding Library.

Xamarin support importing .frameworks. Just right click ‘Native References’ and click in ‘Add native reference’. Find the newly created fat framework and add it.

[<img src="https://i.stack.imgur.com/DVoC8.png" alt="Add Native Reference" />](https://i.stack.imgur.com/DVoC8.png)

[<img src="https://i.stack.imgur.com/y0qai.png" alt="Select .framework" />](https://i.stack.imgur.com/y0qai.png)

[<img src="https://i.stack.imgur.com/DCveu.png" alt="Reference" />](https://i.stack.imgur.com/DCveu.png)

### 4. Create the ApiDefinition based on LIBRARY-Swift.h file inside headers.

You can do it manually, but it won’t be nice. You can use Objetive Sharpie. The tool Xamarin uses to bind its own libraries.

How to use it on [https://developer.xamarin.com/guides/cross-platform/macios/binding/objective-sharpie/](https://developer.xamarin.com/guides/cross-platform/macios/binding/objective-sharpie/)

The basic command will be something like: ****sharpie bind -sdk iphoneos9.3 NAME-Swift.h****

If you get a `System.Reflection.TargetInvocationException` it is probably because you have a different SDK version installed. Run the following command to check with iPhone OS SDK you have installed:

```cs
sharpie xcode -sdks

```

The file ****NAME-Swift.h**** is located in ****NAME.framework/Headers/NAME-Swift.h****

Note: The swift classes must inherit from "NSObject", otherwise ****NAME-Swift.h**** won't import your classes and Objetive Sharpie will convert nothing.

[<img src="https://i.stack.imgur.com/DCveu.png" alt="Sharpie" />](https://i.stack.imgur.com/DCveu.png)

Replace the contents of your binding project ApiDefinition.cs with the newly created one.
[<img src="https://i.stack.imgur.com/Sq9TQ.png" alt="Replace file contents" />](https://i.stack.imgur.com/Sq9TQ.png)

### 5. Change all [Protocol] and [BaseType] to include the class’ name in Objective-C runtime.

In case the original Swift class or protocol doesn't include the @objc(MyClass) annotation as specified in step 1.1 they will have its internal Objective-C names changed, so you need to map it to the right one.

All names are available in the file NAME-Swift.h in the following format:

```cs
SWIFT_CLASS("_TtC11SwiftSample7MyClass")
@interface MyClass : NSObject

```

And

```cs
SWIFT_PROTOCOL("_TtP6Charts17ChartDataProvider_")
@protocol ChartDataProvider

```

To set the name you use BaseTypeAttribute.Name [https://developer.xamarin.com/guides/cross-platform/macios/binding/binding-types-reference/#BaseType.Name](https://developer.xamarin.com/guides/cross-platform/macios/binding/binding-types-reference/#BaseType.Name) property for classes and ProcotolAttribute.Name [https://developer.xamarin.com/api/property/MonoTouch.Foundation.ProtocolAttribute.Name/](https://developer.xamarin.com/api/property/MonoTouch.Foundation.ProtocolAttribute.Name/) for protocols.

```cs
[BaseType(typeof(NSObject), Name = "_TtC11SwiftSample7MyClass")]
interface MyClass

```

Doing it manually is not cool. You can use this tool [https://github.com/Flash3001/SwiftClassify](https://github.com/Flash3001/SwiftClassify) to insert all the names. (It’s work in progress. But it’s quite simple, just by looking at the code you will get how it works).

### 6.1 Include all Swift dependencies to run.

If you try to consume the library in an App and try to run it right now it will crash. The error is due to the lack of libswiftCore.dylib

Something like this:

```cs
Dyld Error Message:
  Library not loaded: @rpath/libswiftCore.dylib
  Referenced from: /Users/USER/Library/Developer/CoreSimulator/Devices/AC440891-C819-4050-8CAB-CE15AB4B3830/data/Containers/Bundle/Application/27D2EC87-5042-4FA7-9B80-A24A8971FB48/SampleUsing.app/Frameworks/SwiftSample.framework/SwiftSample
  Reason: image not found

```

Xamarin.iOS doesn’t give official support for binding a Swift library. So you must manually include the swift core libraries in the Frameworks and SwiftSupport folders. The files for the Frameworks folder are different for Simulator and Device. They can be found in /Applications/Xcode.app/Contents/Developer//XcodeDefault.xctoolchain/usr/lib/swift.Toolchains

Instead of manually copying the files inside the Framework folder you can use this library [https://github.com/Flash3001/Xamarin.Swift3.Support](https://github.com/Flash3001/Xamarin.Swift3.Support) . It includes every single dependency Swift 3.1 needs, each one in a single NuGet package.

[<img src="https://i.stack.imgur.com/Hye5k.png" alt="Add NuGet Package" />](https://i.stack.imgur.com/Hye5k.png)

As you can see, the Nuget Package is included in the consumer App, not in the binding itself. If you try to include it in the binding you will get compile errors.

If you are building a Nuget Package you can instruct Nuget to include it as a dependency.

[<img src="https://i.stack.imgur.com/Fx0GW.png" alt="App Running" />](https://i.stack.imgur.com/Fx0GW.png)

### 6.2. Finding out which Swift dependencies to include.

An important thing to do is to figure out each package you need to include in your project. A simple binding will usually need:

```

    libswiftCore.dylib
     libswiftCoreGraphics.dylib
     libswiftCoreImage.dylib
     libswiftDarwin.dylib
     libswiftDispatch.dylib
     libswiftFoundation.dylib
     libswiftObjectiveC.dylib
     libswiftQuartzCore.dylib
     libswiftUIKit.dylib   

```

To list each dependency you can run the following command inside your LibraryName.framework

```cs
otool -l -arch armv7 LibraryName | grep libswift

```

[<img src="https://i.stack.imgur.com/dlZKB.png" alt="List dependencies" />](https://i.stack.imgur.com/dlZKB.png)

Do not include every package available in NuGet for Swift3 as they might grow your app size.

### 7. Include SwiftSupport to push App to AppStore.

Apple requires your App to be sent with a SwiftSupport folder alongside your Payload folder. Both are inside your IPA package.

You can use this script [https://github.com/bq/ipa-packager](https://github.com/bq/ipa-packager) to do this work for you.

This process is the only one the library consumer will have to do manually. Every time he/she tries to push the App to AppStore.

[<img src="https://i.stack.imgur.com/1MkHy.png" alt="Archive for Publishing" />](https://i.stack.imgur.com/1MkHy.png)

Click 'Sign and Distribute' and Save to Disk
[<img src="https://i.stack.imgur.com/azNYM.png" alt="Sign and Distribute" />](https://i.stack.imgur.com/azNYM.png)

Unzip your .IPA
[<img src="https://i.stack.imgur.com/IVSGg.png" alt="Unzip" />](https://i.stack.imgur.com/IVSGg.png)

Create the new IPA using the script before mentioned
[<img src="https://i.stack.imgur.com/v4ckT.png" alt="New API" />](https://i.stack.imgur.com/v4ckT.png)

If you Unzip the file know, it will contain the SwiftSupport folder.
[<img src="https://i.stack.imgur.com/7qT8K.png" alt="enter image description here" />](https://i.stack.imgur.com/7qT8K.png)

### Remarks

When building a library in Xcode it has an option to include the swift libraries. Don’t! They will be included in your final app as NAME.app/Frameworks/LIBRARY.framework/Frameworks/libswift*.dylib but they must be included as NAME.app/Frameworks/libswift*.dylib

You can find this information elsewhere, but it’s worth mention: Don’t include Bitcode in the library. As of right now Xamarin don’t include Bitcode for iOS and Apple requires all libraries to support the same architectures.

### Disclaimer

This guide is originally created by [Lucas Teixeira](http://stackoverflow.com/users/1698917/lucas-teixeira).
All credits belong to him. Thank you, Lucas.



#### Remarks


<li>
When building a library in Xcode it has an option to include the swift libraries. Don’t! They will be included in your final app as NAME.app/Frameworks/LIBRARY.framework/Frameworks/libswift*.dylib but they must be included as NAME.app/Frameworks/libswift*.dylib
</li>
<li>
You can find this information elsewhere, but it’s worth mention: Don’t include Bitcode in the library. As of right now Xamarin don’t include Bitcode for iOS and Apple requires all libraries to support the same architectures.
</li>

