---
metaTitle: "React Native - Native Modules"
description: "Create your Native Module (IOS)"
---

# Native Modules



## Create your Native Module (IOS)


### Introduction

from [http://facebook.github.io/react-native/docs/native-modules-ios.html](http://facebook.github.io/react-native/docs/native-modules-ios.html)

> 
Sometimes an app needs access to platform API, and React Native doesn't have a corresponding module yet. Maybe you want to reuse some existing Objective-C, Swift or C++ code without having to reimplement it in JavaScript, or write some high performance, multi-threaded code such as for image processing, a database, or any number of advanced extensions.


A Native Module is simply an Objective-C Class that implements the `RCTBridgeModule` protocol.

### Example

In your Xcode project create a new file and select **Cocoa Touch Class**, in the creation wizard choose a name for your Class (**e.g. NativeModule**), make it a **Subclass of**: `NSObject` and choose `Objective-C` for the language.

This will create two files `NativeModuleEx.h` and `NativeModuleEx.m`

You will need to import `RCTBridgeModule.h` to your `NativeModuleEx.h` file as it follows:

```js
#import <Foundation/Foundation.h>
#import "RCTBridgeModule.h"

@interface NativeModuleEx : NSObject <RCTBridgeModule>

@end

```

In your `NativeModuleEx.m` add the following code:

```js
#import "NativeModuleEx.h"

@implementation NativeModuleEx

RCT_EXPORT_MODULE();

RCT_EXPORT_METHOD(testModule:(NSString *)string )
{
  NSLog(@"The string '%@' comes from JavaScript! ", string);
}

@end

```

`RCT_EXPORT_MODULE()` will make your module accessible in JavaScript, you can pass it an optional argument to specify its name. If no name is provided it will match the Objective-C class name.

`RCT_EXPORT_METHOD()` will expose your method to JavaScript, only the methods you export using this macro will be accessible in JavaScript.

Finally, in your JavaScript you can call your method as it follows:

```js
import { NativeModules } from 'react-native';

var NativeModuleEx = NativeModules.NativeModuleEx;

NativeModuleEx.testModule('Some String !');

```

