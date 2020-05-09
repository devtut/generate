---
metaTitle: "iOS - Xcode Build & Archive From Command Line"
description: "Build & Archive"
---

# Xcode Build & Archive From Command Line



## Build & Archive


**Build:**

```swift
xcodebuild -exportArchive -exportFormat ipa \
    -archivePath "/Users/username/Desktop/MyiOSApp.xcarchive" \
    -exportPath "/Users/username/Desktop/MyiOSApp.ipa" \
    -exportProvisioningProfile "MyCompany Distribution Profile"

```

**Archive:**

```swift
xcodebuild -project <ProjectName.xcodeproj> 
    -scheme <ProjectName> 
    -sdk iphonesimulator 
    -configuration Debug 
    -destination "platform=iOS Simulator,name=<Device>,OS=9.3" 
    clean build

```



#### Syntax


- `xcodebuild [-project name.xcodeproj] -scheme schemename [[-destination destinationspecifier] ...] [-destination-timeout value] [-configuration configurationname] [-sdk [sdkfullpath | sdkname]] [action ...] [buildsetting=value ...] [-userdefault=value ...]`



#### Parameters


|Option|Description
|---|---|---|---|---|---|---|---|---|---
|-project|Build the project name.xcodeproj.
|-scheme|Required if building a workspace.
|-destination|Use the destination device
|-configuration|Use the build configuration
|-sdk|specified SDK



#### Remarks


Run `xcodebuild` from the directory containing your project to build an Xcode project. To build an Xcode workspace, you must pass both the **-workspace** and **-scheme** options to define the build.  The parameters of the scheme will
control which targets are built and how they are built, although you may
pass other options to xcodebuild to override some parameters of the
scheme.

