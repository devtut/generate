---
metaTitle: "Swift - Swift Package Manager"
description: "Creation and usage of a simple Swift package"
---

# Swift Package Manager



## Creation and usage of a simple Swift package


To create a Swift Package, open a Terminal then create an empty folder:

```swift
import PackageDescription

let package = Package(
    name: "AwesomeProject"
)

```

Versioning the package is done with Git tags:

```swift
import PackageDescription

let package = Package(
    name: "AwesomeProject",
    targets: [],
    dependencies: [
        .Package(url: "https://github.com/someUser/SomeOtherPackage.git",
                 majorVersion: 1),
    ]
)

```

Then build your project again: the Swift Package Manager will automatically resolve, download and build the dependencies.

