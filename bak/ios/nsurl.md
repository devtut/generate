---
metaTitle: "iOS - NSURL"
description: "How to get last string component from NSURL String., How to get last string component from URL (NSURL) in Swift"
---

# NSURL



## How to get last string component from NSURL String.


```

NSURL *url = [NSURL URLWithString:@"http://www.example.com/images/apple-tree.jpg"];
 NSString *fileName = [url lastPathComponent];
 // fileName = "apple-tree.jpg"

```



## How to get last string component from URL (NSURL) in Swift


Swift 2.3

```swift
let url = NSURL(string: "http://google.com/lastPath")
let lastPath = url?.lastPathComponent

```

Swift 3.0

```

 let url = URL(string: "http://google.com/lastPath")
  let lastPath = url?.lastPathComponent

```

