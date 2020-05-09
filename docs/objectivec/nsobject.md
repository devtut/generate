---
metaTitle: "Objective C - NSObject"
description: "NSObject"
---

# NSObject


`NSObject` is the root class of `Cocoa`, however the `Objective-C` language itself does not define any root classes at all its define by `Cocoa`, Apple's Framework.This root class of most Objective-C class hierarchies, from which subclasses inherit a basic interface to the runtime system and the ability to behave as Objective-C objects.

This class have all basic property of `Objective'C` class object like:

self.

class (name of the class).

superclass (superclass of current class).



## NSObject


`@interface NSString : NSObject`  (`NSObject` is a base class of NSString class).

**You can use below methods for allocation of string class:**

```objectivec
- (instancetype)init

+ (instancetype)new

+ (instancetype)alloc

```

**For Copy any object :**

```objectivec
- (id)copy;

- (id)mutableCopy;

```

**For compare objects :**

```objectivec
- (BOOL)isEqual:(id)object

```

**To get superclass of current class :**

```objectivec
superclass

```

**To check which kind of class is this ?**

```objectivec
- (BOOL)isKindOfClass:(Class)aClass

```

**Some property of NON-ARC classes:**

```objectivec
- (instancetype)retain OBJC_ARC_UNAVAILABLE;

- (oneway void)release OBJC_ARC_UNAVAILABLE;

- (instancetype)autorelease OBJC_ARC_UNAVAILABLE;

- (NSUInteger)retainCount

```



#### Syntax


- self
- superclass
- init
- alloc
- new
- isEqual
- isKindOfClass
- isMemberOfClass
- description

