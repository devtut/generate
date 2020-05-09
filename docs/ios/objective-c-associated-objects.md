---
metaTitle: "iOS - Objective-C Associated Objects"
description: "Basic Associated Object Example"
---

# Objective-C Associated Objects


First introduced in iOS 3.1 as part of the Objective-C runtime, associated objects provide a way to add instance variables to an existing class object (w\o subclassing.

This means you'll be able to attach any object to any other object without subclassing.



## Basic Associated Object Example


Assume we need to add an NSString object to `SomeClass` (we cant subclass).

In this example we not only create an associated object but also wrap it in a computed property in a category for extra neatness

```swift
#import <objc/runtime.h>

@interface SomeClass (MyCategory)
// This is the property wrapping the associated object. below we implement the setter and getter which actually utilize the object association
@property (nonatomic, retain) NSString *associated;
@end

@implementation SomeClass (MyCategory)

- (void)setAssociated:(NSString *)object {
    objc_setAssociatedObject(self, @selector(associated), object,
                             OBJC_ASSOCIATION_RETAIN_NONATOMIC);
}

- (NSString *)associated {
    return objc_getAssociatedObject(self, @selector(associated));
}


```

Now it would be as easy as this to use the property

```swift
SomeClass *instance = [SomeClass alloc] init];
instance.associated = @"this property is an associated object under the hood";


```



#### Syntax


<li>
void objc_setAssociatedObject(id object, void *key, id value, objc_AssociationPolicy policy)
</li>
<li>
id objc_getAssociatedObject(id object, void *key)
</li>
<li>
void objc_removeAssociatedObjects(id object)
</li>



#### Parameters


|Param|Details
|---|---|---|---|---|---|---|---|---|---
|object|The existing object you want to modify
|key|This can basically be any pointer that has a constant memory address, but a nice practice is to use here a computed property (getter)
|value|The object you want to add
|policy|The memory policy for this new `value` i.e. should it be retained / assigned, copied etc.. just like any other property you'd declare



#### Remarks


More details here:

[NSHipster](http://nshipster.com/associated-objects/)

[@kostiakoval](https://medium.com/@kostiakoval/objective-c-associated-objects-8896854c681b#.i0p22bcl6)

[kingscocoa](http://kingscocoa.com/tutorials/associated-objects/)

