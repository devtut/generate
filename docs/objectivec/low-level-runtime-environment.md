---
metaTitle: "Objective C - Low-level Runtime Environment"
description: "Augmenting methods using Method Swizzling, Attach object to another existing object (association), Calling methods directly"
---

# Low-level Runtime Environment



## Augmenting methods using Method Swizzling


The Objective-C runtime allows you to change the implementation of a method at runtime. This is called **method swizzling** and is often used to exchange the implementations of two methods. For example, if the methods `foo` and `bar` are exchanged, sending the message `foo` will now execute the implementation of `bar` and vice versa.

This technique can be used to augment or "patch" existing methods which you cannot edit directly, such as methods of system-provided classes.

In the following example, the `-[NSUserDefaults synchronize]` method is augmented to print the execution time of the original implementation.

**IMPORTANT:** Many people try to do swizzling using `method_exchangeImplementations`. However, this approach is dangerous if you need to call the method you're replacing, because you'll be calling it using a different selector than it is expecting to receive. As a result, your code can break in strange and unexpected ways—particularly if multiple parties swizzle an object in this way.  Instead, you should always do swizzling using `setImplementation` in conjunction with a C function, allowing you to call the method with the original selector.

```objectivec
#import "NSUserDefaults+Timing.h"
#import <objc/runtime.h> // Needed for method swizzling

static IMP old_synchronize = NULL;

static void new_synchronize(id self, SEL _cmd);

@implementation NSUserDefaults(Timing)

+ (void)load
{
    Method originalMethod = class_getInstanceMethod([self class], @selector(synchronize:));
    IMP swizzleImp = (IMP)new_synchronize;
    old_synchronize = method_setImplementation(originalMethod, swizzleImp);
}
@end

static void new_synchronize(id self, SEL _cmd);
{
    NSDate *started;
    BOOL returnValue;

    started = [NSDate date];

    // Call the original implementation, passing the same parameters
    // that this function was called with, including the selector.
    returnValue = old_synchronize(self, _cmd);


    NSLog(@"Writing user defaults took %f seconds.", [[NSDate date] timeIntervalSinceDate:started]);

    return returnValue;
}

@end

```

If you need to swizzle a method that takes parameters, you just add them as additional parameters to the function.  For example:

```objectivec
static IMP old_viewWillAppear_animated = NULL;
static void new_viewWillAppear_animated(id self, SEL _cmd, BOOL animated);

...

Method originalMethod = class_getClassMethod([UIViewController class], @selector(viewWillAppear:));
IMP swizzleImp = (IMP)new_viewWillAppear_animated;
old_viewWillAppear_animated = method_setImplementation(originalMethod, swizzleImp);

...

static void new_viewWillAppear_animated(id self, SEL _cmd, BOOL animated)
{
    ...

    old_viewWillAppear_animated(self, _cmd, animated);

    ...
}

```



## Attach object to another existing object (association)


It's possible to attach an object to an existing object as if there was a new property. This is called **association** and allows one to extend existing objects. It can be used to provide storage when adding a property via a class extension or otherwise add additional information to an existing object.

The associated object is automatically released by the runtime once the target object is deallocated.

```objectivec
#import <objc/runtime.h>

// "Key" for association. Its value is never used and doesn't
// matter. The only purpose of this global static variable is to
// provide a guaranteed unique value at runtime: no two distinct 
// global variables can share the same address.
static char key;

id target = ...;
id payload = ...;
objc_setAssociateObject(target, &key, payload, OBJC_ASSOCIATION_RETAIN);
// Other useful values are OBJC_ASSOCIATION_COPY
// and OBJ_ASSOCIATION_ASSIGN

id queryPayload = objc_getAssociatedObject(target, &key);

```



## Calling methods directly


If you need to call an Objective-C method from C code, you have two ways: using `objc_msgSend`, or obtaining the `IMP` (method implementation function pointer) and calling that.

```objectivec
#import <objc/objc.h>

@implementation Example

- (double)negate:(double)value {
    return -value;
}

- (double)invert:(double)value {
    return 1 / value;
}

@end

// Calls the selector on the object. Expects the method to have one double argument and return a double.
double performSelectorWithMsgSend(id object, SEL selector, double value) {
    // We declare pointer to function and cast `objc_msgSend` to expected signature.
    // WARNING: This step is important! Otherwise you may get unexpected results!
    double (*msgSend)(id, SEL, double) = (typeof(msgSend)) &objc_msgSend;

    // The implicit arguments of self and _cmd need to be passed in addition to any explicit arguments.
    return msgSend(object, selector, value);
}

// Does the same as the above function, but by obtaining the method's IMP.
double performSelectorWithIMP(id object, SEL selector, double value) {
    // Get the method's implementation.
    IMP imp = class_getMethodImplementation([self class], selector);

    // Cast it so the types are known and ARC can work correctly.
    double (*callableImp)(id, SEL, double) = (typeof(callableImp)) imp;

    // Again, you need the explicit arguments.
    return callableImp(object, selector, value);
} 

int main() {
    Example *e = [Example new];

    // Invoke negation, result is -4
    double x = performSelectorWithMsgSend(e, @selector(negate:), 4);

    // Invoke inversion, result is 0.25
    double y = performSelectorWithIMP(e, @selector(invert:), 4);
}

```

`objc_msgSend` works by obtaining the IMP for the method and calling that. The `IMP`s for the last several methods called are cached, so if you're sending an Objective-C message in a very tight loop you can get acceptable performance. In some cases, manually caching the IMP can give slightly better performance, although this is a last resort optimization.



#### Remarks


In order to use the Objective-C runtime, you need to import it.

```objectivec
#import <objc/objc.h>

```

