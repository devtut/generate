---
metaTitle: "Swift - Method Swizzling"
description: "Extending UIViewController and Swizzling viewDidLoad, Basics of Swift Swizzling, Basics of Swizzling - Objective-C"
---

# Method Swizzling




## Extending UIViewController and Swizzling viewDidLoad


In Objective-C, method swizzling is the process of changing the implementation of an existing selector.  This is possible due to the way Selectors are mapped on a dispatch table, or a table of pointers to functions or methods.

Pure Swift methods are not dynamically dispatched by the Objective-C runtime, but we can still take advantage of these tricks on any class that inherits from `NSObject`.

Here, we will extend `UIViewController` and swizzle `viewDidLoad` to add some custom logging:

```swift
extension UIViewController {
    
    // We cannot override load like we could in Objective-C, so override initialize instead
    public override static func initialize() {
        
        // Make a static struct for our dispatch token so only one exists in memory
        struct Static {
            static var token: dispatch_once_t = 0
        }
        
        // Wrap this in a dispatch_once block so it is only run once
        dispatch_once(&Static.token) {
            // Get the original selectors and method implementations, and swap them with our new method
            let originalSelector = #selector(UIViewController.viewDidLoad)
            let swizzledSelector = #selector(UIViewController.myViewDidLoad)
            
            let originalMethod = class_getInstanceMethod(self, originalSelector)
            let swizzledMethod = class_getInstanceMethod(self, swizzledSelector)
            
            let didAddMethod = class_addMethod(self, originalSelector, method_getImplementation(swizzledMethod), method_getTypeEncoding(swizzledMethod))
            
            // class_addMethod can fail if used incorrectly or with invalid pointers, so check to make sure we were able to add the method to the lookup table successfully
            if didAddMethod {
                class_replaceMethod(self, swizzledSelector, method_getImplementation(originalMethod), method_getTypeEncoding(originalMethod))
            } else {
                method_exchangeImplementations(originalMethod, swizzledMethod);
            }
        }
    }
    
    // Our new viewDidLoad function
    // In this example, we are just logging the name of the function, but this can be used to run any custom code
    func myViewDidLoad() {
        // This is not recursive since we swapped the Selectors in initialize().
        // We cannot call super in an extension.
        self.myViewDidLoad()
        print(#function) // logs myViewDidLoad()
    }
}

```



## Basics of Swift Swizzling


Let's swap the implementation of `methodOne()` and `methodTwo()` in our `TestSwizzling` class:

```swift
class TestSwizzling : NSObject {
    dynamic func methodOne()->Int{
        return 1
    }
}

extension TestSwizzling {
    
    //In Objective-C you'd perform the swizzling in load(), 
    //but this method is not permitted in Swift
    override class func initialize()
    {

        struct Inner {
            static let i: () = {

                let originalSelector = #selector(TestSwizzling.methodOne)
                let swizzledSelector = #selector(TestSwizzling.methodTwo)                 
                let originalMethod = class_getInstanceMethod(TestSwizzling.self, originalSelector);
                let swizzledMethod = class_getInstanceMethod(TestSwizzling.self, swizzledSelector)                
                method_exchangeImplementations(originalMethod, swizzledMethod)
            }
        }
        let _ = Inner.i
    }
    
    func methodTwo()->Int{
        // It will not be a recursive call anymore after the swizzling
        return methodTwo()+1
    }
}

var c = TestSwizzling()
print(c.methodOne())
print(c.methodTwo())

```



## Basics of Swizzling - Objective-C


Objective-C example of swizzling UIView's `initWithFrame:` method

```swift
static IMP original_initWithFrame;

+ (void)swizzleMethods {
    static BOOL swizzled = NO;
    if (!swizzled) {
        swizzled = YES;

        Method initWithFrameMethod =
            class_getInstanceMethod([UIView class], @selector(initWithFrame:));
        original_initWithFrame = method_setImplementation(
            initWithFrameMethod, (IMP)replacement_initWithFrame);
    }
}

static id replacement_initWithFrame(id self, SEL _cmd, CGRect rect) {
    
    // This will be called instead of the original initWithFrame method on UIView
    // Do here whatever you need... 

    // Bonus: This is how you would call the original initWithFrame method
    UIView *view =
        ((id (*)(id, SEL, CGRect))original_initWithFrame)(self, _cmd, rect);

    return view;
}

```



#### Remarks


When using method swizzling in Swift there are two requirements that your classes/methods must comply with:

- Your class must extend `NSObject`
- The functions you want to swizzle must have the `dynamic` attribute

For a complete explanation of why this is required, check out [Using Swift with Cocoa and Objective-C](https://developer.apple.com/library/prerelease/ios/documentation/Swift/Conceptual/BuildingCocoaApps/InteractingWithObjective-CAPIs.html#//apple_ref/doc/uid/TP40014216-CH4-XID_38):

> 
**Requiring Dynamic Dispatch**
<p>While the `@objc` attribute exposes your Swift API to the Objective-C
runtime, it does not guarantee dynamic dispatch of a property, method,
subscript, or initializer. <em>The Swift compiler may still devirtualize
or inline member access to optimize the performance of your code,
bypassing the Objective-C runtime</em>. When you mark a member declaration
with the `dynamic` modifier, access to that member is always dynamically
dispatched. Because declarations marked with the `dynamic` modifier are
dispatched using the Objective-C runtime, they’re implicitly marked
with the `@objc` attribute.</p>
<p>Requiring dynamic dispatch is rarely necessary. <strong>However, you must use
the `dynamic` modifier when you know that the implementation of an API
is replaced at runtime</strong>. For example, you can use the
`method_exchangeImplementations` function in the Objective-C runtime to
swap out the implementation of a method while an app is running. If
the Swift compiler inlined the implementation of the method or
devirtualized access to it, **the new implementation would not be used**.</p>


### Links

[Objective-C Runtime Reference](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/ObjCRuntimeRef/)

[Method Swizzling on NSHipster](https://developer.apple.com/library/prerelease/ios/documentation/Swift/Conceptual/BuildingCocoaApps/InteractingWithObjective-CAPIs.html#//apple_ref/doc/uid/TP40014216-CH4-XID_38)

