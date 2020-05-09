---
metaTitle: "Objective-C - Multi-Threading"
description: "Creating a simple thread, Create more complex thread, Thread-local storage"
---

# Multi-Threading



## Creating a simple thread


The most simple way to create a thread is by calling a selector "in the background". This means a new thread is created to execute the selector. The receiving object can be any object, not just `self`, but it needs to respond to the given selector.

```objc
- (void)createThread {
    [self performSelectorInBackground:@selector(threadMainWithOptionalArgument:)
                           withObject:someObject];
}

- (void)threadMainWithOptionalArgument:(id)argument {
    // To avoid memory leaks, the first thing a thread method needs to do is
    // create a new autorelease pool, either manually or via "@autoreleasepool".
    @autoreleasepool {
        // The thread code should be here.
    }
}

```



## Create more complex thread


Using a subclass of `NSThread` allows implementation of more complex threads (for example, to allow passing more arguments or to encapsulate all related helper methods in one
class). Additionally, the `NSThread` instance can be saved in a property or variable and can be queried about its current state (whether it's still running).

The `NSThread` class supports a method called `cancel` that can be called from any thread, which then sets the `cancelled` property to `YES` in a thread-safe way. The thread implementation can query (and/or observe) the `cancelled` property and exit its `main` method. This can be used to gracefully shut down a worker thread.

```objc
// Create a new NSThread subclass
@interface MyThread : NSThread

// Add properties for values that need to be passed from the caller to the new
// thread. Caller must not modify these once the thread is started to avoid
// threading issues (or the properties must be made thread-safe using locks).
@property NSInteger someProperty;

@end

@implementation MyThread

- (void)main
{
    @autoreleasepool {
        // The main thread method goes here
        NSLog(@"New thread. Some property: %ld", (long)self.someProperty);
    }
}

@end


MyThread *thread = [[MyThread alloc] init];
thread.someProperty = 42;
[thread start];

```



## Thread-local storage


Every thread has access to a mutable dictionary that is local to the current thread. This allows to cache informations in an easy way without the need for locking, as each thread has its own dedicated mutable dictionary:

```objc
NSMutableDictionary *localStorage = [NSThread currentThread].threadDictionary;
localStorage[someKey] = someValue;

```

The dictionary is automatically released when the thread terminates.

