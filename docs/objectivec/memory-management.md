---
metaTitle: "Objective C - Memory Management"
description: "Automatic Reference Counting, Strong and weak references, Manual Memory Management, Memory management rules when using manual reference counting."
---

# Memory Management



## Automatic Reference Counting


With automatic reference counting (ARC), the compiler inserts `retain`, `release`, and `autorelease` statements where they are needed, so you don't have to write them yourself. It also writes `dealloc` methods for you.

The sample program from Manual Memory Management looks like this with ARC:

```objectivec
@interface MyObject : NSObject {
    NSString *_property;
}
@end

@implementation MyObject
@synthesize property = _property;

- (id)initWithProperty:(NSString *)property {
    if (self = [super init]) {
        _property = property;
    }
    return self;
}

- (NSString *)property {
    return property;
}

- (void)setProperty:(NSString *)property {
    _property = property;
}

@end

```

```objectivec
int main() {
    MyObject *obj = [[MyObject alloc] init];
    
    NSString *value = [[NSString alloc] initWithString:@"value"];
    [obj setProperty:value];

    [obj setProperty:@"value"];
}

```

You are still able to override the dealloc method to clean up resources not handled by ARC. Unlike when using manual memory management you do not call `[super dealloc]`.

```objectivec
-(void)dealloc {
   //clean up
}

```



## Strong and weak references


A weak reference looks like one of these:

```objectivec
@property (weak) NSString *property;
NSString *__weak variable;

```

If you have a weak reference to an object, then under the hood:

- You're not retaining it.
- When it gets deallocated, every reference to it will automatically be set to `nil`

Object references are always strong by default. But you can explicitly specify that they're strong:

```objectivec
@property (strong) NSString *property;
NSString *__strong variable;

```

A strong reference means that while that reference exists, you are retaining the object.



## Manual Memory Management


This is an example of a program written with manual memory management. You really shouldn't write your code like this, unless for some reason you can't use ARC (like if you need to support 32-bit). The example avoids `@property` notation to illustrate how you used to have to write getters and setters.

```objectivec
@interface MyObject : NSObject {
    NSString *_property;
}
@end

@implementation MyObject
@synthesize property = _property;

- (id)initWithProperty:(NSString *)property {
    if (self = [super init]) {
        // Grab a reference to property to make sure it doesn't go away.
        // The reference is released in dealloc.
        _property = [property retain];
    }
    return self;
}

- (NSString *)property {
    return [[property retain] autorelease];
}

- (void)setProperty:(NSString *)property {
    // Retain, then release. So setting it to the same value won't lose the reference.
    [property retain];
    [_property release];
    _property = property;
}

- (void)dealloc {
    [_property release];
    [super dealloc]; // Don't forget!
}

@end

```

```objectivec
int main() {
    // create object
    // obj is a reference that we need to release
    MyObject *obj = [[MyObject alloc] init];
    
    // We have to release value because we created it.
    NSString *value = [[NSString alloc] initWithString:@"value"];
    [obj setProperty:value];
    [value release];

    // However, string constants never need to be released.
    [obj setProperty:@"value"];
    [obj release];
}

```



## Memory management rules when using manual reference counting.


**These rules apply only if you use manual reference counting!**

<li>
**You own any object you create**
<p>By calling a method whose name begins with `alloc`, `new`, `copy` or `mutableCopy`.
For example:</p>

```objectivec
NSObject *object1 = [[NSObject alloc] init];
NSObject *object2 = [NSObject new];
NSObject *object3 = [object2 copy];

```


That means that you are responsible for releasing these objects when you are done with them.
</li>
<li>
**You can take ownership of an object using retain**
To take ownership for an object you call the retain method.
For example:

```objectivec
NSObject *object = [NSObject new]; // object already has a retain count of 1
[object retain]; // retain count is now 2

```


This makes only sense in some rare situations.
For example when you implement an accessor or an init method to take ownership:

```objectivec
- (void)setStringValue:(NSString *)stringValue {
    [_privateStringValue release]; // Release the old value, you no longer need it
    [stringValue retain]; // You make sure that this object does not get deallocated outside of your scope.
    _privateStringValue = stringValue;
}

```


</li>
<li>
**When you no longer need it, you must relinquish ownership of an object you own**

```objectivec
NSObject* object = [NSObject new]; // The retain count is now 1
[object performAction1]; // Now we are done with the object
[object release]; // Release the object

```


</li>
<li>
**You must not relinquish ownership of an object you do not own**
That means when you didn't take ownership of an object you don't release it.
</li>
<li>
**Autoreleasepool**
The autoreleasepool is a block of code that releases every object in the block that received an autorelease message.
Example:

```objectivec
@autoreleasepool {
    NSString* string = [NSString stringWithString:@"We don't own this object"];
}

```


We have created a string without taking ownership. The `NSString` method `stringWithString:` has to make sure that the string is correctly deallocated after it is no longer needed. Before the method returns the newly created string calls the autorelease method so it does not have to take ownership of the string.
This is how the `stringWithString:` is implemented:

```objectivec
+ (NSString *)stringWithString:(NSString *)string {
    NSString *createdString = [[NSString alloc] initWithString:string];
    [createdString autorelease];
    return createdString;
}

```


It is necessary to use autoreleasepool blocks because you sometimes have objects that you don't own (the fourth rules does not always apply).
Automatic reference counting takes automatically care of the rules so you don't have to.
</li>

