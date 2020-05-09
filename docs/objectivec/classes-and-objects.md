---
metaTitle: "Objective-C - Classes  and Objects"
description: "Difference between allocation and initialization, Creating classes with initialization values, Singleton Class, The instancetype return type, Specifying Generics"
---

# Classes  and Objects



## Difference between allocation and initialization


In most object oriented languages, allocating memory for an object and initializing it is an atomic operation:

```objc
// Both allocates memory and calls the constructor
MyClass object = new MyClass();

```

In Objective-C, these are separate operations. The class methods `alloc` (and its historic sibling `allocWithZone:`) makes the Objective-C runtime reserve the required memory and clears it. Except for a few internal values, all properties and variables are set to 0/`NO`/`nil`.

The object then is already "valid" but we always want to call a method to actually set up the object, which we call an **initializer**. These serve the same purpose as **constructors** in other languages. By convention, these methods start with `init`. From a language point of view, they are just normal methods.

```objc
// Allocate memory and set all properties and variables to 0/NO/nil.
MyClass *object = [MyClass alloc];
// Initialize the object.
object = [object init];

// Shorthand:
object = [[MyClass alloc] init];

```



## Creating classes with initialization values


```objc
#import <Foundation/Foundation.h>
@interface Car:NSObject {
    NSString *CarMotorCode;
    NSString *CarChassisCode;
}

- (instancetype)initWithMotorValue:(NSString *) motorCode andChassisValue:(NSInteger)chassisCode;
- (void) startCar;
- (void) stopCar;

@end

@implementation Car

- (instancetype)initWithMotorValue:(NSString *) motorCode andChassisValue:(NSInteger)chassisCode{
    CarMotorCode = motorCode;
    CarChassisCode = chassisCode;
    return self;
}

- (void) startCar {...}
- (void) stopCar {...}

@end

```

The method `initWithMotorValue: type andChassisValue: type` will be used to initialize the Car objects.



## Singleton Class


**What is a Singleton Class?**

A singleton class returns the same instance no matter how many times an application requests it. Unlike a regular class, A singleton object provides a global point of access to the resources of its class.

**When to Use Singleton Classes?**

Singletons are used in situations where this single point of control is desirable, such as with classes that offer some general service or resource.

**How to Create Singleton Classes**

First, create a New file and subclass it from `NSObject`. Name it anything, we will use `CommonClass` here. Xcode will now generate CommonClass.h and CommonClass.m files for you.

In your `CommonClass.h` file:

```objc
#import <Foundation/Foundation.h>

@interface CommonClass : NSObject {
}
+ (CommonClass *)sharedObject;
@property NSString *commonString;
@end

```

In your `CommonClass.m` File:

```objc
#import "CommonClass.h"

@implementation CommonClass

+ (CommonClass *)sharedObject {
    static CommonClass *sharedClass = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedClass = [[self alloc] init];
    });
    return sharedClass;
}

- (id)init {
    if (self = [super init]) {
        self.commonString = @"this is string";
    }
    return self;
}

@end

```

**How to Use Singleton Classes**

The Singleton Class that we created earlier will be accessible from anywhere in the project as long as you have imported `CommonClass.h` file in the relevant module. To modify and access the shared data in Singleton Class, you will have to access the shared Object of that class which can be accessed by using `sharedObject` method like following:

```objc
[CommonClass sharedObject]

```

To read or modify the elements in Shared Class, do the following:

```objc
NSString *commonString = [[CommonClass sharedObject].commonString; //Read the string in singleton class

NSString *newString = @"New String";
[CommonClass sharedObject].commonString = newString;//Modified the string in singleton class

```



## The "instancetype" return type


Objective-C supports a special type called `instancetype that can only be used as type returned by a method. It evaluates to the class of the receiving object.

Consider the following class hierarchy:

```objc
@interface Foo : NSObject

- (instancetype)initWithString:(NSString *)string;

@end

@interface Bar : Foo
@end


```

When `[[Foo alloc] initWithString:@"abc"]` is called, the compiler can infer that the return type is `Foo *`. The `Bar` class derived from `Foo` but did not override the declaration of the initializer. Yet, thanks to `instancetype`, the compiler can infer that `[[Bar alloc] initWithString:@"xyz"]` returns a value of type `Bar *`.

Consider the return type of `-[Foo initWithString:]` being `Foo *` instead: if you would call `[[Bar alloc] initWithString:]`, the compiler would infer that a `Foo *` is returned, not a `Bar *` as is the intention of the developer. The `instancetype` solved this issue.

Before the introduction of `instancetype`, initializers, static methods like singleton accessors and other methods that want to return an instance of the receiving class needed to return an `id`. The problem is that `id` means **"an object of any type"**. The compiler is thus not able to detect that `NSString *wrong = [[Foo alloc] initWithString:@"abc"];` is assigning to a variable with an incorrect type.

Due to this issue, **initializers should always use `instancetype` instead of `id`** as the return value.



## Specifying Generics


You can enhance your own classes with **generics** just like `NSArray` or `NSDictionary`.

```objc
@interface MyClass<__covariant T>

@property (nonnull, nonatomic, strong, readonly) NSArray<T>* allObjects;

- (void) addObject:(nonnull T)obj;

@end

```



#### Syntax


- Cat *cat = [[Cat alloc] init];     // Create cat object of type Cat
- Dog *dog = [[Dog alloc] init];     // Create dog object of type Dog
<li>NSObject *someObject = [NSObject alloc];
[someObject init]; // **don’t do this**</li>
- XYZObject *object = [XYZObject new]; // Use new to create objects if NO arguments are needed for initialization
- NSString *someString = @"Hello, World!"; // Creating an NSString with **literal syntax**
- NSNumber *myFloat = @3.14f; // Another example to create a NSNumber using literal syntax
- NSNumber *myInt = @(84 / 2); // Create an object using a boxed expression

