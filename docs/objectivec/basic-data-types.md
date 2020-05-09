---
metaTitle: "Objective-C - Basic Data Types"
description: "BOOL, SEL, id, IMP (implementation pointer), NSInteger and NSUInteger"
---

# Basic Data Types




## BOOL


The `BOOL` type is used for boolean values in Objective-C. It has two values, `YES`, and `NO`, in contrast to the more common "true" and "false".

Its behavior is straightforward and identical to the C language's.

```objc
BOOL areEqual = (1 == 1);    // areEqual is YES
BOOL areNotEqual = !areEqual    // areNotEqual is NO
NSCAssert(areEqual, "Mathematics is a lie");    // Assertion passes

BOOL shouldFlatterReader = YES;
if (shouldFlatterReader) {
    NSLog(@"Only the very smartest programmers read this kind of material.");
}

```

A `BOOL` is a primitive, and so it cannot be stored directly in a Foundation collection. It must be wrapped in an `NSNumber`. Clang provides special syntax for this:

```objc
NSNumber * yes = @YES;    // Equivalent to [NSNumber numberWithBool:YES]
NSNumber * no = @NO;    // Equivalent to [NSNumber numberWithBool:NO]

```

The `BOOL` implementation is directly based on C's, in that it is a typedef of the C99 standard type `bool`. The `YES` and `NO` values are defined to `__objc_yes` and `__objc_no`, respectively. These special values are compiler builtins introduced by Clang, which are translated to `(BOOL)1` and `(BOOL)0`. If they are not available, `YES` and `NO` are defined directly as the cast-integer form. The definitions are found in the Objective-C runtime header objc.h



## SEL


Selectors are used as method identifiers in Objective-C.

In the example below, there are two selectors. `new` and `setName:`

```objc
Person* customer = [Person new];
[customer setName:@"John Doe"];

```

Each pair of brackets corresponds to a message send. On the first line we send a message containing the `new` selector to the `Person` class and on the second line we send a message containing the `setName:` selector and a string. The receiver of these messages uses the selector to look up the correct action to perform.

Most of the time, message passing using the bracket syntax is sufficient, but occasionally you need to work with the selector itself. In these cases, the `SEL` type can be used to hold a reference to the selector.

If the selector is available at compile time, you can use `@selector()` to get a reference to it.

```objc
SEL s = @selector(setName:);

```

And if you need to find the selector at runtime, use NSSelectorFromString.

```objc
SEL s NSSelectorFromString(@"setName:");

```

When using NSSelectorFromString, make sure to wrap the selector name in a NSString.

It is commonly used to check if a delegate implements an optional method.

```objc
if ([self.myDelegate respondsToSelector:@selector(doSomething)]) {
    [self.myDelegate doSomething];
}

```



## id


`id` is the generic object pointer, an Objective-C type representing "any object". An instance of any Objective-C class can be stored in an `id` variable. An `id` and any other class type can be assigned back and forth without casting:

```objc
id anonymousSurname = @"Doe";
NSString * surname = anonymousSurname;
id anonymousFullName = [NSString stringWithFormat:@"%@, John", surname];

```

This becomes relevant when retrieving objects from a collection. The return types of methods like `objectAtIndex:` are `id` for exactly this reason.

```objc
DataRecord * record = [records objectAtIndex:anIndex];  

```

It also means that a method or function parameter typed as `id` can accept any object.

When an object is typed as `id`, any known message can be passed to it: method dispatch does not depend on the compile-time type.

```objc
NSString * extinctBirdMaybe = 
               [anonymousSurname stringByAppendingString:anonymousSurname];

```

A message that the object does not actually respond to will still cause an exception at runtime, of course.

```objc
NSDate * nope = [anonymousSurname addTimeInterval:10];
// Raises "Does not respond to selector" exception

```

Guarding against exception.

```objc
NSDate * nope;
if([anonymousSurname isKindOfClass:[NSDate class]]){
    nope = [anonymousSurname addTimeInterval:10];
}

```

The `id` type is defined in objc.h

```objc
typedef struct objc_object {
    Class isa;
} *id;

```



## IMP (implementation pointer)


IMP is a C type referring to the implementation of a method, also known as an implementation pointer. It is a pointer to the start of a method implementation.

Syntax:

```objc
id (*IMP)(id, SEL, …)

```

IMP is defined by:

```objc
typedef id (*IMP)(id self,SEL _cmd,…);

```

To access this IMP, the message **“methodForSelector”** can be used.

**Example 1:**

```objc
IMP ImpDoSomething = [myObject methodForSelector:@selector(doSomething)];

```

The method adressed by the IMP can be called by dereferencing the IMP.

```objc
ImpDoSomething(myObject, @selector(doSomething));

```

So these calls are equal:

```

myImpDoSomething(myObject, @selector(doSomething));
[myObject doSomething]
[myObject performSelector:mySelector]
[myObject performSelector:@selector(doSomething)]
[myObject performSelector:NSSelectorFromString(@"doSomething")];

```

**Example :2:**

```objc
SEL otherWaySelector = NSSelectorFromString(@“methodWithFirst:andSecond:andThird:");

IMP methodImplementation  = [self methodForSelector:otherWaySelector];

result = methodImplementation( self,
                          betterWaySelector,
                          first,
                          second,
                          third );

NSLog(@"methodForSelector : %@", result);

```

Here, we call [NSObject methodForSelector which returns us a pointer to the C function that actually implements the method, which we can the subsequently call directly.



## NSInteger and NSUInteger


The NSInteger is just a typedef for either an int or a long depending on the architecture. The same goes for a NSUInteger which is a typedef for the unsigned variants. If you check the NSInteger you will see the following:

```objc
#if __LP64__ || (TARGET_OS_EMBEDDED && !TARGET_OS_IPHONE) || TARGET_OS_WIN32 || NS_BUILD_32_LIKE_64
typedef long NSInteger;
typedef unsigned long NSUInteger;
#else
typedef int NSInteger;
typedef unsigned int NSUInteger;
#endif

```

The difference between an signed and an unsigned int or long is that a signed int or long can contain negative values. The range of the int is -2 147 483 648 to 2 147 483 647 while the unsigned int has a range of 0 to 4 294 967 295. The value is doubled because the first bit isn't used anymore to say the value is negative or not. For a long and NSInteger on 64-bit architectures, the range is much wider.

Most methods Apple provides are returning an NS(U)Integer over the normal int. You'll get a warning if you try to cast it to a normal int because you will lose precision if you are running on a 64-bit architecture. Not that it would matter in most cases, but it is easier to use NS(U)Integer. For example, the count method on a array will return an NSUInteger.

```objc
NSNumber *iAmNumber = @0;

NSInteger iAmSigned = [iAmNumber integerValue];
NSUInteger iAmUnsigned = [iAmNumber unsignedIntegerValue];

NSLog(@"%ld", iAmSigned); // The way to print a NSInteger.
NSLog(@"%lu", iAmUnsigned); // The way to print a NSUInteger.

```

Just like a BOOL, the NS(U)Integer is a primitive datatype, so you sometimes need to wrap it in a NSNumber you can use the @ before the integer to cast it like above and retrieve it using the methods below. But to cast it to NSNumber, you could also use the following methods:

```objc
[NSNumber numberWithInteger:0];
[NSNumber numberWithUnsignedInteger:0];

```



#### Syntax


- BOOL havePlutonium = YES;    // Direct assigment
- BOOL fastEnough = (car.speedInMPH >= 88);    // Comparison expression
- BOOL fluxCapacitorActive = (havePlutonium && fastEnough);    // Boolean expression
-  
- id somethingWicked = [witchesCupboard lastObject];    // Retrieve untyped object
- id powder = prepareWickedIngredient(somethingWicked);    // Pass and return
- if ([ingredient isKindOfClass:[Toad class]]) {    // Test runtime type

