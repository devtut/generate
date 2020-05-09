---
metaTitle: "Objective C - Properties"
description: "Custom getters and setters, What are properties?, Properties that cause updates"
---

# Properties




## Custom getters and setters


The default property getters and setters can be overridden:

```objectivec
@interface TestClass

@property NSString *someString;

@end

@implementation TestClass

// override the setter to print a message
- (void)setSomeString:(NSString *)newString {
    NSLog(@"Setting someString to %@", newString);
    // Make sure to access the ivar (default is the property name with a _ 
    // at the beginning) because calling self.someString would call the same
    // method again leading to an infinite recursion
    _someString = newString;
}

- (void)doSomething {
    // The next line will call the setSomeString: method
    self.someString = @"Test";
}

@end

```

This can be useful to provide, for example, lazy initialization (by overriding the getter to set the initial value if it has not yet been set):

```objectivec
- (NSString *)someString {
    if (_someString == nil) {
        _someString = [self getInitialValueForSomeString];
    }
    return _someString;
}

```

You can also make a property that computes its value in the getter:

```objectivec
@interface Circle : NSObject

@property CGPoint origin;
@property CGFloat radius;
@property (readonly) CGFloat area;

@end

@implementation Circle

- (CGFloat)area {
    return M_PI * pow(self.radius, 2);
}

@end

```



## What are properties?


Here is an example class which has a couple of instance variables, without using properties:

```objectivec
@interface TestClass : NSObject {
    NSString *_someString;
    int _someInt;
}

-(NSString *)someString;
-(void)setSomeString:(NSString *)newString;

-(int)someInt;
-(void)setSomeInt:(NSString *)newInt;

@end


@implementation TestClass

-(NSString *)someString {
    return _someString;
}

-(void)setSomeString:(NSString *)newString {
    _someString = newString;
}

-(int)someInt {
    return _someInt;
}

-(void)setSomeInt:(int)newInt {
    _someInt = newInt;
}

@end

```

This is quite a lot of boilerplate code to create a simple instance variable.  You have to create the instance variable & create accessor methods which do nothing except set or return the instance variable.  So with Objective-C 2.0, Apple introduced properties, which auto-generate some or all of the boilerplate code.

Here is the above class rewritten with properties:

```objectivec
@interface TestClass

@property NSString *someString;
@property int someInt;

@end


@implementation testClass

@end

```

A property is an instance variable paired with auto-generated getters and setters.  For a property called `someString`, the getter and setter are called `someString` and `setSomeString:` respectively.  The name of the instance variable is, by default, the name of the property prefixed with an underscore (so the instance variable for `someString` is called `_someString`, but this can be overridden with an `@synthesize` directive in the `@implementation` section:

```objectivec
@synthesize someString=foo;    //names the instance variable "foo"
@synthesize someString;    //names it "someString"
@synthesize someString=_someString;        //names it "_someString"; the default if 
                                           //there is no @synthesize directive

```

Properties can be accessed by calling the getters and setters:

```objectivec
[testObject setSomeString:@"Foo"];
NSLog(@"someInt is %d", [testObject someInt]);

```

They can also be accessed using dot notation:

```objectivec
testObject.someString = @"Foo";
NSLog(@"someInt is %d", testObject.someInt);

```



## Properties that cause updates


This object, `Shape` has a property `image` that depends on `numberOfSides` and `sideWidth`. If either one of them is set, than the `image` has to be recalculated. But recalculation is presumably long, and only needs to be done once if both properties are set, so the `Shape` provides a way to set both properties and only recalculate once. This is done by setting the property ivars directly.

In `Shape.h`

```objectivec
@interface Shape {
    NSUInteger numberOfSides;
    CGFloat sideWidth;

    UIImage * image;
}

// Initializer that takes initial values for the properties.
- (instancetype)initWithNumberOfSides:(NSUInteger)numberOfSides withWidth:(CGFloat)width;

// Method that allows to set both properties in once call.
// This is useful if setting these properties has expensive side-effects.
// Using a method to set both values at once allows you to have the side-
// effect executed only once.
- (void)setNumberOfSides:(NSUInteger)numberOfSides andWidth:(CGFloat)width;

// Properties using default attributes.
@property NSUInteger numberOfSides;
@property CGFloat sideWidth;

// Property using explicit attributes.
@property(strong, readonly) UIImage * image;

@end

```

In `Shape.m`

```objectivec
@implementation AnObject

// The variable name of a property that is auto-generated by the compiler
// defaults to being the property name prefixed with an underscore, for
// example "_propertyName". You can change this default variable name using
// the following statement:
// @synthesize propertyName = customVariableName;

- (id)initWithNumberOfSides:(NSUInteger)numberOfSides withWidth:(CGFloat)width {
    if ((self = [self init])) {
       [self setNumberOfSides:numberOfSides andWidth:width];
    }

    return self;
}

- (void)setNumberOfSides:(NSUInteger)numberOfSides {
    _numberOfSides = numberOfSides;

    [self updateImage];
}

- (void)setSideWidth:(CGFloat)sideWidth {
    _sideWidth = sideWidth;

    [self updateImage];
}

- (void)setNumberOfSides:(NSUInteger)numberOfSides andWidth:(CGFloat)sideWidth {
    _numberOfSides = numberOfSides;
    _sideWidth = sideWidth;

    [self updateImage];
}

// Method that does some post-processing once either of the properties has
// been updated.
- (void)updateImage {
    ...
}

@end

```

When properties are assigned to (using `object.property = value`), the setter method `setProperty:` is called. This setter, even if provided by `@synthesize`, can be overridden, as it is in this case for `numberOfSides` and `sideWidth`. However, if you set an property's ivar directly (through `property` if the object is self, or `object->property`), it doesn't call the getter or setter, allowing you to do things like multiple property sets that only call one update or bypass side-effects caused by the setter.



#### Syntax


- @property (**optional_attributes, ...**) **type** **identifier**;
- @synthesize **identifier** = **optional_backing_ivar**;
- @dynamic **identifier**;



#### Parameters


|Attribute|Description
|---|---|---|---|---|---|---|---|---|---
|`atomic`|**Implicit.** Enables synchronization in synthesized accessor methods.
|`nonatomic`|Disables synchronization in the synthesized accessor methods.
|`readwrite`|**Implicit.** Synthesizes getter, setter and backing ivar.
|`readonly`|Synthesizes only the getter method and backing ivar, which can be assigned directly.
|`getter=`**name**|Specifies the name of getter method, implicit is `propertyName`.
|`setter=`**name**|Specifies the name of setter method, implicity is `setPropertyName:`. Colon `:` must be a part of the name.
|`strong`|**Implicit for objects under ARC**. The backing ivar is synthesized using `__strong`, which prevents deallocation of referenced object.
|`retain`|Synonym for `strong`.
|`copy`|Same as `strong`, but the synthesized setter also calls `-copy` on the new value.
|`unsafe_unretained`|**Implicit, except for objects under ARC.** The backing ivar is synthesized using `__unsafe_unretained`, which (for obejcts) results in dangling pointer once the referenced object deallocates.
|`assign`|Synonym for `unsafe_unretained`. Suitable for non-object types.
|`weak`|Backing ivar is synthesized using `__weak`, so the value will be nullified once the referenced object is deallocated.
|`class`|Property accessors are synthesized as class methods, instead of instance methods. No backing storage is synthesized.
|`nullable`|The property accepts `nil` values. Mainly used for Swift bridging.
|`nonnull`|The property doesn’t accept `nil` values. Mainly used for Swift bridging.
|`null_resettable`|The property accepts `nil` values  in setter, but never returns `nil` values from getter. Your custom implementation of getter or setter must ensure this behavior. Mainly used for Swift bridging.
|`null_unspecified`|**Implicit.** The property doesn’t specify handling of `nil` values. Mainly used for Swift bridging.

