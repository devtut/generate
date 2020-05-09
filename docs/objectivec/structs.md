---
metaTitle: "Objective C - Structs"
description: "CGPoint, Defining a Structure and Accessing Structure Members"
---

# Structs



## CGPoint


One really good example of a struct is `CGPoint`; it's a simple value that represents a 2-dimensional point. It has 2 properties, `x` and `y`, and can be written as

```objectivec
typedef struct {
    CGFloat x;
    CGFloat y;
} CGPoint;

```

If you used Objective-C for Mac or iOS app development before, you've almost certainly come across `CGPoint`; `CGPoint`s hold the position of pretty much everything on screen, from views and controls to objects in a game to changes in a gradient. This means that `CGPoint`s are used a lot. This is even more true with really performance-heavy games; these games tend to have a lot of objects, and all of these objects need positions. These positions are often either `CGPoint`s, or some other type of struct that conveys a point (such as a 3-dimensional point for 3d games).

Points like `CGPoint` could easily be represented as objects, like

```objectivec
@interface CGPoint {
    CGFloat x;
    CGFloat y;
}

... //Point-related methods (e.g. add, isEqualToPoint, etc.)

@property(nonatomic, assign)CGFloat x;
@property(nonatomic, assign)CGFloat y;

@end

@implementation CGPoint

@synthesize x, y;

...

@end

```

However, if `CGPoint` was used in this way it would take a lot longer to create and manipulate points. In smaller, faster programs this wouldn't really cause a difference, and in those cases it would be OK or maybe even better to use object points. But in large programs where points are be used a lot, using objects as points can really hurt performance, making the program slower, and also waste memory, which could force the program to crash.



## Defining a Structure and Accessing Structure Members


The format of the struct statement is this:

```objectivec
struct [structure tag]
{
   member definition;
   member definition;
   ...
   member definition;
} [one or more structure variables]; 

```

Example: declare the ThreeFloats structure:

```

  typedef struct {
    float x, y, z;
} ThreeFloats;

@interface MyClass
- (void)setThreeFloats:(ThreeFloats)threeFloats;
- (ThreeFloats)threeFloats;
@end

```

Sending an instance of MyClass the message valueForKey: with the parameter @"threeFloats" will invoke the MyClass method threeFloats and return the result wrapped in an NSValue.



#### Syntax


- typedef struct { **typeA propertyA**; **typeB propertyB**; ... } **StructName**



#### Remarks


In Objective C, you should almost always use an object instead of a struct. However, there are still cases where using a struct is better, such as:

- When you're going to be creating and destroying a lot of values of the (struct) type, so you need good performance and small memory usage

> 
<ul>
- Structs are faster to create and use because when calling a method on an object, the method has to be determined at runtime
- Structs take up less size because objects have an extra property `isa`, which holds their class
</ul>


- When the value has only a couple of properties and a small total size (take `CGSize`; it has 2 floats which are 4 bytes each, so it can take up 8 bytes), and is going to be used a lot (ties in with the first point)
- When you could use [unions](http://stackoverflow.com/documentation/c/1119/structs-unions-and-enums#t=201607251849213065581) or [bitfields](http://stackoverflow.com/documentation/c/1930/bitfields#t=201607251847447438382), and importantly, **need the size saved by them** because you need small memory usage (ties in with the first point)
- When you **really** want to store an array inside of the struct, since Objective-C objects can't directly store C-arrays. However, note that you can still "indirectly" get an array in an Objective-C object by making it a reference (i.e. `type *` in place of the C-array `type[]`)
- When you need to communicate with some other code, such as a library, that's coded in C; structs are fully implemented in C but objects are not

