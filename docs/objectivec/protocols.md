---
metaTitle: "Objective-C - Protocols"
description: "Optional and required methods, Checking existance of optional method implementations, Basic Protocol Definition, Conforming to Protocols, Forward Declarations, Check conforms Protocol"
---

# Protocols



## Optional and required methods


By default, all the methods declared in a protocol are required. This means that any class that conforms to this protocol must implement those methods.

It is also possible to declare **optional** methods. These method can be implemented only if needed.

You mark optional methods with the `@optional` directive.

```objc
@protocol NewProtocol
- (void)protocolMethod:(id)argument;
@optional
- (id)anotherMethod;
@end

```

In this case, only `anotherMethod` is marked as optional; the methods without the `@optional` directive are assumed to be required.

The `@optional` directive applies to methods that follow, until the end of the protocol definition or, until another directive is found.

```objc
@protocol NewProtocol
- (void)protocolMethod:(id)argument;
@optional
- (id)anotherMethod;
- (void)andAnotherMethod:(id)argument;
@required
- (void)lastProtocolMethod;
@end

```

This last example defines a protocol with two optional methods and two required methods.



## Checking existance of optional method implementations


```objc
if ([object respondsToSelector:@selector(someOptionalMethodInProtocol:)])
{
    [object someOptionalMethodInProtocol:argument];
}

```



## Basic Protocol Definition


Defining a new protocol:

```objc
@protocol NewProtocol

- (void)protocolMethod:(id)argument;

- (id)anotherMethod;

@end

```



## Conforming to Protocols


The following syntax indicate that a class adopts a protocol, using angle brackets.

```objc
@interface NewClass : NSObject <NewProtocol>
...
@end

```

This means that any instance of NewClass will respond to methods declared in its interface but also it will provide an implementation for all the required methods of `NewProtocol`.

It is also possible for a class to conform to multiple protocols, by separating them with comma.

```objc
@interface NewClass : NSObject <NewProtocol, AnotherProtocol, MyProtocol>
...
@end

```

Like when conforming to a single protocol, the class must implement each required method of each protocols, and each optional method you choose to implement.



## Forward Declarations


It's possible to declare protocol name without methods:

```objc
@protocol Person;

```

use it your code (class definition, etc):

```objc
@interface World : NSObject
@property (strong, nonatomic) NSArray<id<some>> *employees;
@end

```

and later define protocol's method somewhere in your code:

```objc
@protocol Person
- (NSString *)gender;
- (NSString *)name;
@end

```

It's useful when you don't need to know protocols details until you import that file with protocol definition. So, your class header file stays clear and contains details of the class only.



## Check conforms Protocol


Returns a Boolean indicating if the class conform the protocol:

`[MyClass conformsToProtocol:@protocol(MyProtocol)`];

