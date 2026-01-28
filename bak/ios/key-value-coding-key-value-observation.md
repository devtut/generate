---
metaTitle: "iOS - Key Value Coding-Key Value Observation"
description: "Observing a property of a NSObject subclass, Use of context for KVO Observation"
---

# Key Value Coding-Key Value Observation



## Observing a property of a NSObject subclass


Most KVO and KVC functionality is already implemented by default on all `NSObject` subclasses.

To start observing a property named `firstName` of an object named `personObject` do this in the observing class:

```swift
[personObject addObserver:self
               forKeyPath:@"firstName"
                  options:NSKeyValueObservingOptionNew
                  context:nil];

```

The object that `self` in the above code refers to will then receive a `observeValueForKeyPath:ofObject:change:context:` message whenever the observed key path changes.

```swift
- (void)observeValueForKeyPath:(NSString *)keyPath
                      ofObject:(id)object
                        change:(NSDictionary<NSString *,id> *)change
                       context:(void *)context
{
    NSLog(@"new value of %@ is: %@", keyPath, change[NSKeyValueChangeNewKey]);
}

```

"Key path" is a KVC term. `NSObject` subclasses implement KVC functionality by default.

An instance variable named `_firstName` will be accessible by the `@"firstName"` key path.

A getter method named `firstName` will be called when accessing the `@"firstName"` key path, regardless of there being a `_firstName` instance variable or `setFirstName` setter method.



## Use of context for KVO Observation


```swift
-(void)observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary<NSString *,id> *)change context:(void *)context

```

Context is important if you ship your class for others to use.Context lets your class observer verify that its you observer which is being called.

The problem with not passing an observer is, if some one subclass your class and register an observer for the same object,same key and he does not passes a context ,then the super class observer can be called multiple time.

A variable which is unique and internal for your use is a good context.

For more information.

[importance and good context](http://stackoverflow.com/questions/12719864/best-practices-for-context-parameter-in-addobserver-kvo)



#### Remarks


**KVC** :- Key-Value Coding

Normally instance variables are accessed through properties or accessors but KVC gives another way to access variables in form of strings. In this way your class acts like a dictionary and your property name for example “age” becomes key and value that property holds becomes value for that key.

```swift
For example, you have employee class with "age" property. Normally we access like this.
emp.age = @”20″;
NSString age = emp.age; 

But KVC works like this: 
[emp valueForKey:@"age"]; 
[emp setValue:@"25" forKey:@"age"];

```

**KVO** :- Key-Value Observer

The mechanism through which objects are notified when there is change in any of property is called KVO.
Ex.:keyboard notification

> 
<p>For example, person object is interested in getting notification when
accountBalance property is changed in BankAccount object. To achieve
this, Person Object must register as an observer of the BankAccount’s
accountBalance property by sending an addObserver: forKeyPath:
options: context: message.</p>


