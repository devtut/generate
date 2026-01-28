---
metaTitle: "iOS - NSInvocation"
description: "NSInvocation Objective-C"
---

# NSInvocation




## NSInvocation Objective-C


Refer to this [original Post](http://stackoverflow.com/questions/313400/nsinvocation-for-dummies) by [e.James](http://stackoverflow.com/users/33686/e-james)

According to [Apple's NSInvocation class reference](http://developer.apple.com/documentation/Cocoa/Reference/Foundation/Classes/NSInvocation_Class/Reference/Reference.html):

> 
An `NSInvocation` is an Objective-C message rendered static, that is, it is an action turned into an object.


And, in a **little** more detail:

The concept of messages is central to the objective-c philosophy. Any time you call a method, or access a variable of some object, you are sending it a message. `NSInvocation` comes in handy when you want to send a message to an object at a different point in time, or send the same message several times. `NSInvocation` allows you to **describe** the message you are going to send, and then **invoke** it (actually send it to the target object) later on.

For example, let's say you want to add a string to an array. You would normally send the `addObject:` message as follows:

```swift
[myArray addObject:myString];

```

Now, let's say you want to use `NSInvocation` to send this message at some other point in time:

First, you would prepare an `NSInvocation` object for use with `NSMutableArray`'s `addObject:` selector:

```swift
NSMethodSignature * mySignature = [NSMutableArray
    instanceMethodSignatureForSelector:@selector(addObject:)];
NSInvocation * myInvocation = [NSInvocation
    invocationWithMethodSignature:mySignature];

```

Next, you would specify which object to send the message to:

```swift
[myInvocation setTarget:myArray];

```

Specify the message you wish to send to that object:

```swift
[myInvocation setSelector:@selector(addObject:)];

```

And fill in any arguments for that method:

```swift
[myInvocation setArgument:&myString atIndex:2];

```

Note that object arguments must be passed by pointer. Thank you to [Ryan McCuaig](http://stackoverflow.com/users/53790/ryan-mccuaig) for pointing that out, and please see [Apple's documentation](http://developer.apple.com/mac/library/documentation/cocoa/reference/foundation/Classes/NSInvocation_Class/Reference/Reference.html#//apple_ref/occ/instm/NSInvocation/setArgument:atIndex:) for more details.

At this point, `myInvocation` is a complete object, describing a message that can be sent. To actually send the message, you would call:

```swift
[myInvocation invoke];

```

This final step will cause the message to be sent, essentially executing `[myArray addObject:myString];`.

Think of it like sending an email. You open up a new email (`NSInvocation` object), fill in the address of the person (object) who you want to send it to, type in a message for the recipient (specify a `selector` and arguments), and then click "send" (call `invoke`).

See [Using NSInvocation](http://developer.apple.com/DOCUMENTATION/Cocoa/Conceptual/DistrObjects/Tasks/invocations.html) for more information.

`NSUndoManager` uses `NSInvocation` objects so that it can **reverse** commands. Essentially, what you are doing is creating an `NSInvocation` object to say: "Hey, if you want to undo what I just did, send this message to that object, with these arguments". You give the `NSInvocation` object to the `NSUndoManager`, and it adds that object to an array of undoable actions. If the user calls "Undo", `NSUndoManager` simply looks up the most recent action in the array, and invokes the stored `NSInvocation` object to perform the necessary action.

See [Registering Undo Operations](http://developer.apple.com/documentation/Cocoa/Conceptual/UndoArchitecture/Tasks/RegisteringUndo.html) for more details.

