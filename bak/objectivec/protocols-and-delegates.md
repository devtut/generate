---
metaTitle: "Objective C - Protocols and Delegates"
description: "Implementation of Protocols and Delegation mechanism."
---

# Protocols and Delegates



## Implementation of Protocols and Delegation mechanism.


Suppose you have two views `ViewA` and `ViewB`

Instance of `ViewB` is created inside `ViewA`, so `ViewA` can send message to `ViewB's` instance, but for the reverse to happen we need to implement delegation (so that using delegate `ViewB's` instance could send message to `ViewA`)

Follow these steps to implement the delegation

&lt;li>
In `ViewB` create protocol as

```objectivec
 @protocol ViewBDelegate 

-(void) exampleDelegateMethod;

 @end

```


&lt;/li>
&lt;li>
Declare the delegate in the sender class

```objectivec
 @interface ViewB : UIView
 @property (nonatomic, weak) id< ViewBDelegate > delegate;
 @end

```


&lt;/li>
&lt;li>
Adopt the protocol in Class ViewA
`@interfac ViewA: UIView < ViewBDelegate >`
&lt;/li>
&lt;li>
Set the delegate

```objectivec
-(void) anyFunction   
{
    // create Class ViewB's instance and set the delegate
    [viewB setDelegate:self];
}

```


&lt;/li>
&lt;li>
Implement the delegate method in class `ViewA`

```objectivec
-(void) exampleDelegateMethod
{
    // will be called by Class ViewB's instance
}

```


&lt;/li>
&lt;li>
Use the method in class `ViewB` to call the delegate method as

```objectivec
-(void) callDelegateMethod
{
    [delegate exampleDelegateMethod];
    //assuming the delegate is assigned otherwise error
}

```


&lt;/li>



#### Remarks


**Protocols** and **Delegates** are two related but different concept:

A **Protocol** is a interface a class can conforms to, meaning that class implements the listed methods.

A **Delegate** is typically an anonymous object that conforms to a protocol.

The application of **Delegate** called **Delegation** is a design pattern.

At one end we have the concept of **Inheritance** which creates a tight coupling between the subclass and its superclass whereas **Delegation** design pattern provides an alternative to avoid this tight coupling using which we can create a much looser relationship based on anonymous **Delegate** objects.

