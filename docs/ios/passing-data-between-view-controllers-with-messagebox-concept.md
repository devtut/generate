---
metaTitle: "iOS - Passing Data between View Controllers (with MessageBox-Concept)"
description: "Simple Example Usage"
---

# Passing Data between View Controllers (with MessageBox-Concept)


MessageBox is a simple concept for decoupling entities.

For example entity A can place a message that entity B can read whenever suitable.

A view controller would like to talk to another view controller, but you don't want to create a strong or weak relationship.



## Simple Example Usage


[<img src="https://i.stack.imgur.com/KM9LD.png" alt="enter image description here" />](https://i.stack.imgur.com/KM9LD.png)

```swift
let messageBox:MessageBox = MessageBox()

// set
messageBox.setObject("TestObject1", forKey:"TestKey1")

// get
// but don't remove it, keep it stored, so that it can still be retrieved later
let someObject:String = messageBox.getObject(forKey:"TestKey1", removeIfFound:false)

// get
// and remove it
let someObject:String = messageBox.getObject(forKey:"TestKey1", removeIfFound:true)

```

