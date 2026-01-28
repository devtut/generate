---
metaTitle: "iOS - UIPheonix - easy, flexible, dynamic & highly scalable UI framework"
description: "Example UI Components, Example Usage"
---

# UIPheonix - easy, flexible, dynamic & highly scalable UI framework


Inspired by game development UIPheonix is a super easy, flexible, dynamic and highly scalable UI framework + concept for building reusable component/control-driven apps for macOS, iOS and tvOS. The same API apply for cross platform development! Think of it as using Lego blocks, you can use similar ones and move them around easy as pie.

[https://github.com/MKGitHub/UIPheonix](https://github.com/MKGitHub/UIPheonix)



## Example UI Components


[<img src="https://i.stack.imgur.com/TLFzH.png" alt="enter image description here" />](https://i.stack.imgur.com/TLFzH.png)



## Example Usage


```swift
// init
mUIPheonix = UIPheonix(with:myCollectionView)
mUIPheonix = UIPheonix(with:myTableView)

// connect model-view
mUIPheonix.setModelViewRelationships([MyModel.nameOfClass:MyView.nameOfClass])

// add models for the UI
models.append(SimpleButtonModel(id:1, title:"Hello World!"))

// render, update UI
mUIPheonix.setDisplayModels(models)

```



#### Remarks


- Forget static layouts, constraint issues and warning explosions in the console.
- Forget all the glue code, all the boilerplate code and all the very common overly engineered unnecessary pile of garbage code in your apps.
- Build and make changes to your UI quickly in a snap.
- Make your UI reusable.
- Focus on creating your app, not fighting layout issues.
- Minimal setup, minimal impact on your app, lightweight, no dependencies, no pain but so much gain!
- Builds on top of collection views & table views, so you can easily mix and match.
- Does not replace Apple technologies with custom implementations, so you will always be safe and up-to-date, and you can easily revert at any time.
- Demo apps provided for macOS, iOS and tvOS (Kung Fu!)

