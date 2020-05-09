---
metaTitle: "iOS - Accessibility"
description: "Make a View Accessible, Accessibility Frame, Layout Change, Accessibility Container, Hiding Elements, Screen Change, Announcement, Ordering Elements, Modal View"
---

# Accessibility


Accessibility in iOS allows users with hearing disabilities and visual impairments to access iOS and your application by supporting various features like VoiceOver, Voice Control, White on Black, Mono Audio, Speech to Text and so on. Providing Accessibility in the iOS app means making the app usable for everyone.



## Make a View Accessible


Mark your `UIView` subclass as an accessible element so that it is visible to VoiceOver.

```swift
myView.isAccessibilityElement = YES;

```

Ensure that the view speaks a meaningful label, value, and hint. Apple provides more details on how to choose good descriptions in the [Accessibility Programming Guide](https://developer.apple.com/library/ios/documentation/UserExperience/Conceptual/iPhoneAccessibility/Introduction/Introduction.html).



## Accessibility Frame


The accessibility frame is used by VoiceOver for hit testing touches, drawing the VoiceOver cursor, and calculating where in the focused element to simulate a tap when the user double-taps the screen. Note that the frame is in screen coordinates!

```swift
myElement.accessibilityFrame = frameInScreenCoordinates;

```

If your elements or screen layouts change often, consider overriding -`accessibilityFrame` to always provide an up-to-date rect.
Calculating the screen-relative frame of scroll view subviews can be error-prone and tedious. iOS 10 introduces a new API to make this easier: `accessibilityFrameInContainerSpace`.



## Layout Change


In many cases, content within a single screen will update with new or different content. For example, imagine a form that reveals additional options based on the user’s answer to a previous question. In this case, a “layout change” notification lets you either announce the change or focus on a new element. This notification accepts the same parameters as the screen change notification.

```swift
UIAccessibilityPostNotification(UIAccessibilityLayoutChangedNotification, firstElement);

```



## Accessibility Container


VoiceOver can navigate many apps on iOS because most `UIKit` classes implement `UIAccessibilityProtocol`. Features that don’t represent onscreen elements using `UIView`, including apps that leverage Core Graphics or Metal to perform drawing, must describe these elements for accessibility. As of iOS 8.0, this can be done by assigning a property on the `UIView` containing inaccessible elements:

```swift
myInaccessibleContainerView.accessibilityElements = @[elements, that, should, be, accessible];

```

Each object in the array can be an instance of `UIAccessibilityElement` or any other class that adheres to `UIAccessibilityProtocol`. The child elements should be returned in the order the user should navigate them. As an application author, you can use accessibility containers to override the default top-left to bottom-right ordering of VoiceOver swipe navigation. Given that `UIView` implements `UIAccessibilityProtocol`, you can combine instances of `UIAccessibilityElement` and `UIView` in the same array of child accessibility elements. Note that if you assign elements manually, you do not need to implement any dynamic accessibility protocol methods, though you may need to issue a screen change notification for the elements to be detected by VoiceOver.



## Hiding Elements


Most UIKit classes, including UIView, adhere to `UIAccessibilityProtocol` and return correct values by default. It’s easy to take for granted that a `UIView` set to hidden is also absent from the accessibility hierarchy and won’t be navigated by VoiceOver. While this default behavior is usually sufficient, there are times where a view will be present in the view hierarchy but not visible or navigable. For example, a collection of buttons may be overlapped by another view, rendering them invisible to a sighted user. VoiceOver, however, will still try to navigate them since they are technically not hidden from `UIKit` and therefore are still present in the accessibility hierarchy. In such cases, you must hint to VoiceOver that the parent view isn’t accessible. You can do this by explicitly hiding the view from UIKit by setting hidden when the view goes offscreen:

```swift
myViewFullofButtons.hidden = YES;

```

Alternatively, you can leave the parent view visible and simply hide its children from the accessibility hierarchy:

```swift
myViewFullofButtons.accessibilityElementsHidden = YES;

```

Temporary views are a another place you’ll want to hide elements from the accessibility hierarchy while leaving them visible to users. For example, the view that pops up when you hit the volume button is visible to sighted users but doesn’t demand attention the way a normal alert does. You wouldn’t want VoiceOver to interrupt the user and move the cursor from away from whatever they were doing to announce the new volume, especially given that adjusting volume already provides auditory feedback through the clicking sound it makes. In cases like this, you’ll want to hide the view using `accessibilityElementsHidden`.



## Screen Change


VoiceOver works great most of the time, breezily reading aloud screens full of content and intuitively following the user. Alas, no general solution is perfect. Sometimes only you, the app developer, know where VoiceOver should be focused for an optimal user experience. Fortunately, VoiceOver listens to system accessibility notifications for clues about where focus belongs. To move the VoiceOver cursor manually, post an accessibility screen changed notification:

```swift
UIAccessibilityPostNotification(UIAccessibilityScreenChangedNotification, firstElement);

```

When this notification is posted, a short series of tones notify users of the change. The second parameter can be either the next element to focus or a string announcing the change. Only post a screen change notification if the VoiceOver experience is poor without it and no other workaround exists. Moving the VoiceOver cursor is like poking at a sighted user’s screen. It can be annoying and disorienting to be led around that way.



## Announcement


Announcements are useful for alerting users to events that don’t require any interaction, such as “screen locked” or “finished loading.” Use a more specific announcement to notify users of screen changes or more minor layout changes.

```swift
UIAccessibilityPostNotification(UIAccessibilityAnnouncementNotification, @"The thing happened!");

```



## Ordering Elements


VoiceOver navigates from top-left to bottom-right, irrespective of the view hierarchy. This is usually how content is arranged in left-to-right languages since sighted individuals tend to scan the screen in an “F-shaped pattern”. VoiceOver users will expect to navigate the same way as typical users. Predictability and consistency are very important to accessibility. Please refrain from making customizations that “improve” on default behavior (eg. ordering the tab bar first in the swipe order). That said, if you have received feedback that the order of elements in your app is surprising, there are a couple of ways you can improve the experience.

If VoiceOver should read a view’s subviews one after the next but is not, you may need to hint to VoiceOver that the elements contained within a single view are related. You can do this by setting `shouldGroupAccessibiltyChildren`:

```swift
myView.shouldGroupAccessibilityChildren = YES;

```

To support complex navigation structures that span multiple containers or include interfaces rendered without UIKit, consider implementing the container protocol on the parent view.



## Modal View


Modal views completely capture the user’s attention until a task is complete. iOS clarifies this to users by dimming and disabling all other content when a modal view, such as an alert or popover, is visible. An app that implements a custom modal interface needs to hint to VoiceOver that this view deserve the user’s undivided attention by setting `accessibilityViewIsModal`. Note that this property should only be set on the view containing modal content, not elements contained within a modal view.

```swift
myModalView.accessibilityViewIsModal = YES;

```

Tagging a view as modal encourages VoiceOver to ignore sibling views. If, after setting this property, you find that VoiceOver still navigates other elements in your app, try hiding problem views until the modal dismisses.

