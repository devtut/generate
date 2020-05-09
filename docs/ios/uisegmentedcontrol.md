---
metaTitle: "iOS - UISegmentedControl"
description: "Creating UISegmentedControl via code"
---

# UISegmentedControl


A UISegmentedControl object is a horizontal control made of multiple segments, each segment functioning as a discrete button. A segmented control affords a compact means to group together a number of controls.



## Creating UISegmentedControl via code


1. Create new instance of UISegmentedControl filled with 3 items (segments):

```swift
let mySegmentedControl = UISegmentedControl (items: ["One", "Two", "Three"])

```


1. Setup frame;

```swift
mySegmentedControl.frame = CGRect(x: 0.0, y: 0.0, width: 300, height: 50)

```


1. Make default selection (not that segments are indexed by 0):

```swift
mySegmentedControl.selectedSegmentIndex = 0

```


1. Configure target:

```swift
mySegmentedControl.addTarget(self, action: #selector(segmentedValueChanged(_:)), for: .valueChanged)

```

5 Handle value changed:

```swift
func segmentedValueChanged(_ sender:UISegmentedControl!) {
    print("Selected Segment Index is : \(sender.selectedSegmentIndex)")
}

```


1. Add UISegmentedControl to views hierarchy

```swift
yourView.addSubview(mySegmentedControl)

```

