---
metaTitle: "iOS - Dynamically updating a UIStackView"
description: "Connect the UISwitch to an action we can animate switching between a horizontal or vertical layout of the image views"
---

# Dynamically updating a UIStackView




## Connect the UISwitch to an action we can animate switching between a horizontal or vertical layout of the image views


```swift
@IBAction func axisChange(sender: UISwitch) {
    UIView.animateWithDuration(1.0) {
        self.updateConstraintsForAxis()
    }
}

```

The updateConstraintForAxis function just sets the axis of the stack view containing the two image views:

```swift
private func updateConstraintsForAxis() {
    if (axisSwitch.on) {
        stackView.axis = .Horizontal
    } else {
        stackView.axis = .Vertical
    }
}

```

The animated gif below gives you an idea of how this appears:

[<img src="http://i.stack.imgur.com/n0ZX0.gif" alt="enter image description here" />](http://i.stack.imgur.com/n0ZX0.gif)

