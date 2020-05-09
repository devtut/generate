---
metaTitle: "iOS - Custom UITextField"
description: "Custom UITextField for Filtering Input Text, Custom UITextField to Disallow All Actions like Copy, Paste, etc"
---

# Custom UITextField


Using custom UITextField, we can manipulate the behavior of text field!



## Custom UITextField for Filtering Input Text


Here is an example of custom `UITextField` that takes only numerical text and discards all other.

**NOTE:** For iPhone it is easy to do this using Number type keyboard, but for iPad there is no keyboard with Numbers only

```swift
class NumberTextField: UITextField {
    
required init(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)
    registerForTextFieldNotifications()
}

override init(frame: CGRect) {
    super.init(frame: frame)
}

override func awakeFromNib() {
    super.awakeFromNib()
    keyboardType = .numberPad//useful for iPhone only
}

private func registerForTextFieldNotifications() {
    NotificationCenter.default.addObserver(self, selector: #selector(NumberTextField.textDidChange), name: NSNotification.Name(rawValue: "UITextFieldTextDidChangeNotification"), object: self)
}

deinit {
    NotificationCenter.default.removeObserver(self)
}

func textDidChange() {
    text = filteredText()
}
private func filteredText() -> String {
    let inverseSet = CharacterSet(charactersIn:"0123456789").inverted
    let components = text!.components(separatedBy: inverseSet)
    return components.joined(separator: "")
}
}

```

So, wherever we want text field which would take only numbers as input text, then we can use this custom UITextField



## Custom UITextField to Disallow All Actions like Copy, Paste, etc


If we want to disable all the actions like Copy, Paste, Replace, Select, etc from `UITextField` then we can use following custom text field:

```swift
class CustomTextField: UITextField {

var enableLongPressActions = false

required init(coder aDecoder: NSCoder) {
    super.init(coder: aDecoder)!
}

override init(frame: CGRect) {
    super.init(frame: frame)
}

override func canPerformAction(_ action: Selector, withSender sender: Any?) -> Bool {
    return enableLongPressActions
}
}

```

Using `enableLongPressActions` property, we can enable all actions any time later, if needed.

