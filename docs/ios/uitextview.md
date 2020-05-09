---
metaTitle: "iOS - UITextView"
description: "Set attributed text, Change font, Auto Detect Links, Addresses, Dates, and more, Change text, Change text alignment, UITextViewDelegate methods, Change text color, UITextView with HTML text, Check to see if empty or nil, Getting and Setting the Cursor Postition, Remove extra paddings to fit to a precisely measured text."
---

# UITextView




## Set attributed text


```swift
// Modify some of the attributes of the attributed string.
let attributedText = NSMutableAttributedString(attributedString: textView.attributedText!)

// Use NSString so the result of rangeOfString is an NSRange.
let text = textView.text! as NSString

// Find the range of each element to modify.
let tintedRange = text.range(of: NSLocalizedString("tinted", comment: ""))
let highlightedRange = text.range(of: NSLocalizedString("highlighted", comment: ""))

// Add tint.
attributedText.addAttribute(NSForegroundColorAttributeName, value: UIColor.blue, range: tintedRange)

// Add highlight.
attributedText.addAttribute(NSBackgroundColorAttributeName, value: UIColor.yellow, range: highlightedRange)

textView.attributedText = attributedText

```



## Change font


**Swift**

```swift
//System Font
textView.font = UIFont.systemFont(ofSize: 12)

//Font of your choosing
textView.font = UIFont(name: "Font Name", size: 12)

```

**Objective-C**

```swift
//System Font
textView.font = [UIFont systemFontOfSize:12];

//Font of your choosing
textView.font = [UIFont fontWithName:@"Font Name" size:12];

```



## Auto Detect Links, Addresses, Dates, and more


`UITextView` has built in support to auto detect a variety of data.
The data that is able to be auto-detected currently includes:

```swift
enum {
   UIDataDetectorTypePhoneNumber   = 1 << 0,
   UIDataDetectorTypeLink          = 1 << 1,
   UIDataDetectorTypeAddress       = 1 << 2,
   UIDataDetectorTypeCalendarEvent = 1 << 3,
   UIDataDetectorTypeNone          = 0,
   UIDataDetectorTypeAll           = NSUIntegerMax
};

```

### Enabling auto-detection

```swift
// you may add as many as you like by using the `|` operator between options
textView.dataDetectorTypes = (UIDataDetectorTypeLink | UIDataDetectorTypePhoneNumber);

```

If enabled, the text will appear as a hyperlink on the `UITextView`

### Clickable data

To allow the link to be clicked (which will result in different actions depending on the data type) you must ensure that the `UITextView` is selectable but not editable and that user interaction is enabled

```swift
textView.editable = NO;
textView.selectable = YES;
textView.userInteractionEnabled = YES; // YES by default

```



## Change text


**Swift**

```swift
textView.text = "Hello, world!"

```

**Objective-C:**

```swift
textView.text = @"Hello, world!";

```



## Change text alignment


**Swift**

```swift
textView.textAlignment = .left

```

**Objective-C**

```swift
textView.textAlignment = NSTextAlignmentLeft;

```



## UITextViewDelegate methods


**Responding to Editing Notifications**

- `textViewShouldBeginEditing(_:)`
- `textViewDidBeginEditing(_:)`
- `textViewShouldEndEditing(_:)`
- `textViewDidEndEditing(_:)`

**Responding to Text Changes**

- `textView(_:shouldChangeTextIn:replacementText:)`
- `textViewDidChange(_:)`

**Responding to URL**

- `textView(_: UITextView, shouldInteractWithURL: NSURL, inRange: NSRange) -> Bool`



## Change text color


**Swift**

```swift
textView.textColor = UIColor.red

```

**Objective-C**

```swift
textView.textColor = [UIColor redColor];

```



## UITextView with HTML text


```swift
NSString *htmlString = @"<p> This is an <b>HTML</b> text</p>";
NSAttributedString *attributedString = [[NSMutableAttributedString alloc]
                                                        initWithData: [htmlString dataUsingEncoding:NSUnicodeStringEncoding]
                                                        options: @{ NSDocumentTypeDocumentAttribute: NSHTMLTextDocumentType }
                                                        documentAttributes: nil
                                                        error: nil
                                                        ];
                _yourTextView.attributedText = attributedString;
                // If you want to modify the font
                field.font = [UIFont fontWithName:@"Raleway-Regular" size:15];

```



## Check to see if empty or nil


**Swift**

```swift
if let text = self.textView.text where !text.isEmpty {
    // Do stuff for text
} else {
    // Do stuff for nil text or empty string
}

```

**Objective-C**

```swift
if (self.textView.text.length > 0){
    // Do stuff for text
}   else {
    // Do stuff for nil text or empty string
}

```



## Getting and Setting the Cursor Postition


### Useful information

The very beginning of the text field text:

```swift
let startPosition: UITextPosition = textView.beginningOfDocument

```

The very end of the text field text:

```swift
let endPosition: UITextPosition = textView.endOfDocument

```

The currently selected range:

```swift
let selectedRange: UITextRange? = textView.selectedTextRange

```

### Get cursor position

```swift
if let selectedRange = textView.selectedTextRange {
    
    let cursorPosition = textView.offsetFromPosition(textView.beginningOfDocument, toPosition: selectedRange.start)
    
    print("\(cursorPosition)")
}

```

### Set cursor position

In order to set the position, all of these methods are actually setting a range with the same start and end values.

**To the beginning**

```swift
let newPosition = textView.beginningOfDocument
textView.selectedTextRange = textView.textRangeFromPosition(newPosition, toPosition: newPosition)

```

**To the end**

```swift
let newPosition = textView.endOfDocument
textView.selectedTextRange = textView.textRangeFromPosition(newPosition, toPosition: newPosition)

```

**To one position to the left of the current cursor position**

```swift
// only if there is a currently selected range
if let selectedRange = textView.selectedTextRange {
    
    // and only if the new position is valid
    if let newPosition = textView.positionFromPosition(selectedRange.start, inDirection: UITextLayoutDirection.Left, offset: 1) {
        
        // set the new position
        textView.selectedTextRange = textView.textRangeFromPosition(newPosition, toPosition: newPosition)
    }
}

```

**To an arbitrary position**

Start at the beginning and move 5 characters to the right.

```swift
let arbitraryValue: Int = 5
if let newPosition = textView.positionFromPosition(textView.beginningOfDocument, inDirection: UITextLayoutDirection.Right, offset: arbitraryValue) {
    
    textView.selectedTextRange = textView.textRangeFromPosition(newPosition, toPosition: newPosition)
}

```

### Related

**Select all text**

```swift
textView.selectedTextRange = textView.textRangeFromPosition(textView.beginningOfDocument, toPosition: textView.endOfDocument)

```

**Select a range of text**

```swift
// Range: 3 to 7
let startPosition = textView.positionFromPosition(textView.beginningOfDocument, inDirection: UITextLayoutDirection.Right, offset: 3)
let endPosition = textView.positionFromPosition(textView.beginningOfDocument, inDirection: UITextLayoutDirection.Right, offset: 7)

if startPosition != nil && endPosition != nil {
    textView.selectedTextRange = textView.textRangeFromPosition(startPosition!, toPosition: endPosition!)
}

```

**Insert text at the current cursor position**

```swift
textView.insertText("Hello")

```

### Notes

<li>
This example originally comes from an adaptation of [this Stack Overflow answer](http://stackoverflow.com/a/34922332/3681880).
</li>
<li>
This answer uses a text field, but the same concepts apply to `UITextView`.
</li>
<li>
Use `textView.becomeFirstResponder()` to give focus to the text field and make the keyboard appear.
</li>
<li>
See [this answer](http://stackoverflow.com/a/34940034/3681880) for how to get the text at some range.
</li>

### Related

- [How to Create a Range in Swift](http://stackoverflow.com/a/35193481/3681880) (Deals indirectly with the issue of why we have to use `selectedTextRange` here rather than just `selectedRange`)



## Remove extra paddings to fit to a precisely measured text.


`UITextView` has extra paddings by default. Sometimes it's annoying especially if you want to measure some text without view instance and place them at some area precisely.

Do this to remove such paddings.

```swift
messageTextView.textContainerInset = UIEdgeInsetsZero
messageTextView.textContainer.lineFragmentPadding = 0

```

Now you can measure text size using `NSAttributedString.boundingRectWithSize(...)`, and resize a `UITextView` just to fit it to the text.

```swift
let budget = getSomeCGSizeBudget()
let text = getSomeAttributedString()
let textSize = text.boundingRectWithSize(budget, options: [.UsesLineFragmentOrigin, .UsesFontLeading], context: nil).size
messageTextView.frame.size = textSize // Just fits.

```

