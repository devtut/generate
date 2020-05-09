---
metaTitle: "iOS - NSAttributedString"
description: "Creating a string that has custom kerning (letter spacing), Change the color of a word or string, Create a string with strikethrough text, Appending Attributed Strings and bold text in Swift, Removing all attributes"
---

# NSAttributedString




## Creating a string that has custom kerning (letter spacing)


`NSAttributedString` (and its mutable sibling `NSMutableAttributedString`)  allows you to create strings that are complex in their appearance to the user.

A common application is to use this to display a string and adding custom kerning / letter-spacing.

This would be achieved as follows (where label is a `UILabel`), giving a different kerning for the word "kerning"

**Swift**

```swift
var attributedString = NSMutableAttributedString("Apply kerning")
attributedString.addAttribute(attribute: NSKernAttributeName, value: 5, range: NSMakeRange(6, 7))
label.attributedText = attributedString

```

**Objective-C**

```swift
NSMutableAttributedString *attributedString;
attributedString = [[NSMutableAttributedString alloc] initWithString:@"Apply kerning"];
[attributedString addAttribute:NSKernAttributeName value:@5 range:NSMakeRange(6, 7)];
[label setAttributedText:attributedString];

```



## Change the color of a word or string


**Objective-C**

```swift
UIColor *color = [UIColor redColor];
NSString *textToFind = @"redword";

NSMutableAttributedString *attrsString =  [[NSMutableAttributedString alloc] initWithAttributedString:yourLabel.attributedText];

// search for word occurrence
NSRange range = [yourLabel.text rangeOfString:textToFind];
if (range.location != NSNotFound) {
    [attrsString addAttribute:NSForegroundColorAttributeName value:color range:range];
}

// set attributed text
yourLabel.attributedText = attrsString;

```

**Swift**

```swift
let color = UIColor.red;
let textToFind = "redword"
        
let attrsString =  NSMutableAttributedString(string:yourlabel.text!);
        
// search for word occurrence
let range = (yourlabel.text! as NSString).range(of: textToFind)
if (range.length > 0) {
     attrsString.addAttribute(NSForegroundColorAttributeName,value:color,range:range)
}
        
// set attributed text
yourlabel.attributedText = attrsString

```

**Note**:

The main here is to use a `NSMutableAttributedString` and the selector `addAttribute:value:range` with the attribute `NSForegroundColorAttributeName`  to change a color of a string range:

```swift
NSMutableAttributedString *attrsString =  [[NSMutableAttributedString alloc] initWithAttributedString:label.attributedText];
[attrsString addAttribute:NSForegroundColorAttributeName value:color range:range];

```

You could use another way to get the range, for example: NSRegularExpression.



## Create a string with strikethrough text


**Objective-C**

```swift
NSMutableAttributedString *attributeString = [[NSMutableAttributedString alloc] initWithString:@"Your String here"];
[attributeString addAttribute:NSStrikethroughStyleAttributeName
                    value:@2
                    range:NSMakeRange(0, [attributeString length])];

```

**Swift**

```swift
let attributeString: NSMutableAttributedString =  NSMutableAttributedString(string: "Your String here")
attributeString.addAttribute(NSStrikethroughStyleAttributeName, value: 2, range: NSMakeRange(0, attributeString.length))

```

Then you can add this to your UILabel:

```swift
yourLabel.attributedText = attributeString;

```



## Appending Attributed Strings and bold text in Swift


```swift
let someValue : String = "Something the user entered"     
let text = NSMutableAttributedString(string: "The value is: ")
text.appendAttributedString(NSAttributedString(string: someValue, attributes: [NSFontAttributeName:UIFont.boldSystemFontOfSize(UIFont.systemFontSize())]))

```

The result looks like:

The value is: **Something the user entered**



## Removing all attributes


**Objective-C**

```swift
NSMutableAttributedString *mutAttString = @"string goes here";
NSRange range = NSMakeRange(0, mutAttString.length);
[mutAttString setAttributes:@{} range:originalRange];

```

As per Apple Documentation we use, `setAttributes` and not `addAttribute`.

**Swift**

```swift
mutAttString.setAttributes([:], range: NSRange(0..<string.length))

```



#### Remarks


[Set Color of Font Using NSAttributedString](http://stackoverflow.com/questions/27728466/use-multiple-font-colors-in-a-single-label-swift/27728516#27728516)

