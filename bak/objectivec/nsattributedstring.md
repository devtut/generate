---
metaTitle: "Objective C - NSAttributedString"
description: "Creating a string that has custom kerning (letter spacing) editshare,  Create a string with text struck through, Using Enumerating over Attributes in a String and underline part of string, How you create a tri-color attributed string."
---

# NSAttributedString



## Creating a string that has custom kerning (letter spacing) editshare


`NSAttributedString` (and its mutable sibling `NSMutableAttributedString`)  allows you to create strings that are complex in their appearance to the user.

A common application is to use this to display a string and adding custom kerning / letter-spacing.

This would be achieved as follows (where label is a `UILabel`), giving a different kerning for the word "kerning"

```objectivec
NSMutableAttributedString *attributedString;
attributedString = [[NSMutableAttributedString alloc] initWithString:@"Apply kerning"];
[attributedString addAttribute:NSKernAttributeName value:@5 range:NSMakeRange(6, 7)];
[label setAttributedText:attributedString];

```



##  Create a string with text struck through


```objectivec
NSMutableAttributedString *attributeString = [[NSMutableAttributedString alloc] initWithString:@"Your String here"];
[attributeString addAttribute:NSStrikethroughStyleAttributeName
                    value:@2
                    range:NSMakeRange(0, [attributeString length])];

```



## Using Enumerating over Attributes in a String and underline part of string


```

NSMutableDictionary *attributesDictionary = [NSMutableDictionary dictionary];
 [attributesDictionary setObject:[UIFont systemFontOfSize:14] forKey:NSFontAttributeName];
 //[attributesDictionary setObject:[UIColor redColor] forKey:NSForegroundColorAttributeName];
 NSMutableAttributedString *attributedString = [[NSMutableAttributedString alloc]initWithString:@"Google www.google.com link" attributes:attributesDictionary];

 [attributedString enumerateAttribute:(NSString *) NSFontAttributeName
                             inRange:NSMakeRange(0, [attributedString length])
                             options:NSAttributedStringEnumerationLongestEffectiveRangeNotRequired
                          usingBlock:^(id value, NSRange range, BOOL *stop) {
                              NSLog(@"Attribute: %@, %@", value, NSStringFromRange(range));
                             }];

  NSMutableAttributedString *attributedStr = [[NSMutableAttributedString alloc] initWithString:@"www.google.com "];

  [attributedString addAttribute:NSUnderlineStyleAttributeName
                         value:[NSNumber numberWithInt:NSUnderlineStyleDouble]
                         range:NSMakeRange(7, attributedStr.length)];

  [attributedString addAttribute:NSForegroundColorAttributeName
                         value:[UIColor blueColor]
                         range:NSMakeRange(6,attributedStr.length)];

  _attriLbl.attributedText = attributedString;//_attriLbl (of type UILabel) added in storyboard

```

**Output:**

[<img src="http://i.stack.imgur.com/nqxsQ.png" alt="enter image description here" />](http://i.stack.imgur.com/nqxsQ.png)



## How you create a tri-color attributed string.


```

NSMutableAttributedString * string = [[NSMutableAttributedString alloc] initWithString:@"firstsecondthird"];
[string addAttribute:NSForegroundColorAttributeName value:[UIColor redColor] range:NSMakeRange(0,5)];
[string addAttribute:NSForegroundColorAttributeName value:[UIColor greenColor] range:NSMakeRange(5,6)];
[string addAttribute:NSForegroundColorAttributeName value:[UIColor blueColor] range:NSMakeRange(11,5)];

```

Range : start to end string

Here we have firstsecondthird string so in first we have set range (0,5) so from starting first character to fifth character it will display in green text color.

