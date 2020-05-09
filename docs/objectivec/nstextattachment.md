---
metaTitle: "Objective-C - NSTextAttachment"
description: "NSTextAttachment Example"
---

# NSTextAttachment



## NSTextAttachment Example


```objc
NSTextAttachment *attachment = [[NSTextAttachment alloc] init];
attachment.image = [UIImage imageNamed:@"imageName"];
attachment.bounds = CGRectMake(0, 0, 35, 35);
NSAttributedString *attachmentString = [NSAttributedString attributedStringWithAttachment:attachment];

```



#### Syntax


1. NSTextAttachment *attachmentName = [[NSTextAttachment alloc] init];



#### Remarks


`NSTextAttachment` objects are used by the `NSAttributedString` class cluster as the values for attachment attributes. The objects you create with this class are referred to as text attachment objects, or when no confusion will result, as text attachments or merely attachments.

