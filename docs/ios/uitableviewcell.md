---
metaTitle: "iOS - UITableViewCell"
description: "Xib file of UITableViewCell"
---

# UITableViewCell


Load custom cell xib file uses the cell category class, no need to register the nib file



## Xib file of UITableViewCell


Create a `UITableView` cell category class.

**UITableViewCell+RRCell.h file**

```swift
#import <UIKit/UIKit.h>

@interface UITableViewCell (RRCell)

-(id)initWithOwner:(id)owner;

@end

```

**UITableViewCell+RRCell.m file**

```swift
#import "UITableViewCell+RRCell.h"

@implementation UITableViewCell (RRCell)

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-designated-initializers"

-(id)initWithOwner:(id)owner {
    
    if (self = [super init]) {
        
        NSArray *nib = [[NSBundle mainBundle]loadNibNamed:NSStringFromClass([self class]) owner:self options:nil];
        self = [nib objectAtIndex:0];
    }
    return self;
}

#pragma clang diagnostic pop



@end

```

Import cell category class to use this method into the `cellForRowAtIndexPath` method

```swift
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    //Creted custom cell xib file to load by cell category class
    CustomCell *cell = [[CustomCell alloc]initWithOwner:self];
    
    return cell;     
}

```

