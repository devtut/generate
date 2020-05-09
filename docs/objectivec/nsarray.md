---
metaTitle: "Objective C - NSArray"
description: "Creating NSArray instances, Sorting Arrays, Filter  NSArray and NSMutableArray"
---

# NSArray



## Creating NSArray instances


```objectivec
NSArray *array1 = [NSArray arrayWithObjects:@"one", @"two", @"three", nil];
NSArray *array2 = @[@"one", @"two", @"three"];

```



## Sorting Arrays


The most flexible ways to sort an array is with the sortedArrayUsingComparator: method. This accepts an **^NSComparisonResult(id obj1, id obj2) block**.

```

Return Value            Description
 NSOrderedAscending      obj1 comes before obj2
 NSOrderedSame           obj1 and obj2 have no order
 NSOrderedDescending     obj1 comes after obj2

```

Example:

```

  NSArray *categoryArray = @[@"Apps", @"Music", @"Songs",
                     @"iTunes", @"Books", @"Videos"];
   
    NSArray *sortedArray = [categoryArray sortedArrayUsingComparator:
^NSComparisonResult(id obj1, id obj2) {
    if ([obj1 length] < [obj2 length]) {
        return NSOrderedAscending;
    } else if ([obj1 length] > [obj2 length]) {
        return NSOrderedDescending;
    } else {
        return NSOrderedSame;
    }
  }];

 NSLog(@"%@", sortedArray);

```



## Filter  NSArray and NSMutableArray


```objectivec
NSMutableArray *array =
    [NSMutableArray arrayWithObjects:@"Ken", @"Tim", @"Chris", @"Steve",@"Charlie",@"Melissa", nil];

NSPredicate *bPredicate =
    [NSPredicate predicateWithFormat:@"SELF beginswith[c] 'c'"];
NSArray *beginWithB =
    [array filteredArrayUsingPredicate:bPredicate];
// beginWith "C" contains { @"Chris", @"Charlie" }.

NSPredicate *sPredicate =
    [NSPredicate predicateWithFormat:@"SELF contains[c] 'a'"];
[array filterUsingPredicate:sPredicate];
// array now contains { @"Charlie", @"Melissa" }

```

