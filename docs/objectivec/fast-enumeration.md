---
metaTitle: "Objective-C - Fast Enumeration"
description: "Fast enumeration of an NSArray, Fast enumeration of an NSArray with index."
---

# Fast Enumeration



## Fast enumeration of an NSArray


This example shows how to use fast enumeration in order to traverse through an NSArray.

When you have an array, such as

```objc
NSArray *collection = @[@"fast", @"enumeration", @"in objc"];

```

You can use the `for ... in` syntax to go through each item of the array, automatically starting with the first at index `0` and stopping with the last item:

```objc
for (NSString *item in collection) {
    NSLog(@"item: %@", item);
}

```

In this example, the output generated would look like

```objc
// item: fast
// item: enumeration
// item: in objc

```



## Fast enumeration of an NSArray with index.


This example shows how to use fast enumeration in order to traverse through an NSArray. With this way you can also track current object's index while traversing.

Suppose you have an array,

```objc
NSArray *weekDays = @[@"Monday", @"Tuesday", @"Wednesday", @"Thursday", @"Friday", @"Saturday", @"Sunday"];

```

Now you can traverse through the array like below,

```objc
[weekDays enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {

    //... Do your usual stuff here

    obj  // This is the current object
    idx  // This is the index of the current object
    stop // Set this to true if you want to stop

}];

```

