---
metaTitle: "Objective-C - NSMutableArray"
description: "Sorting Arrays, Creating an NSMutableArray, Adding elements, Insert Elements, Deleting Elements, Move object to another index, Filtering Array content with Predicate"
---

# NSMutableArray




## Sorting Arrays


```objc
NSMutableArray *myColors = [NSMutableArray arrayWithObjects: @"red", @"green", @"blue", @"yellow", nil];
NSArray *sortedArray;
sortedArray = [myColors sortedArrayUsingSelector:@selector(localizedCaseInsensitiveCompare:)];

```



## Creating an NSMutableArray


`NSMutableArray` can be initialized as an empty array like this:

```objc
NSMutableArray *array = [[NSMutableArray alloc] init];
// or
NSMutableArray *array2 = @[].mutableCopy;
// or
NSMutableArray *array3 = [NSMutableArray array];

```

`NSMutableArray` can be initialized with another array like this:

```objc
NSMutableArray *array4 = [[NSMutableArray alloc] initWithArray:anotherArray];
// or
NSMutableArray *array5 = anotherArray.mutableCopy; 

```



## Adding elements


```objc
NSMutableArray *myColors;
myColors = [NSMutableArray arrayWithObjects: @"Red", @"Green", @"Blue", @"Yellow", nil];
[myColors addObject: @"Indigo"];
[myColors addObject: @"Violet"];

//Add objects from an NSArray
NSArray *myArray = @[@"Purple",@"Orange"];
[myColors addObjectsFromArray:myArray];

```



## Insert Elements


```objc
NSMutableArray *myColors;
int i;
int count;
myColors = [NSMutableArray arrayWithObjects: @"Red", @"Green", @"Blue", @"Yellow", nil];
[myColors insertObject: @"Indigo" atIndex: 1];
[myColors insertObject: @"Violet" atIndex: 3];

```



## Deleting Elements


Remove at specific index:

```objc
[myColors removeObjectAtIndex: 3];

```

Remove the first instance of a specific object:

```objc
[myColors removeObject: @"Red"];

```

Remove all instances of a specific object:

```objc
[myColors removeObjectIdenticalTo: @"Red"];

```

Remove all objects:

```objc
[myColors removeAllObjects];

```

Remove last object:

```objc
[myColors removeLastObject];

```



## Move object to another index


**Move **Blue** to the beginning of the array:**

```objc
NSMutableArray *myColors = [NSMutableArray arrayWithObjects: @"Red", @"Green", @"Blue", @"Yellow", nil];

NSUInteger fromIndex = 2;
NSUInteger toIndex = 0;

id blue = [[[self.array objectAtIndex:fromIndex] retain] autorelease];
[self.array removeObjectAtIndex:fromIndex];
[self.array insertObject:blue atIndex:toIndex];

```

`myColors` is now `[@"Blue", @"Red", @"Green", @"Yellow"]`.



## Filtering Array content with Predicate


Using **filterUsingPredicate:** This Evaluates a given predicate against the arrays content and return objects that match.

Example:

```

     NSMutableArray *array = [NSMutableArray array];
      [array setArray:@[@"iOS",@"macOS",@"tvOS"]];
      NSPredicate *predicate = [NSPredicate predicateWithFormat:@"SELF beginswith[c] 'i'"];
      NSArray *resultArray = [array filteredArrayUsingPredicate:predicate];
      NSLog(@"%@",resultArray);

```

