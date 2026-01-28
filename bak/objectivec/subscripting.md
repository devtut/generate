---
metaTitle: "Objective C - Subscripting"
description: "Subscripts with NSArray, Custom Subscripting, Subscripts with NSDictionary"
---

# Subscripting



## Subscripts with NSArray


Subscripts can be used to simplify retrieving and setting elements in an array. Given the following array

```objectivec
NSArray *fruit = @[@"Apples", @"Bananas", @"Cherries"];

```

This line

```objectivec
[fruit objectAtIndex: 1];

```

Can be replaced by

```objectivec
fruit[1];

```

They can also be used to set an element in a mutable array.

```objectivec
NSMutableArray *fruit = [@[@"Apples", @"Bananas", @"Cherries"] mutableCopy];
fruit[1] = @"Blueberries";
NSLog(@"%@", fruit[1]); //Blueberries

```

If the index of the subscript equals the count of the array, the element will be appended to the array.

Repeated subscripts may be used to access elements of nested arrays.

```objectivec
NSArray *fruit = @[@"Apples", @"Bananas", @"Cherries"];
NSArray *vegetables = @[@"Avocado", @"Beans", @"Carrots"];
NSArray *produce = @[fruit, vegetables];
    
NSLog(@"%@", produce[0][1]); //Bananas

```



## Custom Subscripting


You can add subscripting to your own classes by implementing the required methods.

For indexed subscripting (like arrays):

```objectivec
- (id)objectAtIndexedSubscript:(NSUInteger)idx
- (void)setObject:(id)obj atIndexedSubscript:(NSUInteger)idx

```

For keyed subscripting (like dictionaries):

```objectivec
- (id)objectForKeyedSubscript:(id)key
- (void)setObject:(id)obj forKeyedSubscript:(id <NSCopying>)key

```



## Subscripts with NSDictionary


Subscripts can also be used with NSDictionary and NSMutableDictionary. The following code:

```objectivec
NSMutableDictionary *myDictionary = [@{@"Foo": @"Bar"} mutableCopy];
[myDictionary setObject:@"Baz" forKey:@"Foo"];
NSLog(@"%@", [myDictionary objectForKey:@"Foo"]); // Baz

```

Can be shortened to:

```objectivec
NSMutableDictionary *myDictionary = [@{@"Foo": @"Bar"} mutableCopy];
myDictionary[@"Foo"] = @"Baz";
NSLog(@"%@", myDictionary[@"Foo"]); // Baz

```

