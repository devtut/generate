---
metaTitle: "Objective-C - Key Value Coding / Key Value Observing"
description: "Most Common Real Life Key Value Coding Example, Key Value Observing, Querying KVC Data, Collection Operators"
---

# Key Value Coding / Key Value Observing



## Most Common Real Life Key Value Coding Example


Key Value Coding is integrated into **NSObject** using **NSKeyValueCoding** protocol.

**What this means?**

It means that any id object is capable of calling valueForKey method and its various variants like valueForKeyPath etc. '

It also means that any id object can invoke setValue method and its various variants too.

**Example:**

```objc
id obj = [[MyClass alloc] init];
id value = [obj valueForKey:@"myNumber"];

int myNumberAsInt = [value intValue];
myNumberAsInt = 53;
[obj setValue:@(myNumberAsInt) forKey:@"myNumber"];

```

**Exceptions:**

Above example assumes that MyClass has an NSNumber Property called myNumber.
If myNumber does not appear in MyClass interface definition, an NSUndefinedKeyException can be raised at possibly both lines 2 and 5 - popularly known as:

```objc
this class is not key value coding-compliant for the key myNumber.

```

**Why this is SO powerful:**

You can write code that can access properties of a class dynamically, without needing interface for that class. This means that a table view can display values from any properties of an NSObject derived object, provided its property names are supplied dynamically at runtime.

In the example above, the code can as well work without MyClass being available and id type obj being available to calling code.



## Key Value Observing


Setting up key value observing.

In this case, we want to observe the `contentOffset` on an object that our observer owns

```objc
//
// Class to observe
//
@interface XYZScrollView: NSObject
@property (nonatomic, assign) CGPoint contentOffset;
@end

@implementation XYZScrollView
@end


//
// Class that will observe changes
//
@interface XYZObserver: NSObject
@property (nonatomic, strong) XYZScrollView *scrollView;
@end

@implementation XYZObserver

// simple way to create a KVO context
static void *XYZObserverContext = &XYZObserverContext;


// Helper method to add self as an observer to 
// the scrollView's contentOffset property
- (void)addObserver {

    // NSKeyValueObservingOptions
    //
    // - NSKeyValueObservingOptionNew
    // - NSKeyValueObservingOptionOld
    // - NSKeyValueObservingOptionInitial
    // - NSKeyValueObservingOptionPrior
    //
    // can be combined:
    // (NSKeyValueObservingOptionNew | NSKeyValueObservingOptionOld)
    
    NSString *keyPath = NSStringFromSelector(@selector(contentOffset));
    NSKeyValueObservingOptions options = NSKeyValueObservingOptionNew;    

    [self.scrollView addObserver: self 
                      forKeyPath: keyPath 
                         options: options
                         context: XYZObserverContext];
}

- (void)observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary<NSString *,id> *)change context:(void *)context {
    
    if (context == XYZObserverContext) { // check the context

        // check the keyPath to see if it's any of the desired keyPath's.
        // You can observe multiple keyPath's
        if ([keyPath isEqualToString: NSStringFromSelector(@selector(contentOffset))]) {

            // change dictionary keys:
            // - NSKeyValueChangeKindKey
            // - NSKeyValueChangeNewKey
            // - NSKeyValueChangeOldKey
            // - NSKeyValueChangeIndexesKey
            // - NSKeyValueChangeNotificationIsPriorKey
            
            // the change dictionary here for a CGPoint observation will
            // return an NSPoint, so we can take the CGPointValue of it.
            CGPoint point = [change[NSKeyValueChangeNewKey] CGPointValue];
            
            // handle point
        }
        
    } else {

        // if the context doesn't match our current object's context
        // we want to pass the observation parameters to super
        [super observeValueForKeyPath: keyPath
                             ofObject: object
                               change: change
                              context: context];
    }
}

// The program can crash if an object is not removed as observer 
// before it is dealloc'd
//
// Helper method to remove self as an observer of the scrollView's
// contentOffset property
- (void)removeObserver {
    NSString *keyPath = NSStringFromSelector(@selector(contentOffset));
    [self.scrollView removeObserver: self forKeyPath: keyPath];
}

@end

```



## Querying KVC Data


```objc
if ([[dataObject objectForKey:@"yourVariable"] isEqualToString:"Hello World"]) {
    return YES;
} else {
    return NO;
}

```

You can query values stored using KVC quickly and easily, without needing to retrieve or cast these as local variables.



## Collection Operators


**Collection Operators** can be used in a KVC key path to perform an operation on a “collection-type” property (i.e. `NSArray`, `NSSet` and similar). For example, a common operation to perform is to count the objects in a collection. To achieve this, you use the `@count` **collection operator**:

```objc
self.array = @[@5, @4, @3, @2, @1];
NSNumber *count = [self.array valueForKeyPath:@"@count"];
NSNumber *countAlt = [self valueForKeyPath:@"array.@count"];
// count == countAlt == 5

```

While this is completely redundant here (we could have just accessed the `count` property), it **can** be useful on occasion, though it is rarely necessary. There are, however, some collection operators that are much more useful, namely `@max`, `@min`, `@sum`, `@avg` and the `@unionOf` family. It is important to note that these operators **also** require a separate key path **following** the operator to function correctly. Here's a list of them and the type of data they work with:

|Operator|Data Type
|---|---|---|---|---|---|---|---|---|---
|`@count`|(none)
|`@max`|`NSNumber`, `NSDate`, `int` (and related), etc.
|`@min`|`NSNumber`, `NSDate`, `int` (and related), etc.
|`@sum`|`NSNumber`, `int` (and related), `double` (and related), etc.
|`@avg`|`NSNumber`, `int` (and related), `double` (and related), etc.
|`@unionOfObjects`|`NSArray`, `NSSet`, etc.
|`@distinctUnionOfObjects`|`NSArray`, `NSSet`, etc.
|`@unionOfArrays`|`NSArray<NSArray*>`
|`@distinctUnionOfArrays`|`NSArray<NSArray*>`
|`@distinctUnionOfSets`|`NSSet<NSSet*>`

`@max` and `@min` will return the highest or lowest value, respectively, of a property of objects in the collection. For example, look at the following code:

```objc
// “Point” class used in our collection
@interface Point : NSObject

@property NSInteger x, y;

+ (instancetype)pointWithX:(NSInteger)x y:(NSInteger)y;

@end

...

self.points = @[[Point pointWithX:0 y:0],
                [Point pointWithX:1 y:-1],
                [Point pointWithX:5 y:-6],
                [Point pointWithX:3 y:0],
                [Point pointWithX:8 y:-4],
];

NSNumber *maxX = [self valueForKeyPath:@"points.@max.x"];
NSNumber *minX = [self valueForKeyPath:@"points.@min.x"];
NSNumber *maxY = [self valueForKeyPath:@"points.@max.y"];
NSNumber *minY = [self valueForKeyPath:@"points.@min.y"];

NSArray<NSNumber*> *boundsOfAllPoints = @[maxX, minX, maxY, minY];

...

```

In just a 4 lines of code and pure Foundation, with the power of Key-Value Coding collection operators we were able to extract a rectangle that encapsulates all of the points in our array.

It is important to note that these comparisons are made by invoking the `compare:` method on the objects, so if you ever want to make your own class compatible with these operators, you must implement this method.

`@sum` will, as you can probably guess, add up all the values of a property.

```objc
@interface Expense : NSObject

@property NSNumber *price;

+ (instancetype)expenseWithPrice:(NSNumber *)price;

@end

...

self.expenses = @[[Expense expenseWithPrice:@1.50],
                  [Expense expenseWithPrice:@9.99],
                  [Expense expenseWithPrice:@2.78],
                  [Expense expenseWithPrice:@9.99],
                  [Expense expenseWithPrice:@24.95]
];

NSNumber *totalExpenses = [self valueForKeyPath:@"expenses.@sum.price"];

```

Here, we used `@sum` to find the total price of all the expenses in the array. If we instead wanted to find the average price we're paying for each expense, we can use `@avg`:

```objc
NSNumber *averagePrice = [self valueForKeyPath:@"expenses.@avg.price"];

```

Finally, there's the `@unionOf` family. There are five different operators in this family, but they all work mostly the same, with only small differences between each. First, there's `@unionOfObjects` which will return an array of the properties of objects in an array:

```objc
// See "expenses" array above

NSArray<NSNumber*> *allPrices = [self valueForKeyPath:
    @"expenses.@unionOfObjects.price"];

// Equal to @[ @1.50, @9.99, @2.78, @9.99, @24.95 ]

```

`@distinctUnionOfObjects` functions the same as `@unionOfObjects`, but it removes duplicates:

```objc
NSArray<NSNumber*> *differentPrices = [self valueForKeyPath:
    @"expenses.@distinctUnionOfObjects.price"];

// Equal to @[ @1.50, @9.99, @2.78, @24.95 ]

```

And finally, the last 3 operators in the `@unionOf` family will go one step deeper and return an array of values found for a property contained inside dually-nested arrays:

```objc
NSArray<NSArray<Expense*,Expense*>*> *arrayOfArrays =
    @[
        @[ [Expense expenseWithPrice:@19.99],
           [Expense expenseWithPrice:@14.95],
           [Expense expenseWithPrice:@4.50],
           [Expense expenseWithPrice:@19.99]
         ],

        @[ [Expense expenseWithPrice:@3.75],
           [Expense expenseWithPrice:@14.95]
         ]
     ];

// @unionOfArrays
NSArray<NSNumber*> allPrices = [arrayOfArrays valueForKeyPath:
    @"@unionOfArrays.price"];
// Equal to @[ @19.99, @14.95, @4.50, @19.99, @3.75, @14.95 ];

// @distinctUnionOfArrays
NSArray<NSNumber*> allPrices = [arrayOfArrays valueForKeyPath:
    @"@distinctUnionOfArrays.price"];
// Equal to @[ @19.99, @14.95, @4.50, @3.75 ];

```

The one missing from this example is `@distinctUnionOfSets`, however this functions exactly the same as `@distinctUnionOfArrays`, but works with and returns `NSSet`s instead (there is no non-`distinct` version because in a set, every object must be distinct anyway).

And that's it! Collection operators can be really powerful if used correctly, and can help to avoid having to loop through stuff unnecessarily.

One last note: you can also use the standard collection operators on arrays of `NSNumber`s (without additional property access). To do this, you access the `self` pseudo-property that just returns the object:

```objc
NSArray<NSNumber*> *numbers = @[@0, @1, @5, @27, @1337, @2048];

NSNumber *largest = [numbers valueForKeyPath:@"@max.self"];
NSNumber *smallest = [numbers valueForKeyPath:@"@min.self"];
NSNumber *total = [numbers valueForKeyPath:@"@sum.self"];
NSNumber *average = [numbers valueForKeyPath:@"@avg.self"];

```

