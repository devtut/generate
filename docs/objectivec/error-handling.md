---
metaTitle: "Objective C - Error Handling"
description: "Asserting, Error & Exception handling with try catch block"
---

# Error Handling



## Asserting


```objectivec
@implemenetation Triangle

...

-(void)setAngles:(NSArray *)_angles {
    self.angles = _angles;
    
    NSAssert((self.angles.count == 3), @"Triangles must have 3 angles. Array '%@' has %i", self.angles, (int)self.angles.count);
    
    CGFloat angleA = [self.angles[0] floatValue];
    CGFloat angleB = [self.angles[1] floatValue];
    CGFloat angleC = [self.angles[2] floatValue];
    CGFloat sum = (angleA + angleB + angleC);
    NSAssert((sum == M_PI), @"Triangles' angles must add up to pi radians (180°). This triangle's angles add up to %f radians (%f°)", (float)sum, (float)(sum * (180.0f / M_PI)));
}

```

These assertions make sure that you don't give a triangle incorrect angles, by throwing an exception if you do. If they didn't throw an exception than the triangle, not being a true triangle at all, might cause some bugs in later code.



## Error & Exception handling with try catch block


Exceptions represent programmer-level bugs like trying to access an array element that doesn’t exist.

Errors are user-level issues like trying load a file that doesn’t exist. Because errors are expected during the normal execution of a program.

**Example:**

```

   NSArray *inventory = @[@"Sam",
                           @"John",
                           @"Sanju"];
    int selectedIndex = 3;
    @try {
        NSString * name = inventory[selectedIndex];
        NSLog(@"The selected Name is: %@", name);
    } @catch(NSException *theException) {
        NSLog(@"An exception occurred: %@", theException.name);
        NSLog(@"Here are some details: %@", theException.reason);
    } @finally {
        NSLog(@"Executing finally block");
    }

```

OUTPUT:

> 
An exception occurred: NSRangeException


> 
Here are some details: *** -[__NSArrayI objectAtIndex:]: index 3 beyond bounds [0 .. 2]


> 
Executing finally block




#### Syntax


- NSAssert(condition, fmtMessage, **arg1, arg2, ...**) (args in italics are optional) -- Asserts that **condition** evaluates to a true value. If it doesn't than the assertion will raise an exception (NSAssertionException), with the **fmtMessage** formatted with the args provided

