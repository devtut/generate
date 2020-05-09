---
metaTitle: "Objective-C - Methods"
description: "Class methods, Method parameters, Create a basic method, Return values, Calling methods, Instance methods, Pass by value parameter passing, Pass by reference parameter passing"
---

# Methods



## Class methods


A class method is called on the class the method belongs to, not an instance of it. This is possible because Objective-C classes are also objects. To denote a method as a class method, change the `-` to a `+`:

```objc
+ (void)hello {
  NSLog(@"Hello World");
}

```



## Method parameters


If you want to pass in values to a method when it is called, you use parameters:

```objc
- (int)addInt:(int)intOne toInt:(int)intTwo {
  return intOne + intTwo;
}

```

The colon (`:`) separates the parameter from the method name.

The parameter type goes in the parentheses `(int)`.

The parameter name goes after the parameter type.



## Create a basic method


This is how to create a basic method that logs 'Hello World" to the console:

```objc
- (void)hello {
  NSLog(@"Hello World");
}

```

The `-` at the beginning denotes this method as an instance method.

The `(void)` denotes the return type. This method doesn't return anything, so you enter `void`.

The 'hello' is the name of the method.

Everything in the `{}` is the code run when the method is called.



## Return values


When you want to return a value from a method, you put the type you want to return in the first set of parentheses.

```objc
- (NSString)returnHello {
  return @"Hello World";
}

```

The value you want to return goes after the `return` keyword;



## Calling methods


Calling an instance method:

```objc
[classInstance hello];

 @interface Sample
 -(void)hello; // exposing the class Instance method
 @end

 @implementation Sample
     -(void)hello{
        NSLog(@"hello");
      }
 @end

```

Calling an instance method on the current instance:

```objc
[self hello];

@implementation Sample

     -(void)otherMethod{
       [self hello];
     }

     -(void)hello{
        NSLog(@"hello");
      }
 @end

```

Calling a method that takes arguments:

```objc
[classInstance addInt:1 toInt:2];

 @implementation Sample
     -(void)add:(NSInteger)add to:(NSInteger)to
        NSLog(@"sum = %d",(add+to));
      }
 @end

```

Calling a class method:

```objc
[Class hello];

 @interface Sample
 +(void)hello; // exposing the class method
 @end

 @implementation Sample
     +(void)hello{
        NSLog(@"hello");
      }
 @end

```



## Instance methods


An instance method is a method that's available on a particular instance of a class, after the instance has been instantiated:

```objc
MyClass *instance = [MyClass new];
[instance someInstanceMethod];

```

Here's how you define one:

```objc
@interface MyClass : NSObject

- (void)someInstanceMethod; // "-" denotes an instance method

@end

@implementation MyClass

- (void)someInstanceMethod {
    NSLog(@"Whose idea was it to have a method called \"someInstanceMethod\"?");
}

@end

```



## Pass by value parameter passing


In pass by value of parameter passing to a method, actual parameter value is copied to formal parameter value. So actual parameter value will not change after returning from called function.

```objc
@interface SwapClass : NSObject

-(void) swap:(NSInteger)num1 andNum2:(NSInteger)num2;

@end

@implementation SwapClass

-(void) num:(NSInteger)num1 andNum2:(NSInteger)num2{
    int temp;
    temp = num1;
    num1 = num2;
    num2 = temp;
}
@end

```

Calling the methods:

```objc
NSInteger a = 10, b =20;
SwapClass *swap = [[SwapClass alloc]init];
NSLog(@"Before calling swap: a=%d,b=%d",a,b);
[swap num:a andNum2:b];
NSLog(@"After calling swap: a=%d,b=%d",a,b);

```

Output:

```objc
2016-07-30 23:55:41.870 Test[5214:81162] Before calling swap: a=10,b=20
2016-07-30 23:55:41.871 Test[5214:81162] After calling swap: a=10,b=20

```



## Pass by reference parameter passing


In pass by reference of parameter passing to a method, address of actual parameter is passed to formal parameter. So actual parameter value will be changed after returning from called function.

```objc
@interface SwapClass : NSObject

-(void) swap:(int)num1 andNum2:(int)num2;

@end

@implementation SwapClass

-(void) num:(int*)num1 andNum2:(int*)num2{
    int temp;
    temp = *num1;
    *num1 = *num2;
    *num2 = temp;
}
@end

```

Calling the methods:

```objc
int a = 10, b =20;
SwapClass *swap = [[SwapClass alloc]init];
NSLog(@"Before calling swap: a=%d,b=%d",a,b);
[swap num:&a andNum2:&b];
NSLog(@"After calling swap: a=%d,b=%d",a,b);

```

Output:

```objc
2016-07-31 00:01:47.067 Test[5260:83491] Before calling swap: a=10,b=20
2016-07-31 00:01:47.070 Test[5260:83491] After calling swap: a=20,b=10

```



#### Syntax


<li>
`-` or `+`: The type of method. Instance or class?
</li>
<li>
(): Where the return type goes. Use void if you don't want to return anything!
</li>
<li>
Next is the name of the method. Use camelCase and make the name easy to remember an understand.
</li>
<li>
If your method needs parameters, now is the time! The first parameter come right after the name of the function like this `:(type)parameterName`. All the other parameters are done this way `parameterLabel:(type)parameterName`
</li>
<li>
What does your method do? Put it all here, in the curly braces {}!
</li>

