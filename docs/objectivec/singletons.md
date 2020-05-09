---
metaTitle: "Objective-C - Singletons"
description: "Using Grand Central Dispatch (GCD), Creating Singleton and also preventing it from having multiple instance using alloc/init, new., Creating Singleton class and also preventing it from having multiple instances using alloc/init."
---

# Singletons


Just make sure you read this thread ( [What is so bad about singletons?](http://stackoverflow.com/questions/137975/what-is-so-bad-about-singletons) ) before using it.



## Using Grand Central Dispatch (GCD)


GCD will guarantee that your singleton only gets instantiated once, even if called from multiple threads. Insert this into any class for a singleton instance called `shared`.

```objc
+ (instancetype)shared {

    // Variable that will point to the singleton instance. The `static`
    // modifier makes it behave like a global variable: the value assigned
    // to it will "survive" the method call.
    static id _shared;

    static dispatch_once_t _onceToken;
    dispatch_once(&_onceToken, ^{

        // This block is only executed once, in a thread-safe way.
        // Create the instance and assign it to the static variable.
        _shared = [self new];
    });

    return _shared;
}

```



## Creating Singleton and also preventing it from having multiple instance using alloc/init, new.


```objc
//MySingletonClass.h
@interface MYSingletonClass : NSObject

+ (instancetype)sharedInstance;

-(instancetype)init NS_UNAVAILABLE;

-(instancetype)new NS_UNAVAILABLE;

@end

//MySingletonClass.m

@implementation MySingletonClass

+ (instancetype)sharedInstance
{
    static MySingletonClass *_sharedInstance = nil;
    static dispatch_once_t oncePredicate;
    dispatch_once(&oncePredicate, ^{
        _sharedInstance = [[self alloc]init];
    });
    
    return _sharedInstance;
}
-(instancetype)init
{
    self = [super init];
    if(self)
    {
        //Do any additional initialization if required
    }
    return self;
}
@end

```



## Creating Singleton class and also preventing it from having multiple instances using alloc/init.


We can create Singleton class in such a way that developers are forced to used the shared instance (singleton object) instead of creating their own instances.

```objc
@implementation MySingletonClass

+ (instancetype)sharedInstance
{
    static MySingletonClass *_sharedInstance = nil;
    static dispatch_once_t oncePredicate;
    dispatch_once(&oncePredicate, ^{
        _sharedInstance = [[self alloc] initClass];
    });
    
    return _sharedInstance;
}

-(instancetype)initClass
{
    self = [super init];
    if(self)
    {
        //Do any additional initialization if required
    }
    return self;
}

- (instancetype)init
{
    @throw [NSException exceptionWithName:@"Not designated initializer"
                                   reason:@"Use [MySingletonClass sharedInstance]"
                                 userInfo:nil];
    return nil;
}
@end



/*Following line will throw an exception 
  with the Reason:"Use [MySingletonClass sharedInstance]" 
  when tried to alloc/init directly instead of using sharedInstance */
MySingletonClass *mySingletonClass = [[MySingletonClass alloc] init];

```

