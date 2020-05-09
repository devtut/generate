---
metaTitle: "Objective C - Enums"
description: "typedef enum declaration in Objective-C, Defining an enum, Converting C++ std::vector<Enum> to an Objective-C Array"
---

# Enums



## typedef enum declaration in Objective-C


A enum declares a set of ordered values - the typedef just adds a handy name to this. The 1st element is 0 etc.

```

typedef enum {
        Monday=1,
        Tuesday,
        Wednesday

    } WORKDAYS;

    WORKDAYS today = Monday;//value 1

```



## Defining an enum


Enums are defined by the following the syntax above.

```objectivec
typedef NS_ENUM(NSUInteger, MyEnum) {
    MyEnumValueA,
    MyEnumValueB,
    MyEnumValueC,
};

```

You also can set your own raw-values to the enumeration types.

```objectivec
typedef NS_ENUM(NSUInteger, MyEnum) {
    MyEnumValueA = 0,
    MyEnumValueB = 5,
    MyEnumValueC = 10,
};

```

You can also specify on the first value and all the following will use it with increment:

```objectivec
typedef NS_ENUM(NSUInteger, MyEnum) {
    MyEnumValueA = 0,
    MyEnumValueB,
    MyEnumValueC,
};

```

Variables of this enum can be created by `MyEnum enumVar = MyEnumValueA`.



## Converting C++ std::vector<Enum> to an Objective-C Array


Many C++ libraries use enums and return/receive data using vectors that contain enums. As C enums are not Objective-C objects, Objective-C collections cannot be used directly with C enums. The example below deals with this by using a combination of an NSArray and generics and a wrapper object for the array. This way, the collection can be explicit about the data type and there is no worry about possible memory leaks with C arrays Objective-C objects are used.

Here is the C enum & Objective-C equivalent object:

```objectivec
typedef enum 
{  
  Error0 = 0,
  Error1 = 1,
  Error2 = 2
} MyError;

@interface ErrorEnumObj : NSObject

@property (nonatomic) int intValue;

+ (instancetype) objWithEnum:(MyError) myError;
- (MyError) getEnumValue;

@end 

@implementation ErrorEnumObj

+ (instancetype) objWithEnum:(MyError) error
{
    ErrorEnumObj * obj = [ErrorEnumObj new];
    obj.intValue = (int)error;
    return obj;
}

- (MyError) getEnumValue
{
    return (MyError)self.intValue;
}

@end

```

And here is a possible use of it in Objective-C++ (the resulting NSArray can be used in Objective-C only files as no C++ is used).

```objectivec
class ListenerImpl : public Listener
{
public:
    ListenerImpl(Listener* listener) : _listener(listener) {}
    void onError(std::vector<MyError> errors) override
    {
        NSMutableArray<ErrorEnumObj *> * array = [NSMutableArray<ErrorEnumObj *> new]; 
        for (auto&& myError : errors)
        {
            [array addObject:[ErrorEnumObj objWithEnum:myError]];
        }
        [_listener onError:array];
    }

private:
    __weak Listener* _listener;
}

```

If this kind of solution is to be used on multiple enums, the creation of the EnumObj (declaration & implementation) can be done using a macro (to create a template like solution).



#### Syntax


- typedef NS_ENUM(type, name) {...} -- **type** is the type of enumeration and **name** is the name of the enum. values are in "...". This creates a basic enum and a type to go with it; programs like Xcode will assume a variable with the enum type has one of the enum values

