---
metaTitle: "Objective-C - Inheritance"
description: "Car is inherited from Vehicle"
---

# Inheritance



## Car is inherited from Vehicle


Consider a base class **Vehicle** and its derived class **Car** as follows:

```objc
#import <Foundation/Foundation.h>
 
@interface Vehicle : NSObject

{
    NSString *vehicleName;
    NSInteger vehicleModelNo;
}

- (id)initWithName:(NSString *)name andModel:(NSInteger)modelno;
- (void)print;
@end

@implementation Vehicle

- (id)initWithName:(NSString *)name andModel:(NSInteger)modelno{
    vehicleName = name;
    vehicleModelNo = modelno;
    return self;
}

- (void)print{
    NSLog(@"Name: %@", vehicleName);
    NSLog(@"Model: %ld", vehicleModelNo);
}

@end

@interface Car : Vehicle

{
    NSString *carCompanyName;
}

- (id)initWithName:(NSString *)name andModel:(NSInteger)modelno 
  andCompanyName:(NSString *)companyname;
- (void)print;

@end


@implementation Car

- (id)initWithName:(NSString *)name andModel:(NSInteger) modelno 
  andCompanyName: (NSString *) companyname
  {
    vehicleName = name;
    vehicleModelNo = modelno;
    carCompanyName = companyname;
    return self;
}
- (void)print
{
    NSLog(@"Name: %@", vehicleName);
    NSLog(@"Model: %ld", vehicleModelNo);
    NSLog(@"Company: %@", carCompanyName);
}

@end

int main(int argc, const char * argv[])
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];        
    NSLog(@"Base class Vehicle Object");
    Vehicle *vehicle = [[Vehicle alloc]initWithName:@"4Wheeler" andModel:1234];
    [vehicle print];
    NSLog(@"Inherited Class Car Object");
    Car *car = [[Car alloc]initWithName:@"S-Class" 
    andModel:7777 andCompanyName:@"Benz"];
    [car print];        
    [pool drain];
    return 0;
}

```

When the above code is compiled and executed, it produces the following result:

> 
2016-09-29 18:21:03.561 Inheritance[349:303] Base class Vehicle Object
2016-09-29 18:21:03.563 Inheritance[349:303] Name: 4Wheeler
2016-09-29 18:21:03.563 Inheritance[349:303] Model: 1234
2016-09-29 18:21:03.564 Inheritance[349:303] Inherited Class Car Object
2016-09-29 18:21:03.564 Inheritance[349:303] Name: S-Class
2016-09-29 18:21:03.565 Inheritance[349:303] Model: 7777
2016-09-29 18:21:03.565 Inheritance[349:303] Company: Benz




#### Syntax


1. @interface derived-class-Name: base-class-Name

