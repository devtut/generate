---
metaTitle: "Objective-C - NSCalendar"
description: "System Locale Information, Initializing a Calendar, Calendrical Calculations"
---

# NSCalendar



## System Locale Information


`+currentCalendar` returns the logical calendar for the current user.

```

   NSCalendar *calender = [NSCalendar currentCalendar];
    NSLog(@"%@",calender);

```

`+autoupdatingCurrentCalendar` returns the current logical calendar for the current user.

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
NSLog(@"%@",calender); 

```



## Initializing a Calendar


`- initWithCalendarIdentifier:` Initializes a newly-allocated NSCalendar object for the calendar specified by a given identifier.

```objc
NSCalendar *calender = [[NSCalendar alloc]initWithCalendarIdentifier:@"gregorian"];
NSLog(@"%@",calender);

```

`- setFirstWeekday:` Sets the index of the first weekday for the receiver.

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
[calender setFirstWeekday:1];
NSLog(@"%d",[calender firstWeekday]);

```

`- setLocale:` Sets the locale for the receiver.

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
[calender setLocale:[NSLocale currentLocale]];
NSLog(@"%@",[calender locale]);

```

`- setMinimumDaysInFirstWeek:` Sets the minimum number of days in the first week of the receiver.

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
[calender setMinimumDaysInFirstWeek:7];
NSLog(@"%d",[calender minimumDaysInFirstWeek]);     

```

`- setTimeZone:` Sets the time zone for the receiver.

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
[calender setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
NSLog(@"%@",[calender timeZone]);

```



## Calendrical Calculations


`- components:fromDate:` Returns a NSDateComponents object containing a given date decomposed into specified components

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
[calender setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
NSLog(@"%@",[calender components:NSCalendarUnitDay fromDate:[NSDate date]]);
NSLog(@"%@",[calender components:NSCalendarUnitYear fromDate:[NSDate date]]);
NSLog(@"%@",[calender components:NSCalendarUnitMonth fromDate:[NSDate date]]);

```

`- components:fromDate:toDate:options:` Returns, as an NSDateComponents object using specified components, the difference between two supplied dates.

```

NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
[calender setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
NSLog(@"%@",[calender components:NSCalendarUnitYear fromDate:[NSDate dateWithTimeIntervalSince1970:0] toDate:[NSDate dateWithTimeIntervalSinceNow:18000] options:NSCalendarWrapComponents]);

```

`- dateByAddingComponents:toDate:options:` Returns a new NSDate object representing the absolute time calculated by adding given components to a given date.

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
NSDateComponents *dateComponent = [[NSDateComponents alloc]init];
[dateComponent setYear:10];
NSLog(@"%@",[calender dateByAddingComponents:dateComponent toDate:[NSDate             dateWithTimeIntervalSinceNow:0] options:NSCalendarWrapComponents] );

```

`- dateFromComponents:` Returns a new NSDate object representing the absolute time calculated from given components.

```objc
NSCalendar *calender = [NSCalendar autoupdatingCurrentCalendar];
NSDateComponents *dateComponent = [[NSDateComponents alloc]init];
[dateComponent setYear:2020];
NSLog(@"%@",[calender dateFromComponents:dateComponent]);

```

