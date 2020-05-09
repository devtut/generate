---
metaTitle: "Objective C - NSDate"
description: "Creating an NSDate, Date Comparison, Convert NSDate that is composed from hour and minute (only) to a full NSDate, Converting NSDate to NSString"
---

# NSDate



## Creating an NSDate


The `NSDate` class provides methods for creating `NSDate` objects corresponding to a given date and time. An `NSDate` can be initialized using the designated initializer, which:

> 
Returns an `NSDate` object initialized relative to 00:00:00 UTC on 1 January 2001 by a given number of seconds.


```objectivec
NSDate *date = [[NSDate alloc] initWithTimeIntervalSinceReferenceDate:100.0];

```

`NSDate` also provides an easy way to create an `NSDate` equal to the current date and time:

```objectivec
NSDate *now = [NSDate date];

```

It is also possible to create an `NSDate` a given amount of seconds from the current date and time:

```objectivec
NSDate *tenSecondsFromNow = [NSDate dateWithTimeIntervalSinceNow:10.0];

```



## Date Comparison


There are 4 methods for comparing `NSDate`s in Objective-C:

- `- (BOOL)isEqualToDate:(NSDate *)anotherDate`
- `- (NSDate *)earlierDate:(NSDate *)anotherDate`
- `- (NSDate *)laterDate:(NSDate *)anotherDate`
- `- (NSComparisonResult)compare:(NSDate *)anotherDate`

Consider the following example using 2 dates, `NSDate date1 = July 7, 2016` and `NSDate date2 = July 2, 2016`:

```objectivec
NSDateComponents *comps1 = [[NSDateComponents alloc]init];
comps.year = 2016;
comps.month = 7;
comps.day = 7;

NSDateComponents *comps2 = [[NSDateComponents alloc]init];
    comps.year = 2016;
    comps.month = 7;
    comps.day = 2;

NSDate* date1 = [calendar dateFromComponents:comps1]; //Initialized as July 7, 2016 
NSDate* date2 = [calendar dateFromComponents:comps2]; //Initialized as July 2, 2016 

```

Now that the `NSDate`s are created, they can be compared:

```objectivec
if ([date1 isEqualToDate:date2]) {
    //Here it returns false, as both dates are not equal
}

```

We can also use the `earlierDate:` and `laterDate:` methods of the `NSDate` class:

```objectivec
NSDate *earlierDate = [date1 earlierDate:date2];//Returns the earlier of 2 dates. Here earlierDate will equal date2.
NSDate *laterDate = [date1 laterDate:date2];//Returns the later of 2 dates. Here laterDate will equal date1.

```

Lastly, we can use `NSDate`'s `compare:` method:

```objectivec
NSComparisonResult result = [date1 compare:date2];
    if (result == NSOrderedAscending) {
        //Fails
        //Comes here if date1 is earlier than date2. In our case it will not come here.
    }else if (result == NSOrderedSame){
        //Fails
        //Comes here if date1 is the same as date2. In our case it will not come here.
    }else{//NSOrderedDescending
        
        //Succeeds
        //Comes here if date1 is later than date2. In our case it will come here
    }

```



## Convert NSDate that is composed from hour and minute (only) to a full NSDate


There are many cases when one has created an NSDate from only an hour and minute format, i.e: 08:12

The downside for this situation is that your NSDate is almost completely "naked" and what you need to do is to create: day, month, year, second and time zone in order to this object to "play along" with other NSDate types.

For the sake of the example let's say that hourAndMinute is the NSDate type that is composed from hour and minute format:

```objectivec
NSDateComponents *hourAndMintuteComponents = [calendar components:NSCalendarUnitHour | NSCalendarUnitMinute
                                                         fromDate:hourAndMinute];
NSDateComponents *componentsOfDate = [[NSCalendar currentCalendar] components:NSCalendarUnitDay | NSCalendarUnitMonth | NSCalendarUnitYear
                                                                     fromDate:[NSDate date]];

NSDateComponents *components = [[NSDateComponents alloc] init];
[components setDay: componentsOfDate.day];
[components setMonth: componentsOfDate.month];
[components setYear: componentsOfDate.year];
[components setHour: [hourAndMintuteComponents hour]];
[components setMinute: [hourAndMintuteComponents minute]];
[components setSecond: 0];
[calendar setTimeZone: [NSTimeZone defaultTimeZone]];

NSDate *yourFullNSDateObject = [calendar dateFromComponents:components];

```

Now your object is the total opposite of being "naked".



## Converting NSDate to NSString


If ww have NSDate object, and we want to convert it into NSString. There are different types of Date strings. How we can do that?, It is very simple. Just 3 steps.

1. Create NSDateFormatter Object

```

   NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];

```


1. Set the date format in which you want your string.

```

   dateFormatter.dateFormat = @"yyyy-MM-dd 'at' HH:mm";

```


<li>
Now, get the formatted string

```objectivec
 NSDate *date = [NSDate date]; // your NSDate object
 NSString *dateString = [dateFormatter stringFromDate:date];

```


</li>

This will give output something like this: `2001-01-02 at 13:00`

**Note:**

Creating an `NSDateFormatter` instance is an expensive operation, so it is recommended to create it once and reuse when possible.



#### Remarks


`NSDate` is a very simple value type, representing one exact moment in universal time. It does not contain information about time zones, daylight saving time, calendars, or locale.

`NSDate` is really only an immutable wrapper around an `NSTimeInterval` which is a `double`. There is no mutable subclass, as with some other value types in Foundation.

