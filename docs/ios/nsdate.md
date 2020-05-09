---
metaTitle: "iOS - NSDate"
description: "NSDateFormatter, Date Comparison, Get Historic Time from NSDate (eg: 5s ago, 2m ago, 3h ago), Get Unix Epoch time, Get time cycle type (12-hour or 24-hour), Get NSDate from JSON Date format /Date(1268123281843)/, Get Current Date, Get NSDate Object N seconds from current date, Convert NSDate that is composed from hour and minute (only) to a full NSDate, UTC Time offset from NSDate with TimeZone"
---

# NSDate




## NSDateFormatter


Converting an `NSDate` object to string is just 3 steps.

### 1. Create an `NSDateFormatter` object

### Swift

```swift
let dateFormatter = NSDateFormatter()

```

### Swift 3

```swift
let dateFormatter = DateFormatter()

```

### Objective-C

```swift
NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];

```

### 2. Set the date format in which you want your string

### Swift

```swift
dateFormatter.dateFormat = "yyyy-MM-dd 'at' HH:mm"

```

### Objective-C

```swift
dateFormatter.dateFormat = @"yyyy-MM-dd 'at' HH:mm";

```

### 3. Get the formatted string

### Swift

```swift
let date = NSDate() // your NSDate object
let dateString = dateFormatter.stringFromDate(date)

```

### Swift 3

```swift
let date = Date() // your NSDate object
let dateString = dateFormatter.stringFromDate(date)

```

### Objective-C

```swift
NSDate *date = [NSDate date]; // your NSDate object
NSString *dateString = [dateFormatter stringFromDate:date];

```

This will give output something like this: `2001-01-02 at 13:00`

> 
<h3>Note</h3>
Creating an `NSDateFormatter` instance is an expensive operation, so it is recommended to create it once and reuse when possible.


### Useful extension for converting date to string.

```swift
extension Date {
        func toString() -> String {
            let dateFormatter = DateFormatter()
            dateFormatter.dateFormat = "MMMM dd yyyy"
            return dateFormatter.string(from: self)
        }
}

```

Useful links for swift date-formation [swiftly-getting-human-readable-date-nsdateformatter](http://unicode.org/reports/tr35/tr35-4.html#Date_Format_Patterns).

For constructing date formats see [date format patterns](http://www.codingexplorer.com/swiftly-getting-human-readable-date-nsdateformatter/).



## Date Comparison


There are 4 methods for comparing dates:

### Swift

- `isEqualToDate(anotherDate: NSDate) -> Bool`
- `earlierDate(anotherDate: NSDate) -> NSDate`
- `laterDate(anotherDate: NSDate) -> NSDate`
- `compare(anotherDate: NSDate) -> NSComparisonResult`

### Objective-C

- `- (BOOL)isEqualToDate:(NSDate *)anotherDate`
- `- (NSDate *)earlierDate:(NSDate *)anotherDate`
- `- (NSDate *)laterDate:(NSDate *)anotherDate`
- `- (NSComparisonResult)compare:(NSDate *)anotherDate`

Let's say we have 2 dates:

### Swift

```swift
let date1: NSDate = ... // initialized as  July 7, 2016 00:00:00
let date2: NSDate = ... // initialized as  July 2, 2016 00:00:00

```

### Objective-C

```swift
NSDate *date1 = ... // initialized as  July 7, 2016 00:00:00
NSDate *date2 = ... // initialized as  July 2, 2016 00:00:00

```

Then, to compare them, we try this code:

### Swift

```swift
if date1.isEqualToDate(date2) {
    // returns false, as both dates aren't equal
}

earlierDate: NSDate = date1.earlierDate(date2) // returns the earlier date of the two (date 2)
laterDate: NSDate = date1.laterDate(date2) // returns the later date of the two (date1)

result: NSComparisonResult = date1.compare(date2)

if result == .OrderedAscending {
    // true if date1 is earlier than date2
} else if result == .OrderedSame {
    // true if the dates are the same
} else if result == .OrderedDescending {
    // true if date1 is later than date1
}

```

### Objective-C

```swift
if ([date1 isEqualToDate:date2]) {
    // returns false, as both date are not equal
}

NSDate *earlierDate = [date1 earlierDate:date2]; // returns date which comes earlier from both date, here it will return date2
NSDate *laterDate = [date1 laterDate:date2]; // returns date which comes later from both date, here it will return date1

NSComparisonResult result = [date1 compare:date2];
if (result == NSOrderedAscending) {
    // fails
    // comes here if date1 is earlier then date2, in our case it will not come here
} else if (result == NSOrderedSame){
    // fails
    // comes here if date1 is same as date2, in our case it will not come here
} else{ // NSOrderedDescending
    // succeeds
    // comes here if date1 is later than date2, in our case it will come here
}

```

If you want to compare dates and handle seconds, weeks, months and years:

### Swift 3

```swift
let dateStringUTC = "2016-10-22 12:37:48 +0000"
let dateFormatter = DateFormatter()
dateFormatter.locale = Locale(identifier: "en_US_POSIX")
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss X"
let date = dateFormatter.date(from: dateStringUTC)!

let now = Date()

let formatter = DateComponentsFormatter()
formatter.unitsStyle = .full
formatter.maximumUnitCount = 2
let string = formatter.string(from: date, to: Date())! + " " + NSLocalizedString("ago", comment: "added after elapsed time to say how long before")

```

Or you can use this for each component:

```swift
// get the current date and time
let currentDateTime = Date()

// get the user's calendar
let userCalendar = Calendar.current

// choose which date and time components are needed
let requestedComponents: Set<Calendar.Component> = [
    .year,
    .month,
    .day,
    .hour,
    .minute,
    .second
]

// get the components
let dateTimeComponents = userCalendar.dateComponents(requestedComponents, from: currentDateTime)

// now the components are available
dateTimeComponents.year 
dateTimeComponents.month 
dateTimeComponents.day  
dateTimeComponents.hour 
dateTimeComponents.minute
dateTimeComponents.second

```



## Get Historic Time from NSDate (eg: 5s ago, 2m ago, 3h ago)


This can be used in various chat applications, rss feeds, and social apps where you need to have latest feeds with timestamps:

### Objective-C

```swift
- (NSString *)getHistoricTimeText:(NSDate *)since
{
    NSString *str;
    NSTimeInterval interval = [[NSDate date] timeIntervalSinceDate:since];
    if(interval < 60)
        str = [NSString stringWithFormat:@"%is ago",(int)interval];
    else if(interval < 3600)
    {
        int minutes = interval/60;
        str = [NSString stringWithFormat:@"%im ago",minutes];
    }
    else if(interval < 86400)
    {
        int hours =  interval/3600;
        
        str = [NSString stringWithFormat:@"%ih ago",hours];
    }
    else
    {
        NSDateFormatter *dateFormater=[[NSDateFormatter alloc]init];
        [dateFormater setLocale:[NSLocale currentLocale]];
        NSString *dateFormat = [NSDateFormatter dateFormatFromTemplate:@"MMM d, YYYY" options:0 locale:[NSLocale currentLocale]];
        [dateFormater setDateFormat:dateFormat];
        str = [dateFormater stringFromDate:since];
        
    }
    return str;
}

```



## Get Unix Epoch time


To get [Unix Epoch Time](https://en.wikipedia.org/wiki/Unix_time), use the constant `timeIntervalSince1970`:

### Swift

```swift
let date = NSDate() // current date
let unixtime = date.timeIntervalSince1970

```

### Objective-C

```swift
NSDate *date = [NSDate date]; // current date
int unixtime = [date timeIntervalSince1970];

```



## Get time cycle type (12-hour or 24-hour)


### Checking whether the current date contains the symbol for AM or PM

### Objective-C

```swift
NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
[formatter setLocale:[NSLocale currentLocale]];
[formatter setDateStyle:NSDateFormatterNoStyle];
[formatter setTimeStyle:NSDateFormatterShortStyle];
NSString *dateString = [formatter stringFromDate:[NSDate date]];
NSRange amRange = [dateString rangeOfString:[formatter AMSymbol]];
NSRange pmRange = [dateString rangeOfString:[formatter PMSymbol]];
BOOL is24h = (amRange.location == NSNotFound && pmRange.location == NSNotFound);

```

### Requesting the time cycle type from `NSDateFormatter`

### Objective-C

```swift
NSString *formatStringForHours = [NSDateFormatter dateFormatFromTemplate:@"j" options:0 locale:[NSLocale currentLocale]];
NSRange containsA = [formatStringForHours rangeOfString:@"a"];
BOOL is24h = containsA.location == NSNotFound;

```

This uses a special date template string called "j" which according to the [ICU Spec](http://unicode.org/reports/tr35/#Date_Format_Patterns) ...

> 
<p>[...] requests the preferred hour format for the locale (h, H, K, or k), as determined by the preferred attribute of the hours element in supplemental data. [...]
Note that use of 'j' in a skeleton passed to an API is the only way to have a skeleton request a locale's preferred time cycle type (12-hour or 24-hour).</p>


That last sentence is important. It "is the only way to have a skeleton request a locale's preferred time cycle type". Since `NSDateFormatter` and `NSCalendar` are built on the ICU library, the same holds true here.

> 
<h3>Reference</h3>
The second option was derived from [this answer](http://stackoverflow.com/a/11660380/410143).




## Get NSDate from JSON Date format "/Date(1268123281843)/"


Prior to Json.NET 4.5 dates were written using the Microsoft format: "/Date(1198908717056)/". If your server sends date in this format you can use the below code to serialize it to NSDate:

### Objective-C

```swift
(NSDate*) getDateFromJSON:(NSString *)dateString
{
    // Expect date in this format "/Date(1268123281843)/"
    int startPos = [dateString rangeOfString:@"("].location+1;
    int endPos = [dateString rangeOfString:@")"].location;
    NSRange range = NSMakeRange(startPos,endPos-startPos);
    unsigned long long milliseconds = [[dateString substringWithRange:range] longLongValue];
    NSLog(@"%llu",milliseconds);
    NSTimeInterval interval = milliseconds/1000;
    NSDate *date = [NSDate dateWithTimeIntervalSince1970:interval];
    // add code for date formatter if need NSDate in specific format.
    return date;
}

```



## Get Current Date


Getting current date is very easy.  You get NSDate object of current date in just single line as follows:

### Swift

```swift
var date = NSDate()

```

### Swift 3

```swift
var date = Date()

```

### Objective-C

```swift
NSDate *date = [NSDate date];

```



## Get NSDate Object N seconds from current date


The number of seconds from the current date and time for the new date. Use a negative value to specify a date before the current date.

For doing this we have a method named `dateWithTimerIntervalSinceNow(seconds: NSTimeInterval) -> NSDate` (Swift) or `+ (NSDate*)dateWithTimeIntervalSinceNow:(NSTimeInterval)seconds` (Objective-C).

Now for example, if you require a date one week from current date and one week to current date, then we can do it as.

### Swift

```swift
let totalSecondsInWeek:NSTimeInterval = 7 * 24 * 60 * 60;
//Using negative value for previous date from today
let nextWeek = NSDate().dateWithTimerIntervalSinceNow(totalSecondsInWeek)

//Using positive value for future date from today
let lastWeek = NSDate().dateWithTimerIntervalSinceNow(-totalSecondsInWeek)

```

### Swift 3

```swift
let totalSecondsInWeek:TimeInterval = 7 * 24 * 60 * 60;

//Using positive value to add to the current date
let nextWeek = Date(timeIntervalSinceNow: totalSecondsInWeek)

//Using negative value to get date one week from current date
let lastWeek = Date(timeIntervalSinceNow: -totalSecondsInWeek)

```

### Objective-C

```swift
NSTimeInterval totalSecondsInWeek = 7 * 24 * 60 * 60;
//Using negative value for previous date from today
NSDate *lastWeek = [NSDate dateWithTimeIntervalSinceNow:-totalSecondsInWeek];

//Using positive value for future date from today
NSDate *nextWeek = [NSDate dateWithTimeIntervalSinceNow:totalSecondsInWeek];

NSLog(@"Last Week: %@", lastWeek);
NSLog(@"Right Now: %@", now);
NSLog(@"Next Week: %@", nextWeek);

```



## Convert NSDate that is composed from hour and minute (only) to a full NSDate


There are many cases when one has created an NSDate from only an hour and minute format, i.e: 08:12 that returns from a server as a String and you initiate an NSDate instance by these **values only.**

The downside for this situation is that your NSDate is almost completely "naked" and what you need to do is to create: day, month, year, second and time zone in order to this object to "play along" with other NSDate types.

For the sake of the example let's say that hourAndMinute is the NSDate type that is composed from hour and minute format:

### Objective-C

```swift
NSDateComponents *hourAndMinuteComponents = [calendar components:NSCalendarUnitHour | NSCalendarUnitMinute
                                                         fromDate:hourAndMinute];
NSDateComponents *componentsOfDate = [[NSCalendar currentCalendar] components:NSCalendarUnitDay | NSCalendarUnitMonth | NSCalendarUnitYear
                                                                     fromDate:[NSDate date]];

NSDateComponents *components = [[NSDateComponents alloc] init];
[components setDay: componentsOfDate.day];
[components setMonth: componentsOfDate.month];
[components setYear: componentsOfDate.year];
[components setHour: [hourAndMinuteComponents hour]];
[components setMinute: [hourAndMinuteComponents minute]];
[components setSecond: 0];
[calendar setTimeZone: [NSTimeZone defaultTimeZone]];

NSDate *yourFullNSDateObject = [calendar dateFromComponents:components];

```

Now your object is the total opposite of being "naked".



## UTC Time offset from NSDate with TimeZone


Here this will calculate the `UTC` time offset from current data in desired timezone.

```swift
+(NSTimeInterval)getUTCOffSetIntervalWithCurrentTimeZone:(NSTimeZone *)current forDate:(NSDate *)date  {
    NSTimeZone *utcTimeZone = [NSTimeZone timeZoneWithAbbreviation:@"UTC"];
    NSInteger currentGMTOffset = [current secondsFromGMTForDate:date];
    NSInteger gmtOffset = [utcTimeZone secondsFromGMTForDate:date];
    NSTimeInterval gmtInterval = currentGMTOffset - gmtOffset;
    return gmtInterval;
}

```



#### Syntax


- NSDate() // NSDate object init to the current date and time
- NSDate().timeIntervalSince1970 // Current date and time in number of seconds from 00:00:00 UTC on 1 January 1970.
- NSDate().compare(other: NSDate) // Returns a comparison of current date to another date returns a `NSComparisonResult`



#### Remarks


There are different types of date format that you can set: Here is full list of them.

|Format|Meaning/Description|Example1|Example2
|---|---|---|---|---|---|---|---|---|---
|y|A year with at least 1 digit.|175 AD → “175”|2016 AD → “2016”
|yy|A year with exactly 2 digits.|5 AD → “05”|2016 AD → “16”
|yyy|A year with at least 3 digits.|5 AD → “005”|2016 AD → “2016”
|yyyy|A year with at least 4 digits.|5 AD → “0005”|2016 AD → “2016”
|M|A month with at least 1 digit.|July → “7”|"November" → "11"
|MM|A month with at least 2 digits.|July → “07”|"November" → "11"
|MMM|Three letter month abbreviation.|July → “Jul”|"November" → "Nov"
|MMMM|Full name of month.|July → “July”|"November" → "November"
|MMMMM|One letter month abbreviation(Jan,June,July all will have 'J').|July → “J”|"November" → "N"
|d|Day with at least one digit.|8 → “8”|29 → “29”
|dd|Day with at least two digits.|8 → “08”|29 → “29”
|“E”, “EE”, or”EEE”|3 letter day abbreviation of day name.|Monday → “Mon”|Thursday → “Thu”
|EEEE|Full Day name.|Monday → “Monday”|Thursday → “Thursday”
|EEEEE|1 letter day abbreviation of day name.(Thu and Tue will be 'T')|Monday → “M”|Thursday → “T”
|EEEEEE|2 letter day abbreviation of day name.|Monday → “Mo”|Thursday → “Th”
|a|Period of day (AM/PM).|10 PM → “PM”|2 AM → “AM”
|h|A 1-12 based hour with at least 1 digit.|10 PM → “10”|2 AM → “2”
|hh|A 1-12 based hour with at least 2 digits.|10 PM → “10”|2 AM → “02”
|H|A 0-23 based hour with at least 1 digit.|10 PM → “14”|2 AM → “2”
|HH|A 0-23 based hour with at least 2 digits.|10 PM → “14”|2 AM → “02”
|m|A minute with at least 1 digit.|7 → “7”|29 → “29”
|mm|A minute with at least 2 digits.|7 → “07”|29 → “29”
|s|A second with at least 1 digit.|7 → “7”|29 → “29”
|ss|A second with at least 2 digits.|7 → “07”|29 → “29”

There are many more, for getting different time based on zone(z), for getting time with millisecond details(S), etc.

