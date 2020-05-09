---
metaTitle: "Objective C - NSRegularExpression"
description: "Check whether a string matches a pattern, Find all the numbers in a string"
---

# NSRegularExpression



## Check whether a string matches a pattern


```objectivec
NSString *testString1 = @"(555) 123-5678";
NSString *testString2 = @"not a phone number";

NSError *error = nil;
NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"^\\(\\d{3}\\) \\d{3}\\-\\d{4}$"
                                                                       options:NSRegularExpressionCaseInsensitive error:&error];

NSInteger result1 = [regex numberOfMatchesInString:testString1 options:0 range:NSMakeRange(0, testString1.length)];
NSInteger result2 = [regex numberOfMatchesInString:testString2 options:0 range:NSMakeRange(0, testString2.length)];

NSLog(@"Is string 1 a phone number? %@", result1 > 0 ? @"YES" : @"NO");
NSLog(@"Is string 2 a phone number? %@", result2 > 0 ? @"YES" : @"NO");

```

The output will show that the first string is a phone number and the second one isn't.



## Find all the numbers in a string


```objectivec
NSString *testString = @"There are 42 sheep and 8672 cows.";
NSError *error = nil;
NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"(\\d+)"
                                                                       options:NSRegularExpressionCaseInsensitive
                                                                         error:&error];

NSArray *matches = [regex matchesInString:testString
                                  options:0
                                    range:NSMakeRange(0, testString.length)];

for (NSTextCheckingResult *matchResult in matches) {
    NSString* match = [testString substringWithRange:matchResult.range];
    NSLog(@"match: %@", match);
}

```

The output will be `match: 42` and `match: 8672`.



#### Syntax


- NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:PATTERN options:OPTIONS error:ERROR];
- NSArray<NSTextCheckingResult *> *results = [regex matchesInString:STRING options:OPTIONS range:RANGE_IN_STRING];
- NSInteger numberOfMatches = [regex numberOfMatchesInString:STRING options:OPTIONS range:RANGE_IN_STRING];

