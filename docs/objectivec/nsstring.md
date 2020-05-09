---
metaTitle: "Objective-C - NSString"
description: "Encoding and Decoding, String Length, Comparing Strings, Splitting, Creation, Searching for a Substring, Changing Case, Joining an Array of Strings, Removing Leading and Trailing Whitespace, Formatting, Working with C Strings, Reversing a  NSString Objective-C"
---

# NSString


The **NSString** class is a part of Foundation framework to work with strings (series of characters). It also includes methods for comparing, searching and modifying strings.



## Encoding and Decoding


```objc
// decode
NSString *string = [[NSString alloc] initWithData:utf8Data
                                         encoding:NSUTF8StringEncoding];

// encode
NSData *utf8Data = [string dataUsingEncoding:NSUTF8StringEncoding];

```

Some supported encodings are:

- `NSASCIIStringEncoding`
- `NSUTF8StringEncoding`
- `NSUTF16StringEncoding` (== `NSUnicodeStringEncoding`)

Note that `utf8Data.bytes` does not include a terminating null character, which is necessary for C strings. If you need a C string, use `UTF8String`:

```objc
const char *cString = [string UTF8String];
printf("%s", cString);

```



## String Length


NSString has a `length` property to get the number of characters.

```objc
NSString *string = @"example";
NSUInteger length = string.length;       // length equals 7

```

As in the [Splitting Example](http://stackoverflow.com/documentation/objective-c/832/nsstring/3828/splitting), keep in mind that `NSString` uses [UTF-16](https://en.wikipedia.org/wiki/UTF-16) to represent characters. The length is actually just the number of UTF-16 code units. This can differ from what the user perceives as characters.

Here are some cases that might be surprising:

```objc
@"é".length == 1   // LATIN SMALL LETTER E WITH ACUTE (U+00E9)
@"é".length == 2   // LATIN SMALL LETTER E (U+0065) + COMBINING ACUTE ACCENT (U+0301)
@"❤️".length == 2  // HEAVY BLACK HEART (U+2764) + VARIATION SELECTOR-16 (U+FE0F)
@"🇮🇹".length == 4  // REGIONAL INDICATOR SYMBOL LETTER I (U+1F1EE) + REGIONAL INDICATOR SYMBOL LETTER T (U+1F1F9)

```

In order to get the number of user-perceived characters, known technically as "[grapheme clusters](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/Strings/Articles/stringsClusters.html)", you must iterate over the string with [`-enumerateSubstringsInRange:options:usingBlock:`](https://developer.apple.com/library/ios/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/#//apple_ref/occ/instm/NSString/enumerateSubstringsInRange:options:usingBlock:) and keep a count. This is demonstrated in [an answer by Nikolai Ruhe on Stack Overflow](http://stackoverflow.com/a/33539320/603977).



## Comparing Strings


Strings are compared for equality using `isEqualToString:`

The `==` operator just tests for object identity and does not compare the logical values of objects, so it can't be used:

```objc
NSString *stringOne = @"example";
NSString *stringTwo = [stringOne mutableCopy];

BOOL objectsAreIdentical = (stringOne == stringTwo);          // NO
BOOL stringsAreEqual = [stringOne isEqualToString:stringTwo]; // YES

```

The expression `(stringOne == stringTwo)` tests to see if the memory addresses of the two strings are the same, which is usually not what we want.

If the string variables can be `nil` you have to take care about this case as well:

```objc
BOOL equalValues = stringOne == stringTwo || [stringOne isEqualToString:stringTwo];

```

This condition returns `YES` when strings have equal values or both are `nil`.

To order two strings alphabetically, use `compare`:.

```objc
NSComparisonResult result = [firstString compare:secondString];

```

`NSComparisonResult` can be:

- `NSOrderedAscending`: The first string comes before the second string.
- `NSOrderedSame`: The strings are equal.
- `NSOrderedDescending`: The second string comes before the first string.

To compare two strings equality, use `isEqualToString:`.

```objc
BOOL result = [firstString isEqualToString:secondString];

```

To compare with the empty string (`@""`), better use `length`.

```objc
BOOL result = string.length == 0;

```



## Splitting


You can split a string into an array of parts, divided by **a separator character**.

```objc
NSString * yourString = @"Stack,Exchange,Network";
NSArray * yourWords = [yourString componentsSeparatedByString:@","]; 
// Output: @[@"Stack", @"Exchange", @"Network"]

```

If you need to split on a set of **several different delimiters**, use `-[NSString componentsSeparatedByCharactersInSet:]`.

```objc
NSString * yourString = @"Stack Overflow+Documentation/Objective-C";
NSArray * yourWords = [yourString componentsSeparatedByCharactersInSet:
                      [NSCharacterSet characterSetWithCharactersInString:@"+/"]];
// Output: @[@"Stack Overflow", @"Documentation", @"Objective-C"]`

```

If you need to break a string into its **individual characters**, loop
over the length of the string and convert each character into a new string.

```objc
NSMutableArray * characters = [[NSMutableArray alloc] initWithCapacity:[yourString length]];
for (int i = 0; i < [myString length]; i++) {
    [characters addObject: [NSString stringWithFormat:@"%C", 
                                      [yourString characterAtIndex:i]];
}

```

As in the [Length Example](http://stackoverflow.com/documentation/objective-c/832/nsstring/2882/string-length), keep in mind that a "character" here is a UTF-16 code unit, not necessarily what the user sees as a character. If you use this loop with `@"🇮🇹"`, you'll see that it's split into four pieces.

In order to get a list of the user-perceived characters, use [`-enumerateSubstringsInRange:options:usingBlock:`](https://developer.apple.com/library/ios/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/#//apple_ref/occ/instm/NSString/enumerateSubstringsInRange:options:usingBlock:).

```objc
NSMutableArray * characters = [NSMutableArray array];
[yourString enumerateSubstringsInRange:(NSRange){0, [yourString length]}
                               options:NSStringEnumerationByComposedCharacterSequences
                            usingBlock:^(NSString * substring, NSRange r, NSRange s, BOOL * b){
                                [characters addObject:substring];
                            }];

```

This preserves [grapheme clusters](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/Strings/Articles/stringsClusters.html) like the Italian flag as a single substring.



## Creation


**Simple:**

```objc
NSString *newString = @"My String";

```

**From multiple strings:**

```objc
NSString *stringOne = @"Hello";
NSString *stringTwo = @"world";
NSString *newString = [NSString stringWithFormat:@"My message: %@ %@",
                     stringOne, stringTwo];

```

****Using Mutable String****

```objc
NSString *stringOne = @"Hello";
NSString *stringTwo = @"World";
NSMutableString *mutableString = [NSMutableString new];
[mutableString appendString:stringOne];
[mutableString appendString:stringTwo];

```

**From NSData:**

When initializing from `NSData`, an explicit encoding must be provided as `NSString` is not able to guess how characters are represented in the raw data stream. The most common encoding nowadays is UTF-8, which is even a requirement for certain data like JSON.

Avoid using `+[NSString stringWithUTF8String:]` since it expects an explicitly NULL-terminated C-string, which `-[NSData bytes]` does **not** provide.

```objc
NSString *newString = [[NSString alloc] initWithData:myData encoding:NSUTF8StringEncoding];

```

**From NSArray:**

```objc
NSArray *myArray = [NSArray arrayWithObjects:@"Apple", @"Banana", @"Strawberry", @"Kiwi", nil];
NSString *newString = [myArray componentsJoinedByString:@" "];

```



## Searching for a Substring


To search if a String contains a substring, do the following:

```objc
NSString *myString = @"This is for checking substrings";
NSString *subString = @"checking"; 

BOOL doesContainSubstring = [myString containsString:subString];  // YES

```

If targeting iOS 7 or OS X 10.9 (or earlier):

```objc
BOOL doesContainSubstring = ([myString rangeOfString:subString].location != NSNotFound);  // YES

```



## Changing Case


To convert a String to uppercase, use `uppercaseString`:

```objc
NSString *myString = @"Emphasize this";
NSLog(@"%@", [myString uppercaseString]; // @"EMPHASIZE THIS"

```

To convert a String to lowercase, use `lowercaseString`:

```objc
NSString *myString = @"NORMALIZE this";
NSLog(@"%@", [myString lowercaseString]; // @"normalize this"

```

To capitalize the first letter character of each word in a string, use `capitalizedString`:

```objc
NSString *myString = @"firstname lastname";
NSLog(@"%@", [myString capitalizedString]); // @"Firstname Lastname"

```



## Joining an Array of Strings


To combine an `NSArray` of `NSString` into a new `NSString`:

```objc
NSArray *yourWords = @[@"Objective-C", @"is", @"just", @"awesome"];
NSString *sentence = [yourWords componentsJoinedByString:@" "];

// Sentence is now: @"Objective-C is just awesome"

```



## Removing Leading and Trailing Whitespace


```objc
NSString *someString = @"   Objective-C Language  \n";
NSString *trimmedString = [someString stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
//Output will be - "Objective-C Language"

```

Method stringByTrimmingCharactersInSet returns a new string made by removing from both ends of the String characters contained in a given character set.

We can also just remove only whitespace or newline

```objc
// Removing only WhiteSpace
NSString *trimmedWhiteSpace = [someString stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
//Output will be - "Objective-C Language  \n"

// Removing only NewLine
NSString *trimmedNewLine = [someString stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
//Output will be - "   Objective-C Language  "

```



## Formatting


The `NSString` formatting supports all the format strings available on the `printf` ANSI-C function. The only addition made by the language is the `%@` symbol used for formatting all the Objective-C objects.

It is possible to format integers

```objc
int myAge = 21;
NSString *formattedAge = [NSString stringWithFormat:@"I am %d years old", my_age];

```

Or any object subclassed from NSObject

```objc
NSDate *now = [NSDate date];
NSString *formattedDate = [NSString stringWithFormat:@"The time right now is: %@", now];

```

For a complete list of Format Specifiers, please see: [Objective-C, Format Specifiers, Syntax](http://stackoverflow.com/documentation/objective-c/9048/format-specifiers#t=201702111440440714311&a=syntax)



## Working with C Strings


To convert `NSString` to `const char` use [`-[NSString UTF8String]`](http://developer.apple.com/mac/library/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/Reference/NSString.html#//apple_ref/occ/instm/NSString/UTF8String):

```objc
NSString *myNSString = @"Some string";
const char *cString = [myNSString UTF8String];

```

You could also use [`-[NSString cStringUsingEncoding:]`](http://developer.apple.com/mac/library/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/Reference/NSString.html#//apple_ref/occ/instm/NSString/cStringUsingEncoding:) if your string is encoded with something other than UTF-8.

For the reverse path use [`-[NSString stringWithUTF8String:]`](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/index.html#//apple_ref/occ/clm/NSString/stringWithUTF8String:):

```objc
const *char cString = "Some string";
NSString *myNSString = [NSString stringWithUTF8String:cString];
myNSString = @(cString); // Equivalent to the above.

```

Once you have the `const char *`, you can work with it similarly to an array of `chars`:

```objc
printf("%c\n", cString[5]);

```

If you want to modify the string, make a copy:

```objc
char *cpy = calloc(strlen(cString)+1, 1);
strncpy(cpy, cString, strlen(cString));
// Do stuff with cpy
free(cpy);

```



## Reversing a  NSString Objective-C


```objc
// myString is "hi"
NSMutableString *reversedString = [NSMutableString string];
NSInteger charIndex = [myString length];
while (charIndex > 0) {
    charIndex--;
    NSRange subStrRange = NSMakeRange(charIndex, 1);
    [reversedString appendString:[myString substringWithRange:subStrRange]];
}
NSLog(@"%@", reversedString); // outputs "ih"

```



#### Remarks


For nesting various types of objects and data-types into NSStrings refer to: [Objective-C, Format Specifiers](http://stackoverflow.com/documentation/objective-c/9048/format-specifiers#t=201702111426046577396)

