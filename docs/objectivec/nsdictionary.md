---
metaTitle: "Objective C - NSDictionary"
description: "Creating using literals, Creating using dictionaryWithObjectsAndKeys:, Creating using plists, Setting a Value in NSDictionary, Getting a Value from NSDictionary, Check if NSDictionary already has a key or not"
---

# NSDictionary



## Creating using literals


```objectivec
NSDictionary *inventory = @{
    @"Mercedes-Benz SLK250" : @(13),
    @"BMW M3 Coupe" : @(self.BMWM3CoupeInventory.count),
    @"Last Updated" : @"Jul 21, 2016",
    @"Next Update"  : self.nextInventoryUpdateString
};

```



## Creating using dictionaryWithObjectsAndKeys:


```objectivec
NSDictionary *inventory = [NSDictionary dictionaryWithObjectsAndKeys:
    [NSNumber numberWithInt:13], @"Mercedes-Benz SLK250",
    [NSNumber numberWithInt:22], @"Mercedes-Benz E350",
    [NSNumber numberWithInt:19], @"BMW M3 Coupe",
    [NSNumber numberWithInt:16], @"BMW X6",
    nil];

```

`nil` must be passed as the last parameter as a sentinel signifying the end.

It's important to remember that when instantiating dictionaries this way the values go first and the keys second. In the example above the strings are the keys and the numbers are the values. The method's name reflects this too: `dictionaryWithObjectsAndKeys`.
While this is not incorrect, the more modern way of instantiating dictionaries (with literals) is prefered.



## Creating using plists


```objectivec
NSString *pathToPlist = [[NSBundle mainBundle] pathForResource:@"plistName" 
    ofType:@"plist"];
NSDictionary *plistDict = [[NSDictionary alloc] initWithContentsOfFile:pathToPlist];

```



## Setting a Value in NSDictionary


There are multiple ways to set a key's object in an NSDictionary, corresponding to the ways you get a value. For instance, to add a lamborghini to a list of cars

### Standard

```objectivec
[cars setObject:lamborghini forKey:@"Lamborghini"];

```

Just like any other object, call the method of NSDictionary that sets an object of a key, `objectForKey:`. Be careful not to confuse this with `setValue:forKey:`; that's for a completely different thing, [Key Value Coding](http://stackoverflow.com/documentation/objective-c/556/key-value-coding-key-value-observing#t=201607220233420459234)

### Shorthand

```objectivec
cars[@"Lamborghini"] = lamborghini;

```

This is the syntax that you use for dictionaries in most other languages, such as C#, Java, and Javascript. It's much more convenient than the standard syntax, and arguably more readable (especially if you code in these other languages), but of course, it isn't **standard**. It's also only available in newer versions of Objective C



## Getting a Value from NSDictionary


There are multiple ways to get an object from an NSDictionary with a key. For instance, to get a lamborghini from a list of cars

### Standard

```objectivec
Car * lamborghini = [cars objectForKey:@"Lamborghini"];

```

Just like any other object, call the method of NSDictionary that gives you an object for a key, `objectForKey:`. Be careful not to confuse this with `valueForKey:`; that's for a completely different thing, [Key Value Coding](http://stackoverflow.com/documentation/objective-c/556/key-value-coding-key-value-observing#t=201607220233420459234)

### Shorthand

```objectivec
Car * lamborghini = cars[@"Lamborghini"];

```

This is the syntax that you use for dictionaries in most other languages, such as C#, Java, and Javascript. It's much more convenient than the standard syntax, and arguably more readable (especially if you code in these other languages), but of course, it isn't **standard**. It's also only available in newer versions of Objective C



## Check if NSDictionary already has a key or not


Objective c:

```

  //this is the dictionary you start with. 
 NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:@"name1", @"Sam",@"name2", @"Sanju",nil];

//check if the dictionary contains the key you are going to modify.  In this example, @"Sam"
if (dict[@"name1"] != nil) {
    //there is an entry for Key name1       
}
else {
    //There is no entry for name1       
}

```



#### Syntax


<li>
@{ key: value, ... }
</li>
<li>
[NSDictionary dictionaryWithObjectsAndKeys: value, key, ..., nil];
</li>
<li>
dict[**key**] = **value**;
</li>
<li>
id **value** = dict[**key**];
</li>



#### Remarks


The NSDictionary class declares the programmatic interface to objects that manage immutable associations of keys and values. Use this class or its subclass NSMutableDictionary when you need a convenient and efficient way to retrieve data associated with an arbitrary key. NSDictionary creates static dictionaries, and NSMutableDictionary creates dynamic dictionaries. (For convenience, the term dictionary refers to any instance of one of these classes without specifying its exact class membership.)

A key-value pair within a dictionary is called an entry. Each entry consists of one object that represents the key and a second object that is that key’s value. Within a dictionary, the keys are unique. That is, no two keys in a single dictionary are equal (as determined by isEqual:). In general, a key can be any object (provided that it conforms to the NSCopying protocol—see below), but note that when using key-value coding the key must be a string (see Key-Value Coding Fundamentals). Neither a key nor a value can be nil; if you need to represent a null value in a dictionary, you should use NSNull.

NSDictionary is “toll-free bridged” with its Core Foundation counterpart, CFDictionaryRef. See Toll-Free Bridging for more information on toll-free bridging.

