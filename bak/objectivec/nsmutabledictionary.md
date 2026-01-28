---
metaTitle: "Objective C - NSMutableDictionary"
description: "NSMutableDictionary Example, Removing Entries From a Mutable Dictionary"
---

# NSMutableDictionary



## NSMutableDictionary Example


**+ dictionaryWithCapacity:**

Creates and returns a mutable dictionary, initially giving it enough allocated memory to hold a given number of entries.

```objectivec
NSMutableDictionary *dict =  [NSMutableDictionary dictionaryWithCapacity:1];
NSLog(@"%@",dict);

```

**- init**

Initializes a newly allocated mutable dictionary.

```objectivec
NSMutableDictionary *dict =  [[NSMutableDictionary alloc] init];        
NSLog(@"%@",dict);

```

**+ dictionaryWithSharedKeySet:**

Creates a mutable dictionary which is optimized for dealing with a known set of keys.

```objectivec
id sharedKeySet = [NSDictionary sharedKeySetForKeys:@[@"key1", @"key2"]]; // returns NSSharedKeySet
NSMutableDictionary *dict = [NSMutableDictionary dictionaryWithSharedKeySet:sharedKeySet];
dict[@"key1"] = @"Easy";
dict[@"key2"] = @"Tutorial";
//We can an object thats not in the shared keyset
dict[@"key3"] = @"Website";
NSLog(@"%@",dict);  

```

> 
OUTPUT


```objectivec
{
    key1 = Eezy;
    key2 = Tutorials;
    key3 = Website;
}

```

Adding Entries to a Mutable Dictionary

**- setObject:forKey:**

Adds a given key-value pair to the dictionary.

```objectivec
NSMutableDictionary *dict =  [NSMutableDictionary dictionary];
[dict setObject:@"Easy" forKey:@"Key1"];
NSLog(@"%@",dict);

```

> 
OUTPUT


```objectivec
{
    Key1 = Eezy;
}

```

**- setObject:forKeyedSubscript:**

Adds a given key-value pair to the dictionary.

```objectivec
NSMutableDictionary *dict =  [NSMutableDictionary dictionary];
[dict setObject:@"Easy" forKeyedSubscript:@"Key1"];
NSLog(@"%@",dict);  

```

> 
<p>OUTPUT
{
Key1 = Easy;
}</p>


    



## Removing Entries From a Mutable Dictionary


**- removeObjectForKey:**

Removes a given key and its associated value from the dictionary.

```objectivec
NSMutableDictionary *dict =  [NSMutableDictionary dictionaryWithDictionary:@{@"key1":@"Easy",@"key2": @"Tutorials"}];
[dict removeObjectForKey:@"key1"];
NSLog(@"%@",dict);

```

> 
OUTPUT


```objectivec
{
    key2 = Tutorials;
} 

```

**- removeAllObjects**

Empties the dictionary of its entries.

```objectivec
NSMutableDictionary *dict =  [NSMutableDictionary dictionaryWithDictionary:@{@"key1":@"Eezy",@"key2": @"Tutorials"}];
[dict removeAllObjects];
NSLog(@"%@",dict);

```

> 
OUTPUT


```objectivec
{
}

```

**- removeObjectsForKeys:**

Removes from the dictionary entries specified by elements in a given array.

```objectivec
NSMutableDictionary *dict =  [NSMutableDictionary dictionaryWithDictionary:@{@"key1":@"Easy",@"key2": @"Tutorials"}];
[dict removeObjectsForKeys:@[@"key1"]];
NSLog(@"%@",dict);

```

> 
OUTPUT


```objectivec
{
    key2 = Tutorials;
}

```



#### Parameters


|objects|keys
|---|---|---|---|---|---|---|---|---|---
|An array containing the values for the new dictionary.|CellAn array containing the keys for the new dictionary. Each key is copied and the copy is added to the dictionary.

