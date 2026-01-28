---
metaTitle: "iOS - plist iOS"
description: "Example:, Save and edit/delete data from Plist"
---

# plist iOS




## Example:


**1. Static data to be used in app.**

To save static data in plist follow these methods:

**a) Add a new file**

[<img src="https://i.stack.imgur.com/mGHnw.png" alt="enter image description here" />](https://i.stack.imgur.com/mGHnw.png)

**b) Click Property list in Resources**

[<img src="https://i.stack.imgur.com/XlN4t.png" alt="enter image description here" />](https://i.stack.imgur.com/XlN4t.png)

**c) Name the propertylist and a file will be created as(data.plist here)**

[<img src="https://i.stack.imgur.com/wr0El.png" alt="enter image description here" />](https://i.stack.imgur.com/wr0El.png)

d) You can create a plist of Arrays and Dictionaries as:

[<img src="https://i.stack.imgur.com/pfxfW.png" alt="enter image description here" />](https://i.stack.imgur.com/pfxfW.png)

// Read plist from bundle and get Root Dictionary out of it

```swift
NSDictionary *dictRoot = [NSDictionary dictionaryWithContentsOfFile:[[NSBundle mainBundle] pathForResource:@"Data" ofType:@"plist"]];

```

// Your dictionary contains an array of dictionary
// Now pull an Array out of it.

```swift
NSArray *arrayList = [NSArray arrayWithArray:[dictRoot objectForKey:@"Object1"]];

for(int i=0; i< [arrayList count]; i++)
{
    NSMutableDictionary *details=[arrayList objectAtIndex:i];
}

```



## Save and edit/delete data from Plist


You have already created a plist. This plist will remain same in app. If you want to edit the data in this plist, add new data in plist or remove data from plist, you can't make changes in this file.

For this purpose you will have to store your plist in Document Directory. You can edit your plist saved in document directory.

**Save plist in document directory as:**

```swift
NSString *filePath = [[NSBundle mainBundle] pathForResource:@"Data" ofType:@"plist"];

NSDictionary *dict = [[NSDictionary alloc] initWithContentsOfFile:filePath];

NSDictionary *plistDict = dict;

NSFileManager *fileManager = [NSFileManager defaultManager];

NSString *error = nil;

NSData *plistData = [NSPropertyListSerialization dataFromPropertyList:plistDict format:NSPropertyListXMLFormat_v1_0 errorDescription:&error];

if (![fileManager fileExistsAtPath: plistPath]) {
    
    if(plistData)
    {
        [plistData writeToFile:plistPath atomically:YES];
    }
}
else
{
     
}

```

**Retreive data from Plist as:**

```

   NSArray *paths = NSSearchPathForDirectoriesInDomains (NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsPath = [paths objectAtIndex:0];
    NSString *plistPath = [documentsPath stringByAppendingPathComponent:@"Data.plist"];
    NSDictionary *dict = [[NSDictionary alloc] initWithContentsOfFile:plistPath];
    
    NSArray *usersArray = [dict objectForKey:@"Object1"];

```

You can edit remove, add new data as per your requirement and save the plist again to Document Directory.

