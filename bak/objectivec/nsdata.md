---
metaTitle: "Objective C - NSData"
description: "Create, Get NSData lengh, Encoding and decoding a string using NSData Base64, NSData and Hexadecimal String"
---

# NSData



## Create


**From NSString:**

```objectivec
NSString *str = @"Hello world";
NSData *data = [str dataUsingEncoding:NSUTF8StringEncoding];

```

**From Int:**

```objectivec
int i = 1;
NSData *data = [NSData dataWithBytes: &i length: sizeof(i)];

```

**You can also use the following methods:**

```objectivec
+ dataWithContentsOfURL:
+ dataWithContentsOfURL:options:error:
+ dataWithData:
- initWithBase64EncodedData:options:
- initWithBase64EncodedString:options:
- initWithBase64Encoding:
- initWithBytesNoCopy:length:
- initWithBytesNoCopy:length:deallocator:
- initWithBytesNoCopy:length:freeWhenDone:
- initWithContentsOfFile:
- initWithContentsOfFile:options:error:
- initWithContentsOfMappedFile:
- initWithContentsOfURL:
- initWithContentsOfURL:options:error:
- initWithData:

```



## Get NSData lengh


```objectivec
NSString *filePath = [[NSFileManager defaultManager] pathForRessorce: @"data" ofType:@"txt"];
NSData *data = [NSData dataWithContentsOfFile:filePath]; 
int len = [data length]; 

```



## Encoding and decoding a string using NSData Base64


Encoding

```

//Create a Base64 Encoded NSString Object
 NSData *nsdata = [@"iOS Developer Tips encoded in Base64" dataUsingEncoding:NSUTF8StringEncoding];

 // Get NSString from NSData object in Base64
 NSString *base64Encoded = [nsdata base64EncodedStringWithOptions:0];
 // Print the Base64 encoded string
 NSLog(@"Encoded: %@", base64Encoded);

```

Decoding:

```objectivec
// NSData from the Base64 encoded str
NSData *nsdataFromBase64String = [[NSData alloc]initWithBase64EncodedString:base64Encoded options:0];

// Decoded NSString from the NSData
NSString *base64Decoded = [[NSString alloc] initWithData:nsdataFromBase64String encoding:NSUTF8StringEncoding];
NSLog(@"Decoded: %@", base64Decoded);

```



## NSData and Hexadecimal String


**Get NSData from Hexadecimal String**

```objectivec
+ (NSData *)dataFromHexString:(NSString *)string
{
    string = [string lowercaseString];
    NSMutableData *data= [NSMutableData new];
    unsigned char whole_byte;
    char byte_chars[3] = {'\0','\0','\0'};
    int i = 0;
    int length = (int) string.length;
    while (i < length-1) {
        char c = [string characterAtIndex:i++];
        if (c < '0' || (c > '9' && c < 'a') || c > 'f')
            continue;
        byte_chars[0] = c;
        byte_chars[1] = [string characterAtIndex:i++];
        whole_byte = strtol(byte_chars, NULL, 16);
        [data appendBytes:&whole_byte length:1];
    }
    return data;
}

```

**Get Hexadecimal String from data:**

```objectivec
+ (NSString *)hexStringForData:(NSData *)data
{
    if (data == nil) {
        return nil;
    }
    
    NSMutableString *hexString = [NSMutableString string];
    
    const unsigned char *p = [data bytes];
    
    for (int i=0; i < [data length]; i++) {
        [hexString appendFormat:@"%02x", *p++];
    }
    
    return hexString;
}

```

