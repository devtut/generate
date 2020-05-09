---
metaTitle: "iOS - NSData"
description: "Converting NSData to HEX string, Creating NSData objects, Converting NSData to other types"
---

# NSData



## Converting NSData to HEX string


`NSData` can be represented as hexadecimal string, similar to what it outputs in its `description` method.

### Swift

```swift
extension NSData {

    func hexString() -> String {
        return UnsafeBufferPointer<UInt8>(start: UnsafePointer<UInt8>(bytes), count: length)
            .reduce("") { $0 + String(format: "%02x", $1) }
    }

}

```

### Objective-C

```swift
@implementation NSData (HexRepresentation)

- (NSString *)hexString {
    const unsigned char *bytes = (const unsigned char *)self.bytes;
    NSMutableString *hex = [NSMutableString new];
    for (NSInteger i = 0; i < self.length; i++) {
        [hex appendFormat:@"%02x", bytes[i]];
    }
    return [hex copy];
}

@end

```



## Creating NSData objects


### Using a file

### Swift

```swift
let data = NSData(contentsOfFile: filePath) //assuming filePath is a valid path

```

### Objective-C

```swift
NSData *data = [NSData dataWithContentsOfFile:filePath]; //assuming filePath is a valid path

```

### Using a String object

### Swift

```swift
let data = (string as NSString).dataUsingEncoding(NSUTF8StringEncoding) //assuming string is a String object

```

### Objective-C

```swift
NSData *data = [string dataUsingEncoding:NSUTF8StringEncoding]; //assuming string is a String object

```



## Converting NSData to other types


### To String

### Swift

```swift
let string = String(NSString(data: data, encoding: NSUTF8StringEncoding)) //assuming data is a valid NSData object

```

### Objective-C

```swift
NSString *string = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]; //assuming data is a valid NSData object
[string release];

```

### To Array

### Swift

```swift
let array = data.bytes as! NSMutableArray //assuming data is a valid NSData object

```

### Objective-C

```swift
NSMutableArray *array = (NSMutableArray *)[data bytes]; //assuming data is a valid NSData object

```

### To Bytes Array

### Swift

```swift
let bytesArray = data.bytes as! UInt8 //assuming data is a valid NSData object

```

### Objective-C

```swift
UInt8 *bytesArray = (UInt8 *)data.bytes; //assuming data is a valid NSData object

```



#### Remarks


### Useful Resources

[Apple Documentation (NSData)](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSData_Class/)

[NSData.dataWithContentsOfFile()](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSData_Class/#//apple_ref/occ/clm/NSData/dataWithContentsOfFile:)

[NSData.bytes](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSData_Class/#//apple_ref/occ/instp/NSData/bytes)

