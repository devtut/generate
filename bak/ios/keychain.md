---
metaTitle: "iOS - Keychain"
description: "Adding a Password to the Keychain, Keychain Access Control (TouchID with password fallback), Finding a Password in the Keychain, Updating a Password in the Keychain, Removing a Password from the Keychain, Keychain Add, Update, Remove and Find operations using one file."
---

# Keychain



## Adding a Password to the Keychain


Every Keychain Item is most often represented as a `CFDictionary`. You can, however, simply use `NSDictionary` in Objective-C and take advantage of bridging, or in Swift you may use `Dictionary` and explicitly cast to `CFDictionary`.

You could construct a password with the following dictionary:

### Swift

```swift
var dict = [String : AnyObject]()

```

First, you need a key/value pair that lets the Keychain know this is a password. Note that because our dict key is a `String` we must cast any `CFString` to a `String` explicitly in Swift 3. CFString may not be used as the key to a Swift Dictionary because it is not Hashable.

### Swift

```swift
dict[kSecClass as String] = kSecClassGenericPassword

```

Next, our password may have a series of attributes to describe it and help us find it later. [Here's a list of attributes for generic passwords](https://developer.apple.com/reference/security/ksecclasskey).

### Swift

```swift
// The password will only be accessible when the device is unlocked
dict[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlocked
// Label may help you find it later
dict[kSecAttrLabel as String] = "com.me.myapp.myaccountpassword" as CFString
// Username
dict[kSecAttrAccount as String] = "My Name" as CFString
// Service name
dict[kSecAttrService as String] = "MyService" as CFString

```

Finally, we need our actual private data. Be sure not to keep this around in memory for too long. This must be `CFData`.

### Swift

```swift
dict[kSecValueData as String] = "my_password!!".data(using: .utf8) as! CFData

```

Finally, the Keychain Services add function wants to know how it should return the newly constructed keychain item. Since you shouldn't be holding on to the data very long in memory, here's how you could only return the attributes:

### Swift

```swift
dict[kSecReturnAttributes as String] = kCFBooleanTrue

```

Now we have constructed our item. Let's add it:

### Swift

```swift
var result: AnyObject?
let status = withUnsafeMutablePointer(to: &result) {
    SecItemAdd(dict as CFDictionary, UnsafeMutablePointer($0))
}
let newAttributes = result as! Dictionary<String, AnyObject>

```

This places the new attributes dict inside `result`. `SecItemAdd` takes in the dictionary we constructed, as well as a pointer to where we would like our result. The function then returns an `OSStatus` indicating success or an error code. Result codes are described [here](https://developer.apple.com/reference/security/1658642-keychain_services#1662444).



## Keychain Access Control (TouchID with password fallback)


Keychain allows to save items with special SecAccessControl attribute which will allow to get item from Keychain only after user will be authenticated with Touch ID (or passcode if such fallback is allowed). App is only notified whether the authentication was successful or not, whole UI is managed by iOS.

First, SecAccessControl object should be created:

### Swift

```swift
let error: Unmanaged<CFError>?

guard let accessControl = SecAccessControlCreateWithFlags(kCFAllocatorDefault, kSecAttrAccessibleWhenPasscodeSetThisDeviceOnly, .userPresence, &error) else {
    fatalError("Something went wrong")
}

```

Next, add it to the dictionary with kSecAttrAccessControl key (which is mutually exclusive with kSecAttrAccessible key you've been using in other examples):

### Swift

```swift
var dictionary = [String : Any]()

dictionary[kSecClass as String] = kSecClassGenericPassword
dictionary[kSecAttrLabel as String] = "com.me.myapp.myaccountpassword" as CFString
dictionary[kSecAttrAccount as String] = "My Name" as CFString
dictionary[kSecValueData as String] = "new_password!!".data(using: .utf8) as! CFData
dictionary[kSecAttrAccessControl as String] = accessControl

```

And save it as you've done before:

### Swift

```swift
let lastResultCode = SecItemAdd(query as CFDictionary, nil)

```

To access stored data, just query Keychain for a key. Keychain Services will present authentication dialog to the user and return data or nil depending on whether suitable fingerprint was provided or passcode matched.

Optionally, prompt string can be specified:

### Swift

```swift
var query = [String: Any]()

query[kSecClass as String] = kSecClassGenericPassword
query[kSecReturnData as String] = kCFBooleanTrue
query[kSecAttrAccount as String] = "My Name" as CFString
query[kSecAttrLabel as String] = "com.me.myapp.myaccountpassword" as CFString
query[kSecUseOperationPrompt as String] = "Please put your fingers on that button" as CFString

var queryResult: AnyObject?
let status = withUnsafeMutablePointer(to: &queryResult) {
    SecItemCopyMatching(query as CFDictionary, UnsafeMutablePointer($0))
}

```

Pay attention that `status` will be `err` if user declined, canceled or failed authorization.

### Swift

```swift
if status == noErr {
    let password = String(data: queryResult as! Data, encoding: .utf8)!
    print("Password: \(password)")
} else {
    print("Authorization not passed")
}

```



## Finding a Password in the Keychain


To construct a query, we need to represent it as a `CFDictionary`. You may also use `NSDictionary` in Objective-C or `Dictionary` in Swift and cast to `CFDictionary`.

We need a class key:

### Swift

```swift
var dict = [String : AnyObject]()
dict[kSecClass as String] = kSecClassGenericPassword

```

Next, we can specify attributes to narrow down our search:

### Swift

```swift
// Label
dict[kSecAttrLabel as String] = "com.me.myapp.myaccountpassword" as CFString
// Username
dict[kSecAttrAccount as String] = "My Name" as CFString
// Service name
dict[kSecAttrService as String] = "MyService" as CFString

```

We can also specify special search modifier keys described [here](https://developer.apple.com/reference/security/1658642-keychain_services/1663699-search_attribute_keys).

Finally, we need to say how we'd like our data returned. Below, we'll request that just the private password itself be returned as a `CFData` object:

### Swift

```swift
dict[kSecReturnData as String] = kCFBooleanTrue

```

Now, let's search:

### Swift

```swift
var queryResult: AnyObject?
let status = withUnsafeMutablePointer(to: &queryResult) {
    SecItemCopyMatching(dict as CFDictionary, UnsafeMutablePointer($0))
}
// Don't keep this in memory for long!!
let password = String(data: queryResult as! Data, encoding: .utf8)!

```

Here, `SecItemCopyMatching` takes in a query dictionary and a pointer to where you'd like the result to go. It returns an `OSStatus` with a result codes. [Here](https://developer.apple.com/reference/security/1658642-keychain_services#1662444) are the possibilities.



## Updating a Password in the Keychain


As usual, we first need a `CFDictionary` to represent the item we want to update. This must contain all of the old values for the item, including the old private data. Then it takes a `CFDictionary` of any attributes or the data itself that you would like to change.

So first, let's construct a class key and a list of attributes. These attributes can narrow our search but you must include any attributes and there old values if you will be changing them.

### Swift

```swift
var dict = [String : AnyObject]()
dict[kSecClass as String] = kSecClassGenericPassword
// Label
dict[kSecAttrLabel as String] = "com.me.myapp.myaccountpassword" as CFString
// Username
dict[kSecAttrAccount as String] = "My Name" as CFString

```

Now we must add the old data:

### Swift

```swift
dict[kSecValueData as String] = "my_password!!".data(using: .utf8) as! CFData

```

Now let's create the same attributes but a different password:

### Swift

```swift
var newDict = [String : AnyObject]()
newDict[kSecClass as String] = kSecClassGenericPassword
// Label
newDict[kSecAttrLabel as String] = "com.me.myapp.myaccountpassword" as CFString
// Username
newDict[kSecAttrAccount as String] = "My Name" as CFString
// New password
newDict[kSecValueData as String] = "new_password!!".data(using: .utf8) as! CFData

```

Now, we just pass it to Keychain Services:

### Swift

```swift
let status = SecItemUpdate(dict as CFDictionary, newDict as CFDictionary)

```

`SecItemUpdate` returns a status code. Results are described [here](https://developer.apple.com/reference/security/1658642-keychain_services#1662444).



## Removing a Password from the Keychain


We need only one thing in order to delete an item from the Keychain: a `CFDictionary` with attributes describing the items to be deleted. Any items that match the query dictionary will be deleted permanently, so if you are only intending to delete a single item be sure to be specific with your query. As always, we can use an `NSDictionary` in Objective-C or in Swift we can use a `Dictionary` and then cast to `CFDictionary`.

A query dictionary, in this context exclusively includes a class key to describe what the item is and attributes to describe information about the item. Inclusion of search restrictions such as `kSecMatchCaseInsensitive` is not allowed.

### Swift

```swift
var dict = [String : AnyObject]()
dict[kSecClass as String] = kSecClassGenericPassword
// Label
dict[kSecAttrLabel as String] = "com.me.myapp.myaccountpassword" as CFString
// Username
dict[kSecAttrAccount as String] = "My Name" as CFString

```

And now we can simply remove it:

### Swift

```swift
let status = SecItemDelete(dict as CFDictionary)

```

`SecItemDelete` returns an `OSStatus`. Result codes are described [here](https://developer.apple.com/reference/security/1658642-keychain_services#1662444).



## Keychain Add, Update, Remove and Find operations using one file.


**Keychain.h**

```swift
#import <Foundation/Foundation.h>
typedef void (^KeychainOperationBlock)(BOOL successfulOperation, NSData *data, OSStatus status);

@interface Keychain : NSObject

-(id) initWithService:(NSString *) service_ withGroup:(NSString*)group_;

-(void)insertKey:(NSString *)key withData:(NSData *)data withCompletion:(KeychainOperationBlock)completionBlock;
-(void)updateKey:(NSString*)key withData:(NSData*) data withCompletion:(KeychainOperationBlock)completionBlock;
-(void)removeDataForKey:(NSString*)key withCompletionBlock:(KeychainOperationBlock)completionBlock;
-(void)findDataForKey:(NSString*)key withCompletionBlock:(KeychainOperationBlock)completionBlock;

@end

```

Keychain.m

```swift
#import "Keychain.h"
#import <Security/Security.h>

@implementation Keychain

{
    NSString * keychainService;
    NSString * keychainGroup;
}

-(id) initWithService:(NSString *)service withGroup:(NSString*)group
{
    self =[super init];
    if(self) {
        keychainService = [NSString stringWithString:service];
        if(group) {
            keychainGroup = [NSString stringWithString:group];
        }
    }
    
    return  self;
}

-(void)insertKey:(NSString *)key
        withData:(NSData *)data
  withCompletion:(KeychainOperationBlock)completionBlock
{
    NSMutableDictionary * dict =[self prepareDict:key];
    [dict setObject:data forKey:(__bridge id)kSecValueData];
    [dict setObject:keychainService forKey:(id)kSecAttrService];
    
    OSStatus status = SecItemAdd((__bridge CFDictionaryRef)dict, NULL);
    if(errSecSuccess != status) {
        DLog(@"Unable add item with key =%@ error:%d",key,(int)status);
        if (completionBlock) {
            completionBlock(errSecSuccess == status, nil, status);
        }
    }
    if (status == errSecDuplicateItem) {
        [self updateKey:key withData:data withCompletion:^(BOOL successfulOperation, NSData *updateData, OSStatus updateStatus) {
            if (completionBlock) {
                completionBlock(successfulOperation, updateData, updateStatus);
            }
            DLog(@"Found duplication item -- updating key with data");
        }];
    }
}

-(void)findDataForKey:(NSString *)key
  withCompletionBlock:(KeychainOperationBlock)completionBlock
{
    NSMutableDictionary *dict = [self prepareDict:key];
    [dict setObject:(__bridge id)kSecMatchLimitOne forKey:(__bridge id)kSecMatchLimit];
    [dict setObject:keychainService forKey:(id)kSecAttrService];
    [dict setObject:(id)kCFBooleanTrue forKey:(__bridge id)kSecReturnData];
    CFTypeRef result = NULL;
    OSStatus status = SecItemCopyMatching((__bridge CFDictionaryRef)dict,&result);
    
    if( status != errSecSuccess) {
        DLog(@"Unable to fetch item for key %@ with error:%d",key,(int)status);
        if (completionBlock) {
            completionBlock(errSecSuccess == status, nil, status);
        }
    } else {
        if (completionBlock) {
            completionBlock(errSecSuccess == status, (__bridge NSData *)result, status);
        }
    }
}

-(void)updateKey:(NSString *)key
        withData:(NSData *)data
  withCompletion:(KeychainOperationBlock)completionBlock
{
    NSMutableDictionary * dictKey =[self prepareDict:key];
    
    NSMutableDictionary * dictUpdate =[[NSMutableDictionary alloc] init];
    [dictUpdate setObject:data forKey:(__bridge id)kSecValueData];
    [dictUpdate setObject:keychainService forKey:(id)kSecAttrService];
    OSStatus status = SecItemUpdate((__bridge CFDictionaryRef)dictKey, (__bridge CFDictionaryRef)dictUpdate);
    if( status != errSecSuccess) {
        DLog(@"Unable to remove item for key %@ with error:%d",key,(int)status);
    }
    if (completionBlock) {
        completionBlock(errSecSuccess == status, nil, status);
    }
}

-(void)removeDataForKey:(NSString *)key
    withCompletionBlock:(KeychainOperationBlock)completionBlock {
    NSMutableDictionary *dict = [self prepareDict:key];
    OSStatus status = SecItemDelete((__bridge CFDictionaryRef)dict);
    if( status != errSecSuccess) {
        DLog(@"Unable to remove item for key %@ with error:%d",key,(int)status);
    }
    if (completionBlock) {
        completionBlock(errSecSuccess == status, nil, status);
    }
}

#pragma mark Internal methods

-(NSMutableDictionary*) prepareDict:(NSString *) key {
    
    NSMutableDictionary *dict = [[NSMutableDictionary alloc] init];
    [dict setObject:(__bridge id)kSecClassGenericPassword forKey:(__bridge id)kSecClass];
    
    NSData *encodedKey = [key dataUsingEncoding:NSUTF8StringEncoding];
    [dict setObject:encodedKey forKey:(__bridge id)kSecAttrGeneric];
    [dict setObject:encodedKey forKey:(__bridge id)kSecAttrAccount];
    [dict setObject:keychainService forKey:(__bridge id)kSecAttrService];
    [dict setObject:(__bridge id)kSecAttrAccessibleAlwaysThisDeviceOnly forKey:(__bridge id)kSecAttrAccessible];
    
    //This is for sharing data across apps
    if(keychainGroup != nil) {
        [dict setObject:keychainGroup forKey:(__bridge id)kSecAttrAccessGroup];
    }
    
    return  dict;
}

@end

```



#### Syntax


- kSecClassGenericPassword // A value key representing a non-internet password
- kSecClassInternetPassword // A value key representing an internet password
- kSecClassCertificate // A value key representing a certificate
- kSecClassCertificate // A value key representing a key
- kSecClassIdentity // A value key representing an identity, which is a certificate plus a key



#### Remarks


iOS stores private information such as passwords, encryption keys, certificates, and identities in a secure storage area called the Keychain. This storage area is managed completely by a co-processor called the Secure Enclave, which is embedded inside the application processor. Because the Keychain is sandboxed on iOS, keychain items may only be retrieved by the application that put them there in the first place.

In some cases you must turn on Keychain Sharing in Xcode capabilities in order to avoid errors.

In order to interact with the keychain, we use a c framework called Keychain Services. For more information, see [Apple's Keychain Services Programming Guide](https://developer.apple.com/library/content/documentation/Security/Conceptual/keychainServConcepts/01introduction/introduction.html#//apple_ref/doc/uid/TP30000897).

Because Keychain Services is below the `Foundation` level, it is restricted to using `CoreFoundation` types. As a result, most objects are internally represented as `CFDictionary`s holding `CFString`s as their keys and a variety of `CoreFoundation` types as their values.

While Keychain Services is included as a part of the `Security` framework, importing `Foundation` is usually a good option since it includes some helper functions in the backend.

Additionally, if you don't want to deal with Keychain Services directly, Apple provides the [Generic Keychain](https://developer.apple.com/library/content/samplecode/GenericKeychain/Introduction/Intro.html#//apple_ref/doc/uid/DTS40007797) Swift sample project that provides Swift types that use Keychain Services behind the scenes.

