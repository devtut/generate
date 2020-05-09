---
metaTitle: "iOS - NSUserDefaults"
description: "Setting values, Use Managers to Save and Read Data, UserDefaults uses in Swift 3, Getting Default Values, Saving Values, Clearing NSUserDefaults"
---

# NSUserDefaults



## Setting values


To set a value in `NSUserDefaults`, you can use the following functions:

### Swift < 3

```swift
setBool(_:forKey:)
setFloat(_:forKey:)
setInteger(_:forKey:)
setObject(_:forKey:)
setDouble(_:forKey:)
setURL(_:forKey:)

```

### Swift 3

In Swift 3 the names of function is changed to `set` insted of `set` folloed by the type.

```swift
set(_:forKey:) 

```

### Objective-C

```swift
-(void)setBool:(BOOL)value forKey:(nonnull NSString *)defaultName;
-(void)setFloat:(float)value forKey:(nonnull NSString *)defaultName;
-(void)setInteger:(NSInteger)value forKey:(nonnull NSString *)defaultName;
-(void)setObject:(nullable id)value forKey:(nonnull NSString *)defaultName;
-(void)setDouble:(double)value forKey:(nonnull NSString *)defaultName;
-(void)setURL:(nullable NSURL *)value forKey:(nonnull NSString *)defaultName;

```

Example usage would be:

### Swift < 3

```swift
NSUserDefaults.standardUserDefaults.setObject("Netherlands", forKey: "HomeCountry")

```

### Swift 3

```swift
UserDefaults.standard.set("Netherlands", forKey: "HomeCountry")

```

### Objective-C

```swift
[[NSUserDefaults standardUserDefaults] setObject:@"Netherlands" forKey:@"HomeCountry"];

```

###  Custom objects 

### Swift

```swift
public func encodeWithCoder(aCoder: NSCoder) {
    aCoder.encodeObject(name, forKey:"name")
    aCoder.encodeObject(unitId, forKey: "unitId")
}
    
required public init(coder aDecoder: NSCoder) {
    super.init()
    name = aDecoder.decodeObjectForKey("name") as? String
    unitId = aDecoder.decodeIntegerForKey("unitId") as? NSInteger
}

```

### Objective-C

```swift
- (id)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        name = [coder decodeObjectForKey:@"name"];
        unitId = [coder decodeIntegerForKey:@"unitId"];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder*)coder {
    [coder encodeObject:name forKey:@"name"];
    [coder encodeInteger:unitId forKey:@"unitId"];
}

```



## Use Managers to Save and Read Data


While you can use the `NSUserDefaults` methods anywhere, it can sometimes be better to define a manager that saves and reads from `NSUserDefaults` for you and then use that manager for reading or writing your data.

Suppose that we want to save a user’s score into `NSUserDefaults`.  We can create a class like the one below that has at two methods: `setHighScore` and `highScore`. Anywhere you want to access the high scores, create an instance of this class.

### Swift

```swift
public class ScoreManager: NSObject {

    let highScoreDefaultKey = "HighScoreDefaultKey"

    var highScore = {
        set {
            // This method includes your implementation for saving the high score
            // You can use NSUserDefaults or any other data store like CoreData or
            // SQLite etc.
    
            NSUserDefaults.standardUserDefaults().setInteger(newValue, forKey: highScoreDefaultKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
        get {
        //This method includes your implementation for reading the high score

            let score = NSUserDefaults.standardUserDefaults().objectForKey(highScoreDefaultKey)
    
            if (score != nil) {
                return score.integerValue;
            } else {
                //No high score available, so return -1
                return -1;
            }
        }
    }
}

```

### Objective-C

```swift
#import "ScoreManager.h"

#define HIGHSCRORE_KEY @"highScore"

@implementation ScoreManager

- (void)setHighScore:(NSUInteger) highScore {
    // This method includes your implementation for saving the high score
    // You can use NSUserDefaults or any other data store like CoreData or
    // SQLite etc.

    [[NSUserDefaults standardUserDefaults] setInteger:highScore forKey:HIGHSCRORE_KEY];
    [[NSUserDefaults standardUserDefaults] synchronize];
}

- (NSInteger)highScore
{

    //This method includes your implementation for reading the high score

    NSNumber *highScore = [[NSUserDefaults standardUserDefaults] objectForKey:HIGHSCRORE_KEY];
    if (highScore) {
        return highScore.integerValue;
    }else
    {
        //No high score available, so return -1

        return -1;
    }

}

@end

```

The advantages are that:

<li>
The implementation of your read and write process is only in one place and you can change it (for example switch from `NSUserDefaults` to Core Data) whenever you want and not worry about changing all places that you are working with the high score.
</li>
<li>
Simply call only one method when you want to access to score or write it.
</li>
<li>
Simply debug it when you see a bug or something like this.
</li>

> 
<h3>Note</h3>
If you are worried about synchronization, it is better to use a singleton class that manages the synchronization.




## UserDefaults uses in Swift 3


**Every application needed to store User Session or User related details inside application in UserDefaults.So we made whole logic inside a Class for managing UserDefaults better way.**

**Swift 3**

```swift
import Foundation

public struct Session {

    fileprivate static let defaults = UserDefaults.standard

    enum userValues: String {
        case auth_token
        case email
        case fname
        case mobile
        case title
        case userId
        case userType
        case OTP
        case isApproved
    }
    
    
    //MARK: - Getting here User Details
    static func getUserSessionDetails()->[String:AnyObject]? {
        let dictionary = defaults.object(forKey: "LoginSession") as? [String:AnyObject]
        return  dictionary
    }
    
    //MARK: - Saving Device Token
    static func saveDeviceToken(_ token:String){
        guard (gettingDeviceToken() ?? "").isEmpty else {
            return
        }
        defaults.removeObject(forKey: "deviceToken")
        defaults.set(token, forKey: "deviceToken")
        defaults.synchronize()
    }
    
    //MARK: - Getting Token here
    static func gettingDeviceToken()->String?{
        let token = defaults.object(forKey: "deviceToken") as? String
        if token == nil{
            return ""
        }else{ return token}
    }
    
    //MARK: - Setting here User Details
    static func setUserSessionDetails(_ dic :[String : AnyObject]){
        defaults.removeObject(forKey: "LoginSession")
        defaults.set(dic, forKey: "LoginSession")
        defaults.synchronize()
    }
    
    //MARK:- Removing here all Default Values
    static func userSessionLogout(){
        //Set Activity
        defaults.removeObject(forKey: "LoginSession")
        defaults.synchronize()
    }
    
    //MARK: - Get value from session here
    static func getUserValues(value: userValues) -> String? {
        let dic = getUserSessionDetails() ?? [:]
        guard let value = dic[value.rawValue] else{
            return ""
        }
        return value as? String
    }
    
}

```

**Use of UserDefaults Class**

```swift
//Saving user Details
Session.setUserSessionDetails(json ?? [:])

//Retriving user Details 
let userId = Session.getUserValues(value: .userId) ?? ""

```



## Getting Default Values


To get a value in NSUserDefaults you can use the following functions:

### Swift

```swift
arrayForKey(_:)
boolForKey(_:)
dataForKey(_:)
dictionaryForKey(_:)
floatForKey(_:)
integerForKey(_:)
objectForKey(_:)
stringArrayForKey(_:)
stringForKey(_:)
doubleForKey(_:)
URLForKey(_:)

```

### Objective-C

```swift
-(nullable NSArray *)arrayForKey:(nonnull NSString *)defaultName;
-(BOOL)boolForKey:(nonnull NSString *)defaultName;
-(nullable NSData *)dataForKey:(nonnull NSString *)defaultName;
-(nullable NSDictionary<NSString *, id> *)dictionaryForKey:(nonnull NSString *)defaultName;
-(float)floatForKey:(nonnull NSString *)defaultName;
-(NSInteger)integerForKey:(nonnull NSString *)defaultName;
-(nullable id)objectForKey:(nonnull NSString *)key;
-(nullable NSArray<NSString *> *)stringArrayForKey:(nonnull NSString *)defaultName;
-(nullable NSString *)stringForKey:(nonnull NSString *)defaultName;
-(double)doubleForKey:(nonnull NSString *)defaultName;
-(nullable NSURL *)URLForKey:(nonnull NSString *)defaultName;

```

Example usage would be:

### Swift

```swift
let homeCountry = NSUserDefaults.standardUserDefaults().stringForKey("HomeCountry")

```

### Objective-C

```swift
NSString *homeCountry = [[NSUserDefaults standardUserDefaults] stringForKey:@"HomeCountry"];

```



## Saving Values


`NSUserDefaults` are written to disk periodically by the system, but there are times when you want your changes saved immediately, such as when the app transitions into background state. This is done by calling `synchronize`.

### Swift

```swift
NSUserDefaults.standardUserDefaults().synchronize()

```

### Objective-C

```swift
[[NSUserDefaults standardUserDefaults] synchronize];

```



## Clearing NSUserDefaults


### Swift

```swift
let bundleIdentifier = NSBundle.mainBundle().bundleIdentifier()

NSUserDefaults.standardUserDefaults().removePersistentDomainForName(bundleIdentifier)

```

### Objective-C

```swift
NSString *bundleIdentifier = [[NSBundle mainBundle] bundleIdentifier];

[[NSUserDefaults standardUserDefaults] removePersistentDomainForName: bundleIdentifier];

```



#### Syntax


<li>
`UserDefaults.standard.set(dic, forKey: "LoginSession") //Save value inside userdefaults`
<ul>
- `UserDefaults.standard.object(forKey: "LoginSession") as? [String:AnyObject] ?? [:] //Get value from UserDefaults`



#### Remarks


NSUserDefault which are used to store all type of DataType, and you can get its value anywhere in the class of app. [**NSUserDefault**](http://iosdevcenters.blogspot.com/2016/05/save-data-using-nsuserdefaults-in-swift.html)

