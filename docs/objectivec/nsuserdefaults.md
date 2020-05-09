---
metaTitle: "Objective-C - NSUserDefaults"
description: "Simple example, Clear NSUserDefaults"
---

# NSUserDefaults




## Simple example


For example:

FOR SAVING:

```

 NSUserDefaults *prefs = [NSUserDefaults standardUserDefaults];

  // saving an NSString
  [prefs setObject:txtUsername.text forKey:@"userName"];
  [prefs setObject:txtPassword.text forKey:@"password"];

  [prefs synchronize];

```

FOR RETRIEVING

```

 NSUserDefaults *prefs = [NSUserDefaults standardUserDefaults];

  // getting an NSString
 NSString *savedUsername = [prefs stringForKey:@"userName"];
 NSString *savedPassword = [prefs stringForKey:@"password"];

```



## Clear NSUserDefaults


```objc
NSString *appDomain = [[NSBundle mainBundle] bundleIdentifier];
[[NSUserDefaults standardUserDefaults] removePersistentDomainForName:appDomain];

```

