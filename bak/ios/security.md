---
metaTitle: "iOS - Security"
description: "Securing Data in iTunes Backups, Transport Security using SSL"
---

# Security


Security in iOS is related to data security, transport security, code security, etc



## Securing Data in iTunes Backups


If we want our app data to be protected against iTunes backups, we have to skip our app data from being backed up in iTunes.<br />
Whenever iOS device backed up using iTunes on macOS, all the data stored by all the apps is copied in that backup and stored on backing computer.

But we can exclude our app data from this backup using `URLResourceKey.isExcludedFromBackupKey` key.<br />
Here is the directory structure of our app:
[<img src="https://i.stack.imgur.com/f5OKH.png" alt="enter image description here" />](https://i.stack.imgur.com/f5OKH.png)<br />
**Note:** Generally sensitive data is stored in 'Application Support' directory.

e.g. If we want to exclude all our data stored in **Application Support** directory then we can use above mentioned key as follow:

```

   let urls = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)
    let baseURL = urls[urls.count-1];
    
    let bundleIdentifier = Bundle.main.object(forInfoDictionaryKey: "CFBundleIdentifier") as! String
    let pathURL = baseURL.appendingPathComponent(bundleIdentifier)
    let persistentStoreDirectoryPath = pathURL.path
    if !FileManager.default.fileExists(atPath: persistentStoreDirectoryPath) {
        do {
            try FileManager.default.createDirectory(atPath: path, withIntermediateDirectories: true, attributes: nil)  
        }catch {
            //handle error
        }
    }
    let dirURL = URL.init(fileURLWithPath: persistentStoreDirectoryPath, isDirectory: true)
    do {
        try (dirURL as NSURL).setResourceValue((true), forKey: .isExcludedFromBackupKey)
    } catch {
        //handle error
    }

```

There are lots of tools available to see iTunes backups for all the backed up data to confirm whether above approach works or not.<br />
[iExplorer](https://iexplorer.en.softonic.com) is good one to explore iTunes backups.



## Transport Security using SSL


iOS apps needs to be written in a way to provide security to data which is being transported over network.<br />
SSL is the common way to do it.

Whenever app tries to call web services to pull or push data to servers, it should **use SSL over HTTP, i.e. HTTPS**.<br />
To do this, **app must call `https://server.com/part`** such web services and not `http://server.com/part`.<br />
In this case, app needs to trust the server `server.com` using SSL certificate.

Here is the example of validating server trust-

Implement `URLSessionDelegate` as:

```swift
func urlSession(_ session: URLSession, didReceive challenge: URLAuthenticationChallenge, completionHandler: @escaping (URLSession.AuthChallengeDisposition, URLCredential?) -> Void) {
    
    if challenge.protectionSpace.authenticationMethod == NSURLAuthenticationMethodServerTrust {
        let serverTrust:SecTrust = challenge.protectionSpace.serverTrust!

        func acceptServerTrust() {
            let credential:URLCredential = URLCredential(trust: serverTrust)
            challenge.sender?.use(credential, for: challenge)
            completionHandler(.useCredential, URLCredential(trust: challenge.protectionSpace.serverTrust!))
        }
        
        let success = SSLTrustManager.shouldTrustServerTrust(serverTrust, forCert: "Server_Public_SSL_Cert")
        if success {
            acceptServerTrust()
            return
        }
    }
    else if challenge.protectionSpace.authenticationMethod == NSURLAuthenticationMethodClientCertificate {
        completionHandler(.rejectProtectionSpace, nil);
        return
    }
    completionHandler(.cancelAuthenticationChallenge, nil)
}

```

Here is trust manager: (couldn't found Swift code)

```swift
@implementation SSLTrustManager
+ (BOOL)shouldTrustServerTrust:(SecTrustRef)serverTrust forCert:(NSString*)certName {
// Load up the bundled certificate.
NSString *certPath = [[NSBundle mainBundle] pathForResource:certName ofType:@"der"];
NSData *certData = [[NSData alloc] initWithContentsOfFile:certPath];
CFDataRef certDataRef = (__bridge_retained CFDataRef)certData;
SecCertificateRef cert = SecCertificateCreateWithData(NULL, certDataRef);

// Establish a chain of trust anchored on our bundled certificate.
CFArrayRef certArrayRef = CFArrayCreate(NULL, (void *)&cert, 1, NULL);
SecTrustSetAnchorCertificates(serverTrust, certArrayRef);

// Verify that trust.
SecTrustResultType trustResult;
SecTrustEvaluate(serverTrust, &trustResult);

// Clean up.
CFRelease(certArrayRef);
CFRelease(cert);
CFRelease(certDataRef);

// Did our custom trust chain evaluate successfully?
return trustResult == kSecTrustResultUnspecified;
}
@end

```

**Server_Public_SSL_Cert.der** is servers' public SSL key.

Using this approach our app can make sure that it is communicating to the intended server and no one is intercepting the app-server communication.

