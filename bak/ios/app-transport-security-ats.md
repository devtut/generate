---
metaTitle: "iOS - App Transport Security (ATS)"
description: "Load all HTTP content, Selectively load HTTP content, Endpoints require SSL"
---

# App Transport Security (ATS)



## Load all HTTP content


Apple introduced ATS with iOS 9 as a new security feature to improve privacy and security between apps and web services. ATS by default fails all non HTTPS requests. While this can be really nice for production environments, it can be a nuisance during testing.

ATS is configured in the target's `Info.plist` file with the `NSAppTransportSecurity` dictionary (`App Transport Security Settings` in the Xcode Info.plist editor). To allow all HTTP content, add the `Allow Arbitrary Loads` boolean (`NSAllowsArbitraryLoads`) and set it to `YES`. This is not recommended for production apps, and if HTTP content is required, it is recommended that it be selectively enabled instead.



## Selectively load HTTP content


Similar to enabling all HTTP content, all configuration happens under the `App Transport Security Settings`. Add the `Exception Domains` dictionary (`NSExceptionDomains`) to the top level ATS settings.

For every domain, add a dictionary item to the Exception Domains, where the key is the domain in question. Set `NSExceptionAllowsInsecureHTTPLoads` to `YES` to disable the HTTPS requirement for that domain.



## Endpoints require SSL


Introduced in iOS 9, all endpoints must adhere to the HTTPS specification.<br />
Any endpoints not using SSL will fail with a warning in the console log. To your application it will appear that the internet connection failed.

To configure exceptions: Place the following in your Info.plist file:

1. Allow particular domain (testdomain.com) **only**:

```swift
<key>NSAppTransportSecurity</key>
<dict>
<key>NSExceptionDomains</key>
<dict>
    <key>testdomain.com</key>
    <dict>
        <key>NSIncludesSubdomains</key>
        <true/>
        <key>NSExceptionAllowsInsecureHTTPLoads</key>
        <true/>
    </dict>
</dict>

```

The key that permits such behavior is `NSExceptionAllowsInsecureHTTPLoads`. In this case, app will allow HTTP connection to mentioned domain (testdomain.com) only and block all other HTTP connections.

The key `NSIncludesSubdomains` specifies that any and all **subdomains** of the mentioned domain (testdomain.com) should also be allowed.

1. Allow any domain:

```swift
<key>NSAppTransportSecurity</key>
<dict>
    <key>NSAllowsArbitraryLoads</key>
    <true/>
</dict>

```

In this case, app will allow HTTP connection to **any** domain. As of January 1st 2017, using this flag will cause thorough App Store review and the app developers will have to explain why they need to use this exception in the first place. Possible explanations include:

- An application that loads encrypted media content that contains no personalized information.
- Connections to devices that cannot be upgraded to use secure connections.
- Connection to a server that is managed by another entity and does not support secure connections.



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`NSAppTransportSecurity`|Configure ATS
|`NSAllowsArbitraryLoads`|Set to `YES` to disable ATS everywhere. In iOS 10 and later, and macOS 10.12 and later, the value of this key is ignored if any of the following keys are present in your app’s Info.plist file: NSAllowsArbitraryLoadsInMedia, NSAllowsArbitraryLoadsInWebContent, NSAllowsLocalNetworking
|`NSAllowsArbitraryLoadsInMedia`|Set to `YES` to disable ATS for media loaded using APIs from the AV Foundation framework. ****(iOS 10+, macOS 10.12+)****
|`NSAllowsArbitraryLoadsInWebContent`|Set to `YES` to disable ATS in your app’s web views (`WKWebView`, `UIWebView`, `WebView`) without affecting your NSURLSession connections. ****(iOS 10+, macOS 10.12+)****
|`NSAllowsLocalNetworking`|Set to `YES` to disable for connections to unqualified domains and to .local domains. ****(iOS 10+, macOS 10.12+)****
|`NSExceptionDomains`|Configure exceptions for specific domains
|`NSIncludesSubdomains`|Set to `YES` to apply the exceptions to all subdomains of the selected domain.
|`NSRequiresCertificateTransparency`|Set to `YES` to require that valid, signed Certificate Transparency (CT) timestamps, from known CT logs, be presented for server (X.509) certificates on a domain. ****(iOS 10+, macOS 10.12+)****
|`NSExceptionAllowsInsecureHTTPLoads`|Set to `YES` to allow HTTP on the selected domain.
|`NSExceptionRequiresForwardSecrecy`|Defaults to `YES`; Set to `NO` to disable Forward Secrecy and accept more ciphers.
|`NSExceptionMinimumTLSVersion`|Defaults to `TLSv1.2`; Possible values are: `TLSv1.0`, `TLSv1.1`, `TLSv1.2`
|`NSThirdPartyExceptionAllowsInsecureHTTPLoads`|Similar to `NSExceptionAllowsInsecureHTTPLoads`, but for domains that you have no control over
|`NSThirdPartyExceptionRequiresForwardSecrecy`|Similar to `NSExceptionRequiresForwardSecrecy`, but for domains that you have no control over
|`NSThirdPartyExceptionMinimumTLSVersion`|Similar to `NSExceptionMinimumTLSVersion`, but for domains that you have no control over



#### Remarks


The [App Transport Security](https://developer.apple.com/library/content/releasenotes/General/WhatsNewIniOS/Articles/iOS9.html#//apple_ref/doc/uid/TP40016198-SW14) is a security feature in iOS and macOS. It prevents apps from establishing unsecured connections. By default, apps can only use secure HTTPS connections.

If an app needs to connect to a server via HTTP, exceptions must be defined in the `Info.plist`. (see the examples for more information about that)

**Note:** In 2017, Apple will enforce ATS. That means, that you can no longer upload apps that have ATS-exceptions defined in the `Info.plist`. If you can provide good arguments, why you have to use HTTP, you can contact Apple and they might allow you to define exceptions. (Source: [WWDC 2016 - Session 706](https://developer.apple.com/videos/play/wwdc2016/706/))

More information on the App Transport Security configuration can be found in the [CocoaKeys Documentation](https://developer.apple.com/library/ios/documentation/General/Reference/InfoPlistKeyReference/Articles/CocoaKeys.html#//apple_ref/doc/uid/TP40009251-SW33).

