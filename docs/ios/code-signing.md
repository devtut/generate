---
metaTitle: "iOS - Code signing"
description: "Provisioning Profiles"
---

# Code signing



## Provisioning Profiles


In order to build an IPA file in XCode, you require to sign your application with a certificate and provisioning profile. These can be created at [https://developer.apple.com/account/ios/profile/create](https://developer.apple.com/account/ios/profile/create)

### Provisioning Profile Types

Provisioning Profiles are split into two types, Development, and Distribution:

### Development

- iOS App Development / tvOS App Development - used in development in order to install your app onto a test device.

### Distribution

- App Store / tvOS App Store - used to sign your application for app store upload.
- In House - Used for Enterprise distribution of your app, to devices within your business.
- Ad Hoc / tvOS Ad Hoc - Used to distribute your app to a limited number of specific devices (e.g. you will need to know the UDIDs of the devices you want to install your app to).

