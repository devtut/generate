---
metaTitle: "React Native - Linking Native API"
description: "Outgoing Links, Incomming Links"
---

# Linking Native API


Linking API enables you to both send and receive links between applications. For example, opening the Phone app with number dialed in or opening the Google Maps and starting a navigation to a chosen destination. You can also utilise Linking to make your app able to respond to links opening it from other applications.

To use `Linking` you need to first import it from `react-native`

`import {Linking} from 'react-native'`



## Outgoing Links


To open a link call openURL.

```js
Linking.openURL(url)
.catch(err => console.error('An error occurred ', err))

```

The preferred method is to check if any installed app can handle a given URL beforehand.

```js
Linking.canOpenURL(url)
.then(supported => {
  if (!supported) {
    console.log('Unsupported URL: ' + url)
  } else {
    return Linking.openURL(url)
  }
}).catch(err => console.error('An error occurred ', err))

```

### URI Schemes

|Target App|Example|Reference
|---|---|---|---|---|---|---|---|---|---
|Web Browser|`https://stackoverflow.com`|
|Phone|`tel:1-408-555-5555`|[Apple](https://developer.apple.com/library/content/featuredarticles/iPhoneURLScheme_Reference/PhoneLinks/PhoneLinks.html#//apple_ref/doc/uid/TP40007899-CH6-SW1)
|Mail|`mailto:email@example.com`|[Apple](https://developer.apple.com/library/content/featuredarticles/iPhoneURLScheme_Reference/MailLinks/MailLinks.html#//apple_ref/doc/uid/TP40007899-CH4-SW1)
|SMS|`sms:1-408-555-1212`|[Apple](https://developer.apple.com/library/content/featuredarticles/iPhoneURLScheme_Reference/SMSLinks/SMSLinks.html#//apple_ref/doc/uid/TP40007899-CH7-SW1)
|Apple Maps|`http://maps.apple.com/?ll=37.484847,-122.148386`|[Apple](https://developer.apple.com/library/content/featuredarticles/iPhoneURLScheme_Reference/MapLinks/MapLinks.html#//apple_ref/doc/uid/TP40007899-CH5-SW1)
|Google Maps|`geo:37.7749,-122.4194`|[Google](https://developers.google.com/maps/documentation/android-api/intents)
|iTunes|See [iTunes Link Maker](https://linkmaker.itunes.apple.com/en-us)|[Apple](https://developer.apple.com/library/content/featuredarticles/iPhoneURLScheme_Reference/iTunesLinks/iTunesLinks.html#//apple_ref/doc/uid/TP40007899-CH3-SW1)
|Facebook|`fb://profile`|[Stack Overflow](https://stackoverflow.com/questions/5707722/what-are-all-the-custom-url-schemes-supported-by-the-facebook-iphone-app)
|YouTube|`http://www.youtube.com/v/oHg5SJYRHA0`|[Apple](https://developer.apple.com/library/content/featuredarticles/iPhoneURLScheme_Reference/YouTubeLinks/YouTubeLinks.html#//apple_ref/doc/uid/TP40007899-CH8-SW1)
|Facetime|`facetime://user@example.com`|[Apple](https://developer.apple.com/library/content/featuredarticles/iPhoneURLScheme_Reference/FacetimeLinks/FacetimeLinks.html#//apple_ref/doc/uid/TP40007899-CH2-SW1)
|iOS Calendar|`calshow:514300000` [1]|[iPhoneDevWiki](http://iphonedevwiki.net/index.php/NSURL)

[1] Opens the calendar at the stated number of seconds since 1. 1. 2001 (UTC?). For some reason this API is undocumented by Apple.



## Incomming Links


You can detect when your app is launched from an external URL.

```js
componentDidMount() {
  const url = Linking.getInitialURL()
  .then((url) => {
    if (url) {
      console.log('Initial url is: ' + url)
    }
  }).catch(err => console.error('An error occurred ', err))
}

```

To enable this on iOS [Link `RCTLinking` to your project](https://facebook.github.io/react-native/docs/linking-libraries-ios.html#manual-linking).

To enable this on Android, [follow these steps](https://developer.android.com/training/app-indexing/deep-linking.html#adding-filters).

