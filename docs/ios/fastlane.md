---
metaTitle: "iOS - Fastlane"
description: "fastlane tools"
---

# Fastlane



## fastlane tools


[fastlane](https://fastlane.tools) is an open source build automation tool for Android and iOS for developers. It reduce your build generation time. It is a command line tool that uses [Ruby](http://ruby-lang.org/en), so you need Ruby on your computer. Most Macs already have Ruby installed by default.

### Install fastlane

1. Open a terminal.
1. Run `sudo gem install fastlane --verbose`
1. If you haven’t installed the Xcode command-line tools yet, run `xcode-select --install` to install them
1. Now, `cd` into your project folder (type `cd ` [with the space at the end] and drag your project folder into the terminal)
1. Run `fastlane init` to get fastlane setup.
1. Now you can able to use all the Fastlane tools:

### iOS Tools

- [deliver](https://github.com/fastlane/fastlane/tree/master/deliver): Upload screenshots, metadata, and your app to the App Store
- [snapshot](https://github.com/fastlane/fastlane/tree/master/snapshot): Automate taking localized screenshots of your iOS app on every device
- [frameit](https://github.com/fastlane/fastlane/tree/master/frameit): Quickly put your screenshots into the right device frames
- [pem](https://github.com/fastlane/fastlane/tree/master/pem): Automatically generate and renew your push notification profiles
- [sigh](https://github.com/fastlane/fastlane/tree/master/sigh): Because you would rather spend your time building stuff than fighting               provisioning
- [produce](https://github.com/fastlane/fastlane/tree/master/produce): Create new iOS apps on iTunes Connect and Dev Portal using the command line
- [cert](https://github.com/fastlane/fastlane/tree/master/cert): Automatically create and maintain iOS code signing certificates
- [gym](https://github.com/fastlane/fastlane/tree/master/gym): Building your iOS apps has never been easier
- [match](https://github.com/fastlane/fastlane/tree/master/match): Easily sync your certificates and profiles across your team using Git
- [scan](https://github.com/fastlane/fastlane/tree/master/scan): The easiest way to run tests for your iOS and Mac apps
- [spaceship](https://github.com/fastlane/fastlane/tree/master/spaceship): Ruby library to access the Apple Dev Center and iTunes Connect

### iOS TestFlight Tools

- [pilot](https://github.com/fastlane/fastlane/tree/master/pilot): The best way to manage your TestFlight testers and builds from your terminal
- [boarding](https://github.com/fastlane/boarding): The easiest way to invite your TestFlight beta testers

### Android Tools

- [supply](https://github.com/fastlane/fastlane/tree/master/supply): Upload your Android app and its metadata to Google Play
- [screengrab](https://github.com/fastlane/fastlane/tree/master/screengrab): Automate taking localized screenshots of your Android app on every device

