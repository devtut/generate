---
metaTitle: "React Native - Getting started with react-native"
description: "Setup for Mac, Setup for Linux (Ubuntu), Setup for Windows"
---

# Getting started with react-native



## Setup for Mac


**Installing package manager Homebrew `brew`**

Paste that at a Terminal prompt.

```js
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

```

**Installing Xcode IDE**

Download it using link below or find it on Mac App Store

[https://developer.apple.com/download/](https://developer.apple.com/download/)

> 
**NOTE:** If you have **Xcode-beta.app** installed along with production version of **Xcode.app**, make sure you are using production version of `xcodebuild` tool. You can set it with:
`sudo xcode-select -switch /Applications/Xcode.app/Contents/Developer/`


**Installing Android environment**

<li>
Git **`git`**
*If you have installed XCode, Git is already installed, otherwise run the following

```js
brew install git 

```


</li>

<li>
[Latest JDK](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
</li>
<li>
[Android Studio](http://developer.android.com/sdk/index.html)
Choose a Custom installation
[<img src="https://i.stack.imgur.com/WklQv.png" alt="Choose a Custom installation" />](https://i.stack.imgur.com/WklQv.png)
Choose both Performance and Android Virtual Device
[<img src="https://i.stack.imgur.com/UiNxG.png" alt="Choose both Performance and Android Virtual Device" />](https://i.stack.imgur.com/UiNxG.png)
After installation, choose Configure -> SDK Manager from the Android Studio welcome window.
[<img src="https://i.stack.imgur.com/JYSu6.png" alt="choose Configure -> SDK Manager" />](https://i.stack.imgur.com/JYSu6.png)
In the SDK Platforms window, choose Show Package Details and under Android 6.0 (Marshmallow), make sure that Google APIs, Intel x86 Atom System Image, Intel x86 Atom_64 System Image, and Google APIs Intel x86 Atom_64 System Image are checked.
[<img src="https://i.stack.imgur.com/wBiuq.png" alt="SDK Platforms window" />](https://i.stack.imgur.com/wBiuq.png)
In the SDK Tools window, choose Show Package Details and under Android SDK Build Tools, make sure that Android SDK Build-Tools 23.0.1 is selected.
[<img src="https://i.stack.imgur.com/9ZsjC.png" alt="SDK Tools window" />](https://i.stack.imgur.com/9ZsjC.png)
</li>
<li>
Environment Variable `ANDROID_HOME`
Ensure the ANDROID_HOME environment variable points to your existing Android SDK. To do that, add this to your ~/.bashrc, ~/.bash_profile (or whatever your shell uses) and re-open your terminal:
<p>If you installed the SDK without Android Studio, then it may be something like:
/usr/local/opt/android-sdk</p>

```js
export ANDROID_HOME=~/Library/Android/sdk

```


</li>

**Dependencies for Mac**

You will need Xcode for iOS and Android Studio for android, node.js, the React Native command line tools, and Watchman.

We recommend installing node and watchman via Homebrew.

```js
brew install node
brew install watchman

```

> 
[Watchman](https://facebook.github.io/watchman) is a tool by Facebook for watching changes in the filesystem. It is highly recommended you install it for better performance. It is optional.


Node comes with npm, which lets you install the React Native command line interface.

```js
npm install -g react-native-cli

```

If you get a permission error, try with sudo:

```js
sudo npm install -g react-native-cli.

```

For iOS the easiest way to install Xcode is via the Mac App Store.
And for android download and install Android Studio.

If you plan to make changes in Java code, we recommend Gradle Daemon which speeds up the build.

**Testing your React Native Installation**

Use the React Native command line tools to generate a new React Native project called "AwesomeProject", then run react-native run-ios inside the newly created folder.

```js
react-native init AwesomeProject
cd AwesomeProject
react-native run-ios

```

You should see your new app running in the iOS Simulator shortly. react-native run-ios is just one way to run your app - you can also run it directly from within Xcode or Nuclide.

**Modifying your app**

Now that you have successfully run the app, let's modify it.

- Open index.ios.js or index.android.js in your text editor of choice and edit some lines.
<li>Hit Command⌘ + R in your iOS Simulator to reload the app and see your
change! That's it!</li>

Congratulations! You've successfully run and modified your first React Native app.

<sub>source: [Getting Started - React-Native](http://facebook.github.io/react-native/docs/getting-started.html) </sub>



## Setup for Linux (Ubuntu)


**1) Setup Node.JS**

### Start the terminal and run the following commands to install nodeJS:

```js
curl -sL https://deb.nodesource.com/setup_5.x | sudo -E bash -

sudo apt-get install nodejs

```

### If node command is unavailable

```js
sudo ln -s /usr/bin/nodejs /usr/bin/node

```

### Alternatives NodeJS instalations:

```js
curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
sudo apt-get install -y nodejs

```

or

```js
curl -sL https://deb.nodesource.com/setup_7.x | sudo -E bash -
sudo apt-get install -y nodejs

```

### check if you have the current version

```js
node -v 

```

### Run the npm to install the react-native

```js
sudo npm install -g react-native-cli

```

**2) Setup Java**

```js
sudo apt-get install lib32stdc++6 lib32z1 openjdk-7-jdk

```

**3) Setup Android Studio:**

### Android SDK or Android Studio

```js
http://developer.android.com/sdk/index.html

```

### Android SDK e ENV

```js
export ANDROID_HOME=/YOUR/LOCAL/ANDROID/SDK
export PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools

```

**4) Setup emulator:**

On the terminal run the command

```js
android

```

Select "SDK Platforms" from within the SDK Manager and you should see a blue checkmark next to "Android 7.0 (Nougat)". In case it is not, click on the checkbox and then "Apply".

[<img src="https://i.stack.imgur.com/ZrSya.png" alt="enter image description here" />](https://i.stack.imgur.com/ZrSya.png)

**5) Start a project**

### Example app init

```js
react-native init ReactNativeDemo && cd ReactNativeDemo

```

### Obs: Always check if the version on `android/app/build.gradle` is the same as the Build Tools downloaded on your android SDK

```js
android {
    compileSdkVersion XX
    buildToolsVersion "XX.X.X"
...

```

**6) Run the project**

### Open Android AVD to set up a virtual android. Execute the command line:

```js
android avd

```

Follow the instructions to create a virtual device and start it

Open another terminal and run the command lines:

```js
react-native run-android
react-native start

```



## Setup for Windows


Note: You cannot develop react-native apps for iOS on Windows, only react-native android apps.

The official setup docs for react-native on windows can be [found here](https://facebook.github.io/react-native/docs/getting-started.html#dependencies-for-windows-android). If you need more details there is a [granular guide here](http://bitvbit.blogspot.com/2016/07/react-native-android-apps-on-windows.html).

**Tools/Environment**

- Windows 10
- command line tool (eg Powershell or windows command line)
- [Chocolatey](https://chocolatey.org/) ([steps to setup via PowerShell](http://bitvbit.blogspot.com/2016/07/react-native-android-apps-on-windows.html#setup-choco))
- The JDK (version 8)
- Android Studio
- An Intel machine with Virtualization technology enabled for HAXM (optional, only needed if you want to use an emulator)

**1) Setup your machine for react native development**

Start the command line as an administrator run the following commands:

```js
choco install nodejs.install
choco install python2

```

Restart command line as an administrator so you can run npm

```js
npm install -g react-native-cli

```

After running the last command copy the directory that react-native was installed in. You will need this for Step 4. I tried this on two computers in one case it was: `C:\Program Files (x86)\Nodist\v-x64\6.2.2`. In the other it was: `C:\Users\admin\AppData\Roaming\npm`

**2) Set your Environment Variables**

[A Step by Step guide with images can be found here for this section.](http://bitvbit.blogspot.com/2016/07/react-native-android-apps-on-windows.html)

Open the Environment Variables window by navigating to:

[Right click] "Start" menu -> System -> Advanced System Settings -> Environment Variables

In the bottom section find the "Path" System Variable and add the location that react-native was installed to in step 1.

If you haven't added an ANDROID_HOME environment variable you will have to do that here too. While still in the "Environment Variables" window, add a new System Variable with the name "ANDROID_HOME" and value as the path to your android sdk.

Then restart the command line as an admin so you can run react-native commands in it.

**3) Create your project**
In command line, navigate to the folder you want to place your project and run the following command:

```js
react-native init ProjectName

```

**4) Run your project**
Start an emulator from android studio
Navigate to the root directory of your project in command line and run it:

```js
cd ProjectName
react-native run-android

```

You may run into dependency issues. For example, there may be an error that you do not have the correct build tools version. To fix this you will have to open [the sdk manager in Android Studio](https://developer.android.com/studio/intro/update.html#sdk-manager) and download the build tools from there.

**Congrats!**

To refresh the ui you can press the `r` key twice while in the emulator and running the app. To see developer options you can press `ctrl + m`.



#### Remarks


React Native lets you build mobile apps using only JavaScript. It uses the same design as React, letting you compose a rich mobile UI from declarative components.

With React Native, you don't build a “mobile web app”, an “HTML5 app”, or a “hybrid app”. You build a real mobile app that's indistinguishable from an app built using Objective-C or Java. React Native uses the same fundamental UI building blocks as regular iOS and Android apps. You just put those building blocks together using JavaScript and React.

It is open-source and maintained by Facebook.

- [Website](http://facebook.github.io/react-native/)
- [Documentation](http://facebook.github.io/react-native/docs/getting-started.html)
- [GitHub Repository](https://github.com/facebook/react-native)

<sub>Source: [React Native website](https://facebook.github.io/react-native/)</sub>

