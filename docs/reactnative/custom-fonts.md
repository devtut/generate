---
metaTitle: "React Native - Custom Fonts"
description: "Custom fonts for both Android and IOS, Steps to use custom fonts in React Native (Android), Steps to use custom fonts in React Native (iOS)"
---

# Custom Fonts



## Custom fonts for both Android and IOS


<li>
Create a folder in your project folder, and add your fonts to it. Example:
<ul>
<li>Example: Here we added a folder in root called "mystuff", then "fonts", and inside it we placed our fonts:
<ul>
- <img src="https://i.imgur.com/1tFQQmR.png" alt="" />

<li>
Add the below code in `package.json`.
<pre>{
    ...

    "rnpm": {
        "assets": [
          "path/to/fontfolder"
        ]
    },

    ...
}
</pre>
<ul>
<li>
For the example above, our package.json would now have a path of "mystuff/fonts":

```js
"rnpm": {
  "assets": [
    "mystuff/fonts"
  ]
}

```


</li>

Run `react-native link` command.

Using custom fonts on project below code

Where `FONT-NAME` is the prefix platform specific.

### Android

FONT-NAME is the words before the extension in file. Example: Your font's file name is `Roboto-Regular.ttf`, so you would set `fontFamily: Roboto-Regular`.

### iOS

FONT-NAME is "Full Name" found after right clicking, on the font file, then clicking on "Get Info". ( Source: [https://stackoverflow.com/a/16788493/2529614](https://stackoverflow.com/a/16788493/2529614) ), in the screenshot below, the file name is `MM Proxima Nova Ultra bold.otf`, however "Full Name" is "Proxima Nova Semibold", thus you would set `fontFamily: Proxima Nova Semibold`. Screenshot - <img src="https://i.imgur.com/Xn8c0j4.png" alt="" />

Run `react-native run-ios` or `react-native run-android` again (this will recompile with the resources)



## Steps to use custom fonts in React Native (Android)


1. Paste your fonts file inside `android/app/src/main/assets/fonts/font_name.ttf`
1. Recompile the Android app by running `react-native run-android`
1. Now, You can use `fontFamily: 'font_name'` in your React Native Styles



## Steps to use custom fonts in React Native (iOS)


**1. Include the font in your Xcode project.**

[<img src="http://i.stack.imgur.com/mJktz.png" alt="Including font to Xcode project" />](http://i.stack.imgur.com/mJktz.png)

**2. Make sure that they are included in the Target Membership column**

Click on the font from the navigator, and check if the font included.

[<img src="http://i.stack.imgur.com/dYodB.png" alt="Check font in target membership" />](http://i.stack.imgur.com/dYodB.png)

**3. Check if the font included as Resource in your bundle**

click on your Xcode project file, select "Build Phases, select "Copy Bundle Resources". Check if your font is added.

[<img src="http://i.stack.imgur.com/rgxoN.png" alt="check in build phases" />](http://i.stack.imgur.com/rgxoN.png)

**4. Include the font in Application Plist (Info.plist)**

from the application main folder open Info.plist, click on "Information Property List", and then click the plus sign (+). from drop down list choose "Fonts provided by application".

[<img src="http://i.stack.imgur.com/1CthG.png" alt="enter image description here" />](http://i.stack.imgur.com/1CthG.png)

**5. Add Font name in Fonts provided by application**

expand Fonts Provided by Application and add the Font Name exactly to value column

[<img src="http://i.stack.imgur.com/7Jo4E.png" alt="enter image description here" />](http://i.stack.imgur.com/7Jo4E.png)

<li>
Use it in the Application

```js
 <Text style={{fontFamily:'IndieFlower'}}>
   Welcome to React Native!
 </Text>

```


</li>

