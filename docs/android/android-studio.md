---
metaTitle: "Android - Android Studio"
description: "Setup Android Studio, View And Add Shortcuts in Android Studio, Filter logs from UI, Create filters configuration, Custom colors of logcat message based on message importance, Enable/Disable blank line copy, Android Studio useful shortcuts, Android Studio Improve performance tip, Gradle build project takes forever, Create assets folder"
---

# Android Studio



## Setup Android Studio


**System Requirements**

- Microsoft® Windows® 8/7/Vista/2003 (32 or 64-bit).
- Mac® OS X® 10.8.5 or higher, up to 10.9 (Mavericks)
- GNOME or KDE desktop

**Installation**

Window

1. Download and install [JDK (Java Development Kit)](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html) version 8
1. Download [Android Studio](https://developer.android.com/studio/index.html)
1. Launch `Android Studio.exe` then mention JDK path and download the latest SDK

Linux

1. Download and install [JDK (Java Development Kit)](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html) version 8
1. Download [Android Studio](https://developer.android.com/studio/index.html)
1. Extract the zip file
1. Open terminal, cd to the extracted folder, cd to bin (example `cd android-studio/bin`)
1. Run ./studio.sh



## View And Add Shortcuts in Android Studio


By going to Settings >> Keymap A window will popup showing All the `Editor Actions` with the their name and shortcuts. Some of the `Editor Actions` do not have shortcuts. So right click on that and add a new shortcut to that.<br />
Check the image below

[<img src="http://i.stack.imgur.com/bjDlR.png" alt="enter image description here" />](http://i.stack.imgur.com/bjDlR.png)



## Filter logs from UI


Android logs can be filtered directly from the UI. Using this code

```java
public class MainActivity extends AppCompatActivity {
    private final static String TAG1 = MainActivity.class.getSimpleName();
    private final static String TAG2 = MainActivity.class.getCanonicalName();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        Log.e(TAG1,"Log from onCreate method with TAG1");
        Log.i(TAG2,"Log from onCreate method with TAG2");
    }
}

```

If I use the regex `TAG1|TAG2` and the level `verbose` I get

```java
01-14 10:34:46.961 12880-12880/android.doc.so.thiebaudthomas.sodocandroid E/MainActivity: Log from onCreate method with TAG1
01-14 10:34:46.961 12880-12880/android.doc.so.thiebaudthomas.sodocandroid I/androdi.doc.so.thiebaudthomas.sodocandroid.MainActivity: Log from onCreate method with TAG2

```

[<img src="http://i.stack.imgur.com/NkuER.png" alt="enter image description here" />](http://i.stack.imgur.com/NkuER.png)

The level can be set to get logs with given level and above. For example the `verbose` level will catch `verbose, debug, info, warn, error and assert` logs.

Using the same example, if I set the level to `error`, I only get

```java
01-14 10:34:46.961 12880-12880/androdi.doc.so.thiebaudthomas.sodocandroid E/MainActivity: Log from onCreate method with TAG1

```

[<img src="http://i.stack.imgur.com/8i6o0.png" alt="enter image description here" />](http://i.stack.imgur.com/8i6o0.png)



## Create filters configuration


Custom filters can be set and save from the UI. In the `AndroidMonitor` tab, click on the right dropdown (must contains `Show only selected application` or `No filters`) and select `Edit filter configuration`.

Enter the filter you want

[<img src="http://i.stack.imgur.com/0cm2p.png" alt="enter image description here" />](http://i.stack.imgur.com/0cm2p.png)

And use it (you can selected it from the same dropdown)

[<img src="http://i.stack.imgur.com/5ujVM.png" alt="enter image description here" />](http://i.stack.imgur.com/5ujVM.png)

**Important** If you add an input in the filter bar, android studio will consider both your filter and your input.

With both input and filter there is no output
[<img src="http://i.stack.imgur.com/oSuUP.png" alt="Custom filter with input" />](http://i.stack.imgur.com/oSuUP.png)

Without filter, there is some outputs
[<img src="http://i.stack.imgur.com/31oP6.png" alt="Filter input only" />](http://i.stack.imgur.com/31oP6.png)



## Custom colors of logcat message based on message importance


Go to File -> Settings -> Editor -> Colors & Fonts -> Android Logcat

**Change the colors as you need:**

[<img src="http://i.stack.imgur.com/Dg8rG.png" alt="enter image description here" />](http://i.stack.imgur.com/Dg8rG.png)

**Choose the appropriate color:**

[<img src="http://i.stack.imgur.com/GZpbS.png" alt="enter image description here" />](http://i.stack.imgur.com/GZpbS.png)



## Enable/Disable blank line copy


`ctrl + alt + shift + /` (`cmd + alt + shift + /` on `MacOS`) should show you the following dialog:

[<img src="http://i.stack.imgur.com/YZ5Ax.png" alt="enter image description here" />](http://i.stack.imgur.com/YZ5Ax.png)

Clicking on `Registry` you will get

[<img src="http://i.stack.imgur.com/LC1p9.png" alt="enter image description here" />](http://i.stack.imgur.com/LC1p9.png)

The key you want to enable/disable is

```java
editor.skip.copy.and.cut.for.empty.selection

```

Tested on `Linux Ubuntu` and `MacOS`.



## Android Studio useful shortcuts


The following are some of the more common/useful shortcuts.

These are based on the default IntelliJ shortcut map. You can switch to other common IDE shortcut maps via `File -> Settings -> Keymap -> <Choose Eclipse/Visual Studio/etc from Keymaps dropdown>`

|Action|Shortcut
|---|---|---|---|---|---|---|---|---|---
|Format code|<kbd>CTRL</kbd> + <kbd>ALT</kbd> + <kbd>L</kbd>
|Add unimplemented methods|<kbd>CTRL</kbd> + <kbd>I</kbd>
|Show logcat|<kbd>ALT</kbd> + <kbd>6</kbd>
|Build|<kbd>CTRL</kbd> + <kbd>F9</kbd>
|Build and Run|<kbd>CTRL</kbd> + <kbd>F10</kbd>
|Find|<kbd>CTRL</kbd> + <kbd>F</kbd>
|Find in project|<kbd>CTRL</kbd>+<kbd>SHIFT</kbd> + <kbd>F</kbd>
|Find and replace|<kbd>CTRL</kbd> + <kbd>R</kbd>
|Find and replace in project|<kbd>CTRL</kbd> + <kbd>SHIFT</kbd> + <kbd>R</kbd>
|Override methods|<kbd>CTRL</kbd> + <kbd>O</kbd>
|Show project|<kbd>ALT</kbd> + <kbd>1</kbd>
|Hide project - logcat|<kbd>SHIFT</kbd> + <kbd>ESC</kbd>
|Collapse all|<kbd>CTRL</kbd> + <kbd>SHIFT</kbd> + <kbd>NumPad +</kbd>
|View Debug Points|<kbd>CTRL</kbd> + <kbd>SHIFT</kbd> + <kbd>F8</kbd>
|Expand all|<kbd>CTRL</kbd> + <kbd>SHIFT</kbd> + <kbd>NumPad -</kbd>
|Open Settings|<kbd>ALT</kbd> + <kbd>s</kbd>
|Select Target (open current file in Project view)|<kbd>ALT</kbd> + <kbd>F1</kbd> → <kbd>ENTER</kbd>
|Search Everywhere|<kbd>SHIFT</kbd> → <kbd>SHIFT</kbd> (Double shift)
|Code | Surround With|<kbd>CTRL</kbd> → <kbd>ALT</kbd> + <kbd>T</kbd>
|Create method form selected code|<kbd>ALT</kbd> + <kbd>CTRL</kbd>

Refactor:

|Action|Shortcut
|---|---|---|---|---|---|---|---|---|---
|Refactor This (menu/picker for all applicable refactor actions of the current element)|Mac <kbd>CTRL</kbd> + <kbd>T</kbd> - Win/Linux <kbd>CTRL</kbd> + <kbd>ALT</kbd> + <kbd>T</kbd>
|Rename|<kbd>SHIFT</kbd> + <kbd>F6</kbd>
|Extract Method|Mac <kbd>CMD</kbd> + <kbd>ALT</kbd> + <kbd>M</kbd> - Win/Linux <kbd>CTRL</kbd> + <kbd>ALT</kbd> + <kbd>M</kbd>
|Extract Parameter|Mac <kbd>CMD</kbd> + <kbd>ALT</kbd> + <kbd>P</kbd> - Win/Linux <kbd>CTRL</kbd> + <kbd>ALT</kbd> + <kbd>P</kbd>
|Extract Variable|Mac <kbd>CMD</kbd> + <kbd>ALT</kbd> + <kbd>V</kbd> - Win/Linux <kbd>CTRL</kbd> + <kbd>ALT</kbd> + <kbd>V</kbd>



## Android Studio Improve performance tip


**Enable Offline Work:**

1. Click File -> Settings. Search for "gradle" and click in `Offline work` box.
1. Go to Compiler (in same settings dialog just below `Gradle`) and add `--offline` to `Command-line Options` text box.

**Improve Gradle Performance**

Add following two line of code in your gradle.properties file.

```java
org.gradle.daemon=true
org.gradle.parallel=true

```

**Increasing the value of `-Xmx` and `-Xms` in `studio.vmoptions` file**

```java
-Xms1024m
-Xmx4096m
-XX:MaxPermSize=1024m
-XX:ReservedCodeCacheSize=256m
-XX:+UseCompressedOops

```

Window

> 
<p>%USERPROFILE%.{FOLDER_NAME}\studio.exe.vmoptions and/or
%USERPROFILE%.{FOLDER_NAME}\studio64.exe.vmoptions</p>


Mac

> 
~/Library/Preferences/{FOLDER_NAME}/studio.vmoptions


Linux

> 
<p>~/.{FOLDER_NAME}/studio.vmoptions and/or
~/.{FOLDER_NAME}/studio64.vmoptions</p>




## Gradle build project takes forever


**Android Studio** -> **Preferences** -> **Gradle** -> Tick  **Offline work** and then restart your Android studio.

**Reference screenshot:**

[<img src="http://i.stack.imgur.com/VhZL5.png" alt="enter image description here" />](http://i.stack.imgur.com/VhZL5.png)



## Create assets folder


- Right click in MAIN folder > New > Folder > Assets Folder.
- Assets folder will be under MAIN folder with the same symbol as RES folder.
<li>In this example I put a font file.
[<img src="https://i.stack.imgur.com/IF2ED.png" alt="enter image description here" />](https://i.stack.imgur.com/IF2ED.png)</li>

