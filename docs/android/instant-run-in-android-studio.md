---
metaTitle: "Android - Instant Run in Android Studio"
description: "Enabling or disabling Instant Run, Types of code Swaps in Instant Run, Unsupported code changes when using Instant Run"
---

# Instant Run in Android Studio



## Enabling or disabling Instant Run


<li>Open the Settings or Preferences dialog:
<ul>
1. On Windows or Linux, select `File` > `Settings` from the main menu.
1. On Mac OSX, select `Android Studio` > `Preferences` from the main menu.
</ul>
</li>
1. Navigate to `Build, Execution, Deployment` > `Compiler`.
1. In the text field next to Command-line Options, enter your command-line options.
1. Click OK to save and exit.

[<img src="http://i.stack.imgur.com/9lTtp.png" alt="enter image description here" />](http://i.stack.imgur.com/9lTtp.png)

The top option is Instant run. Check/uncheck that box.

[Documentation](https://developer.android.com/studio/run/index.html#instant-run)



## Types of code Swaps in Instant Run


There are three types of code swaps that Instant run enables to support faster debugging and running app from your code in Android Studio.

- Hot Swap
- Warm Swap
- Cold Swap

**When are each of these swaps triggered?**

**HOT SWAP** is triggered when an existing method's implementation is changed.

**WARM SWAP** is triggered when an existing resource is changed or removed (anything in the res folder)

**COLD SWAP** whenever there is a structural code change in your app's code e.g.

1. Add, remove, or change:

- an annotation
- an instance field
- a static field
- a static method signature
- an instance method signature

1. Change which parent class the current class inherits from
1. Change the list of implemented interfaces
1. Change a class's static initializer
1. Reorder layout elements that use dynamic resource IDs

**What happens when a code swap happens?**

**HOT SWAP** changes are visible instantly - as soon as the next call to the method whose implementation is changed is made.

**WARM SWAP** restarts the current activity

**COLD SWAP** restarts the entire app (without reinstall)



## Unsupported code changes when using Instant Run


There are a few changes where instant won't do its trick and a full build and reinstall fo your app will happen just like it used to happen before Instant Run was born.

1. Change the app manifest
1. Change resources referenced by the app manifest
1. Change an Android widget UI element (requires a Clean and Rerun)

[Documentation](https://developer.android.com/studio/run/index.html#instant-run)



#### Remarks


Instant Run is an extended behavior for the run and debug commands that enables faster debugging by not requiring a full build and reinstall for eevry change done in your app's code.

> 
<p>Introduced in Android Studio 2.0, Instant Run is a behavior for the
Run  and Debug commands that significantly reduces the time between
updates to your app. Although your first build may take longer to
complete, Instant Run pushes subsequent updates to your app without
building a new APK, so changes are visible much more quickly.</p>
<p>Instant Run is supported only when you deploy the debug build variant,
use Android Plugin for Gradle version 2.0.0 or higher, and set
minSdkVersion to 15 or higher in your app's module-level build.gradle
file. For the best performance, set minSdkVersion to 21 or higher.</p>
<p>After deploying an app, a small, yellow thunderbolt icon appears
within the Run  button (or Debug   button), indicating that Instant
Run is ready to push updates the next time you click the button.
Instead of building a new APK, it pushes just those new changes and,
in some cases, the app doesn't even need to restart but immediately
shows the effect of those code changes.</p>
<p>Instant Run pushes updated code and resources to your connected device
or emulator by performing a hot swap, warm swap, or cold swap. It
automatically determines the type of swap to perform based on the type
of change you made. The video above provides interesting detail about
how this all works under the hood. For a quick summary of how Instant
Run behaves when you push certain code changes to a target device,
however, see the following table.</p>


[Documentation](https://developer.android.com/studio/run/index.html#instant-run)

