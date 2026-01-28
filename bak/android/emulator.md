---
metaTitle: "Android - Emulator"
description: "Taking screenshots, Open the AVD Manager, Simulate call, Resolving Errors while starting emulator"
---

# Emulator



## Taking screenshots


If you want to take a screenshot from the Android Emulator (2.0), then you just need to press <kbd>Ctrl</kbd> + <kbd>S</kbd> or you click on the camera icon on the side bar:

[<img src="https://i.stack.imgur.com/QBRpX.png" alt="Taking a screenshot in the Android Emulator 2.0" />](https://i.stack.imgur.com/QBRpX.png)

If you use an older version of the Android Emulator or you want to take a screenshot from a real device, then you need to click on the camera icon in the Android Monitor:

[<img src="https://i.stack.imgur.com/VO8nt.png" alt="Screen Capture button in the Android Monitor" />](https://i.stack.imgur.com/VO8nt.png)

Double check that you have selected the right device, because this is a common pitfall.

After taking a screenshot, you can optionally add the following decorations to it (also see the image below):

1. A device frame around the screenshot.
1. A drop shadow below the device frame.
1. A screen glare across device frame and screenshot.

[<img src="https://i.stack.imgur.com/8edUy.png" alt="Decoration options for screenshots" />](https://i.stack.imgur.com/8edUy.png)



## Open the AVD Manager


Once the SDK installed, you can open the AVD Manager from the command line using `android avd`.

You can also access AVD Manager from Android studio using `Tools` > `Android` > `AVD Manager` or by clicking on the AVD Manager icon in the toolbar which is the second in the screenshot below.

[<img src="http://i.stack.imgur.com/4Tnrd.png" alt="enter image description here" />](http://i.stack.imgur.com/4Tnrd.png)



## Simulate call


To simulate a phone call, press the 'Extended controls' button indicated by three dots, choose 'Phone' and select 'Call'. You can also optionally change the phone number.

[<img src="http://i.stack.imgur.com/3qsrE.png" alt="enter image description here" />](http://i.stack.imgur.com/3qsrE.png)



## Resolving Errors while starting emulator


First of all, ensure that you've enabled the '**Virtualization'** in your BIOS setup.

Start the **Android SDK Manager**, select **Extras** and then select **Intel Hardware Accelerated Execution Manager** and wait until your download completes. If it still doesn't work, open your SDK folder and run `/extras/intel/Hardware_Accelerated_Execution_Manager/IntelHAXM.exe`.

Follow the on-screen instructions to complete installation.

Or for OS X you can do it without onscreen prompts like this: `/extras/intel/Hardware_Accelerated_Execution_Manager/HAXM\ installation`

> 
If your CPU does not support VT-x or SVM, you can not use x86-based Android images. Please use ARM-based images instead.


After installation completed, confirm that the virtualization driver is operating correctly by opening a command prompt window and running the following command: `sc query intelhaxm`

To run an x86-based emulator with VM acceleration:
If you are running the emulator from the command line, just specify an x86-based AVD: `emulator -avd <avd_name>`

If you follow all the steps mentioned above correctly, then surely you should be able to see your AVD with HAXM coming up normally.



#### Remarks


**AVD** stands for **Android Virtual Device**

