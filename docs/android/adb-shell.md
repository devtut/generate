---
metaTitle: "Android - adb shell"
description: "Granting & revoking API 23+ permissions, Send text, key pressed and touch events to Android Device via ADB, List packages, Recording the display, Set Date/Time via adb, Open Developer Options, Print application data, Changing file permissions using chmod command, Generating a Boot Complete broadcast, View external/secondary storage content, kill a process inside an Android device"
---

# adb shell


`adb shell` opens a Linux shell in a target device or emulator.
It is the most powerful and versatile way to control an Android device via `adb`.

This topic was split from [ADB (Android Debug Bridge)](http://stackoverflow.com/documentation/android/1051/adb-android-debug-bridge) due to reaching the limit of examples, many of which were involving `adb shell` command.



## Granting & revoking API 23+ permissions


A one-liner that helps granting or revoking vulnerable permissions.

<li>
**granting**

```java
adb shell pm grant <sample.package.id> android.permission.<PERMISSION_NAME>

```


</li>
<li>
**revoking**

```java
adb shell pm revoke <sample.package.id> android.permission.<PERMISSION_NAME>

```


</li>
<li>
**Granting all run-time permissions at a time on installation (-g)**

```java
adb install -g /path/to/sample_package.apk

```


</li>



## Send text, key pressed and touch events to Android Device via ADB


execute the following command to insert the text into a view with a focus (if it supports text input)

**Send text on SDK 23+**

```java
adb shell "input keyboard text 'Paste text on Android Device'"

```

If already connected to your device via `adb`:

```java
input text 'Paste text on Android Device'

```

**Send text prior to SDK 23**

```java
adb shell "input keyboard text 'Paste%stext%son%sAndroid%sDevice'"

```

Spaces are not accepted as the input, replace them with %s.

**Send events**

To simulate pressing the hardware power key

```java
adb shell input keyevent 26

```

or alternatively

```java
adb shell input keyevent POWER

```

Even if you don't have a hardware key you still can use a `keyevent` to perform the equivalent action

```java
adb shell input keyevent CAMERA

```

**Send touch event as input**

```java
adb shell input tap Xpoint Ypoint

```

**Send swipe event as input**

```java
adb shell input swipe Xpoint1 Ypoint1 Xpoint2 Ypoint2 [DURATION*]

```

*DURATION is optional, default=300ms. [source](http://androidxref.com/7.1.1_r6/xref/frameworks/base/cmds/input/src/com/android/commands/input/Input.java#201)

Get X and Y points by enabling pointer location in developer option.

**ADB sample shell script**

> 
To run a script in Ubuntu, Create script.sh right click the file and add read/write permission and tick **allow executing file as program**.


> 
Open terminal emulator and run the command ./script.sh


Script.sh

```

for (( c=1; c<=5; c++ ))
    do  
       adb shell input tap X Y
       echo "Clicked $c times"
       sleep 5s
    done

```

For a comprehensive list of event numbers

- shortlist of several interesting events [ADB Shell Input Events](http://stackoverflow.com/questions/7789826/adb-shell-input-events)
- reference documentation [https://developer.android.com/reference/android/view/KeyEvent.html#KEYCODE_POWER](https://developer.android.com/reference/android/view/KeyEvent.html#KEYCODE_POWER).



## List packages


Prints all packages, optionally only those whose package name contains the text in <FILTER>.

```java
adb shell pm list packages [options] <FILTER>

All <FILTER>

adb shell pm list packages

```

Attributes:

`-f` to see their associated file.

`-i` See the installer for the packages.

`-u` to also include uninstalled packages.

`-u` Also include uninstalled packages.

Attributes that filter:

`-d` for disabled packages.

`-e` for enabled packages.

`-s` for system packages.

`-3` for third party packages.

`--user <USER_ID>` for a specific user space to query.



## Recording the display


Recording the display of devices running Android 4.4 (API level 19) and higher:

```java
adb shell screenrecord [options] <filename>
adb shell screenrecord /sdcard/demo.mp4

```

(press Ctrl-C to stop recording)

Download the file from the device:

```java
adb pull /sdcard/demo.mp4

```

> 
Note: Stop the screen recording by pressing Ctrl-C, otherwise the recording stops automatically at three minutes or the time limit set by `--time-limit`.


```java
adb shell screenrecord --size <WIDTHxHEIGHT>

```

Sets the video size: 1280x720. The default value is the device's native display resolution (if supported), 1280x720 if not. For best results, use a size supported by your device's Advanced Video Coding (AVC) encoder.

```java
adb shell screenrecord --bit-rate <RATE>

```

Sets the video bit rate for the video, in megabits per second. The default value is 4Mbps. You can increase the bit rate to improve video quality, but doing so results in larger movie files. The following example sets the recording bit rate to 5Mbps:

```java
adb shell screenrecord --bit-rate 5000000 /sdcard/demo.mp4

```

```java
adb shell screenrecord --time-limit <TIME>

```

Sets the maximum recording time, in seconds. The default and maximum value is 180 (3 minutes).

```java
adb shell screenrecord --rotate

```

Rotates the output 90 degrees. This feature is experimental.

```java
adb shell screenrecord --verbose

```

Displays log information on the command-line screen. If you do not set this option, the utility does not display any information while running.

> 
Note: This might not work on some devices.


The screen recording command isn't compatible with android versions pre 4.4

> 
The screenrecord command is a shell utility for recording the display of devices running Android 4.4 (API level 19) and higher. The utility records screen activity to an MPEG-4 file.




## Set Date/Time via adb


Default SET format is `MMDDhhmm[[CC]YY][.ss]`, that's (2 digits each)

For example, to set July 17'th 10:10am, without changing the current year, type:

```java
adb shell 'date 07171010.00'

```

**Tip 1:** the date change will not be reflected immediately, and a noticable change will happen only after the system clock advances to the next minute.<br>
You can force an update by attaching a `TIME_SET` intent broadcast to your call, like that:

```java
adb shell 'date 07171010.00 ; am broadcast -a android.intent.action.TIME_SET'

```

**Tip 2:** to synchronize Android's clock with your local machine:

Linux:

```java
adb shell date `date +%m%d%H%M%G.%S`

```

Windows (PowerShell):

```java
$currentDate = Get-Date -Format "MMddHHmmyyyy.ss" # Android's preferred format
adb shell "date $currentDate"

```

**Both tips together:**

```java
adb shell 'date `date +%m%d%H%M%G.%S` ; am broadcast -a android.intent.action.TIME_SET'

```

Default SET format is 'YYYYMMDD.HHmmss'

```java
adb shell 'date -s 20160117.095930'

```

**Tip:** to synchronize Android's clock with your local (linux based) machine:

```java
adb shell date -s `date +%G%m%d.%H%M%S`

```



## Open Developer Options


```java
adb shell am start -n com.android.settings/.DevelopmentSettings

```

Will navigate your device/emulator to the `Developer Options` section.



## Print application data


This command print all relevant application data:

- version code
- version name
- granted permissions (Android API 23+)
- etc..

```java
adb shell dumpsys package <your.package.id>

```



## Changing file permissions using chmod command


<em>**Notice, that in order to change file prmissions, your device need to be rooted,**
`su` binary doesn't come with factory shipped devices!</em>

Convention:

```java
adb shell su -c "chmod <numeric-permisson> <file>"

```

Numeric permission constructed from user, group and world sections.

For example, if you want to change file to be readable, writable and executable by everyone, this will be your command:

```java
adb shell su -c "chmod 777 <file-path>"

```

Or

```java
adb shell su -c "chmod 000 <file-path>"

```

if you intent to deny any permissions to it.

**1st digit**-specifies user permission, **2nd digit**- specifies group permission, **3rd digit** - specifies world (others) permission.

Access permissions:

```java
--- :   binary value:   000,  octal value: 0 (none)
--x :   binary value:   001,  octal value: 1 (execute)
-w- :   binary value:   010,  octal value: 2 (write)
-wx :   binary value:   011,  octal value: 3 (write, execute)
r-- :   binary value:   100,  octal value: 4 (read)
r-x :   binary value:   101,  octal value: 5 (read, execute)
rw- :   binary value:   110,  octal value: 6 (read, write)
rwx :   binary value:   111,  octal value: 7 (read, write, execute)

```



## Generating a "Boot Complete" broadcast


This is relevant for apps that implement a `BootListener`. Test your app by killing your app and then test with:

```java
adb shell am broadcast -a android.intent.action.BOOT_COMPLETED -c android.intent.category.HOME -n your.app/your.app.BootListener

```

(replace `your.package/your.app.BootListener` with proper values).



## View external/secondary storage content


View content:

```java
adb shell ls \$EXTERNAL_STORAGE
adb shell ls \$SECONDARY_STORAGE

```

View path:

```java
adb shell echo \$EXTERNAL_STORAGE
adb shell echo \$SECONDARY_STORAGE

```



## kill a process inside an Android device


Sometimes Android's logcat is running infinitely with errors coming from some process not own by you, draining battery or just making it hard to debug your code.

A convenient way to fix the problem without restarting the device is to locate and kill the process causing the problem.

From Logcat

`03-10 11:41:40.010 1550-1627/? E/SomeProcess: ....`

notice the process number: 1550

Now we can open a shell and kill the process.
Note that we cannot kill `root` process.

```java
adb shell

```

inside the shell we can check more about the process using

```java
ps -x | grep 1550

```

and kill it if we want:

```java
kill -9 1550

```



#### Syntax


- adb shell [-e escape] [-n] [-Tt] [-x] [command]



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|-e|choose escape character, or "none"; default '~'
|-n|don't read from stdin
|-T|disable PTY allocation
|-t|force PTY allocation
|-x|disable remote exit codes and stdout/stderr separation

