---
metaTitle: "Android - Installing apps with ADB"
description: "Uninstall an app, Install all apk file in directory, Install an app"
---

# Installing apps with ADB



## Uninstall an app


Write the following command in your terminal to uninstall an app with a provided package name:

```java
adb uninstall <packagename>

```



## Install all apk file in directory


Windows :

```java
for %f in (C:\your_app_path\*.apk) do adb install "%f"

```

Linux :

```java
for f in *.apk ; do adb install "$f" ; done

```



## Install an app


Write the following command in your terminal:

```java
adb install [-rtsdg] <file>

```

Note that you have to pass a file that is on your computer and not on your device.

If you append `-r` at the end, then any existing conflicting apks will be overwritten. Otherwise, the command will quit with an error.

`-g` will immediately grant all runtime permissions.

`-d` allows version code downgrade (only appliable on debuggable packages).

Use `-s` to install the application on the external SD card.

`-t` will allow to use test applications.

