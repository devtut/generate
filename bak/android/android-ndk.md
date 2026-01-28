---
metaTitle: "Android - Android NDK"
description: "How to log in ndk, Building native executables for Android, How to clean the build, How to use a makefile other than Android.mk"
---

# Android NDK




## How to log in ndk


First make sure you link against the logging library in your `Android.mk` file:

```java
LOCAL_LDLIBS := -llog   

```

Then use one of the following `__android_log_print()` calls:

```java
#include <android/log.h>    
#define TAG "MY LOG"

__android_log_print(ANDROID_LOG_VERBOSE,    TAG, "The value of 1 + 1 is %d", 1 + 1)
__android_log_print(ANDROID_LOG_WARN,       TAG, "The value of 1 + 1 is %d", 1 + 1)
__android_log_print(ANDROID_LOG_DEBUG,      TAG, "The value of 1 + 1 is %d", 1 + 1)
__android_log_print(ANDROID_LOG_INFO,       TAG, "The value of 1 + 1 is %d", 1 + 1)
__android_log_print(ANDROID_LOG_ERROR,      TAG, "The value of 1 + 1 is %d", 1 + 1)

```

Or use those in a more convenient way by defining corresponding macros:

```java
#define  LOGV(...)  __android_log_print(ANDROID_LOG_VERBOSE,    TAG, __VA_ARGS__)
#define  LOGW(...)  __android_log_print(ANDROID_LOG_WARN,       TAG, __VA_ARGS__)
#define  LOGD(...)  __android_log_print(ANDROID_LOG_DEBUG,      TAG, __VA_ARGS__)
#define  LOGI(...)  __android_log_print(ANDROID_LOG_INFO,       TAG, __VA_ARGS__)
#define  LOGE(...)  __android_log_print(ANDROID_LOG_ERROR,      TAG, __VA_ARGS__)

```

**Example**:

```java
int x = 42;
LOGD("The value of x is %d", x);

```



## Building native executables for Android


project/jni/main.c

```java
#include <stdio.h>
#include <unistd.h>

int main(void) {
  printf("Hello world!\n");
  return 0;
}

```

project/jni/Android.mk

```java
LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE    := hello_world
LOCAL_SRC_FILES := main.c
include $(BUILD_EXECUTABLE)

```

project/jni/Application.mk

```java
APP_ABI := all
APP_PLATFORM := android-21

```

If you want to support devices running Android versions lower than 5.0 (API 21), you need to compile your binary with `APP_PLATFORM` set to an older API, e.g. `android-8`. This is a consequence of Android 5.0 enforcing **Position Independent Binaries** (PIE), whereas older devices do not necessarily support PIEs. Therefore, you need to use either the PIE or the non-PIE, depending on the device version. If you want to use the binary from within your Android application, you need to check the API level and extract the correct binary.

`APP_ABI` can be changed to specific platforms such as `armeabi` to build the binary for those architectures only.

In the worst case, you will have both a PIE and a non-PIE binary for each architecture (about 14 different binaries using ndk-r10e).

To build the executable:

```java
cd project
ndk-build

```

You will find the binaries at `project/libs/<architecture>/hello_world`. You can use them via `ADB` (`push` and `chmod` it with executable permission) or from your application (extract and `chmod` it with executable permission).

To determine the architecture of the CPU, retrieve the build property `ro.product.cpu.abi` for the primary architecture or `ro.product.cpu.abilist` (on newer devices) for a complete list of supported architectures. You can do this using the `android.os.Build` class from within your application or using `getprop <name>` via ADB.



## How to clean the build


If you need to clean the build:

```java
ndk-build clean

```



## How to use a makefile other than Android.mk


ndk-build NDK_PROJECT_PATH=PROJECT_PATH APP_BUILD_SCRIPT=MyAndroid.mk

